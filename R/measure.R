#' Measure Colonies
#'
#' Measure and save colonies.
#'
#' @param dir Path to directory to process.
#' @param overwrite Should previous measurements be overwritten?
#' @param save.plates Should cropped/rotated plates be saved? Defaults to \code{FALSE}.
#' @param save.colonies Should cropped colonies be saved? Defaults to \code{TRUE}.
#'
#' @importFrom readr read_csv write_csv
#' @importFrom parallel mclapply detectCores
#' @export

measure <- function(dir = '.', overwrite = F, save.plates = F, save.colonies = T) {

  status <- screenmill_status(dir)
  assert_that(is.flag(overwrite), is.flag(save.plates), is.flag(save.colonies))

  # Stop if plates have not yet been annotated
  if (!status$flag$calibrated) stop('Could not find calibration files. Please annotate and calibrate before measuring.\nSee ?annotate and ?calibrate for more details.')

  if (!overwrite && status$flag$measured) {
    # Exit if already calibratd and no overwrite
    message('This batch has already been measured Set "overwrite = TRUE" to re-measure.')
    return(invisible(status$dir))
  } else {
    # Remove pre-existing files
    suppressWarnings(file.remove(status$path$measurements))
  }

  # Read metadata
  annot <-
    read_annotations(status$dir) %>% mutate(path = file.path(dir, file, fsep = '/')) %>%
    select(path, file, plate_id, template, position)
  paths <- unique(annot$path)
  plates <-
    left_join(annot, read_calibration_crop(dir), by = c('template', 'position')) %>%
    select(path, plate_id, starts_with('rough'), rotate, starts_with('fine'), invert)
  grids  <-
    left_join(annot, read_calibration_grid(dir), by = c('template', 'position')) %>%
    group_by(plate_id) %>%
    arrange(row, column, replicate) %>%
    mutate(colony_num = 1:n()) %>%
    arrange(plate_id, colony_num) %>%
    ungroup %>%
    select(
      plate_id,
      strain_collection_id, plate, row, column, replicate,
      colony_row, colony_col, colony_num, l, r, t, b
    ) %>%
    filter(complete.cases(.)) %>%
    ungroup

  # Record start time
  time <- Sys.time()

  # For each image
  message('Measuring ', length(paths), ' images')
  #progress <- progress_estimated(length(paths))
  progress <- progress_bar$new(total = length(paths))
  cores <- ifelse(.Platform$OS.type == 'windows', 1, max(1, detectCores() - 1, na.rm = T))
  lapply(paths, function(pth) {

    progress$tick()
    img <- read_greyscale(pth)
    coords <- filter(plates, path == pth)
    plate_ids <- unique(coords$plate_id)

    # For each plate within this image
    measurements <-
      mclapply(plate_ids, function(p) {

        # Crop plates
        crop    <- filter(plates, plate_id == p)
        rough   <- with(crop, img[ rough_l:rough_r, rough_t:rough_b ])
        rotated <- EBImage::rotate(rough, crop$rotate)
        fine    <- with(crop, rotated[ fine_l:fine_r, fine_t:fine_b ])
        if (crop$invert) fine <- 1 - fine

        # Save cropped plate in desired format
        if (save.plates) {
          target <- paste0(dir, '/plates/')
          if (!dir.exists(target)) dir.create(target)
          EBImage::writeImage(
            fine,
            paste0(target, p, '.tif'),
            type = 'tiff',
            compression = 'none',
            bits.per.sample = 8L
          )
        }

        # ---- Measure colonies ----
        grid <- filter(grids, plate_id == p)

        result    <- with(grid, measureColonies(fine, l, r, t, b, background))
        grid$size <- result$measurements

        # Save colonies in desired format
        if (save.colonies) {
          target <- paste0(dir, '/colonies/')
          if (!dir.exists(target)) dir.create(target)
          saveRDS(result$colonies, paste0(target, p, '.rds'))
        }

        grid %>%
          select(
            plate_id, strain_collection_id, plate, row, column, replicate,
            colony_row, colony_col, colony_num, size
          )
      }, mc.cores = cores) %>%
      bind_rows

    write_csv(measurements, status$path$measurements, append = file.exists(status$path$measurements))
  })

  message('Finished measuring in ', format(round(Sys.time() - time, 2)))
  return(invisible(status$dir))
}

measure_addin <- function() {
  message('Choose a file in the directory of images you wish to process.')
  dir <- dirname(file.choose())
  measure(dir, overwrite = TRUE)
}

background <- function(colony) {
  colony  <- as.vector(colony)
  centers <- kmeans(colony, centers = 2)$centers
  bg      <- colony[which(colony < mean(centers))]
  result  <- mean(bg)
  if (is.nan(result)) result <- mean(centers)
  return(result)
}
