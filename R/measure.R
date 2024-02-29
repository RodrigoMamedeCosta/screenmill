log_message <- function(message, log_file = "logs/measure_log.txt") {
  # Check if ErrorLogs folder exists, create if not
  if (!file.exists("logs")) {
    dir.create("logs")
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste(timestamp, message, "\n"), file = log_file, append = TRUE)
}

measure <- function(dir = '.', overwrite = F, save.plates = F, save.colonies = T) {
  log_message("Starting measure function")
  
  status <- screenmill_status(dir)
  log_message("screenmill_status called")
  
  assert_that(is.flag(overwrite), is.flag(save.plates), is.flag(save.colonies))
  log_message("Assertions checked")
  
  if (!status$flag$calibrated) {
    log_message("Calibration files not found, stopping")
    stop('Could not find calibration files. Please annotate and calibrate before measuring.\nSee ?annotate and ?calibrate for more details.')
  }
  
  if (!overwrite && status$flag$measured) {
    log_message("Data already measured and overwrite not requested, exiting")
    message('This batch has already been measured. Set "overwrite = TRUE" to re-measure.')
    return(invisible(status$dir))
  } else {
    log_message("Removing pre-existing files if any")
    suppressWarnings(file.remove(status$path$measurements))
  }
  
  log_message("Reading annotations")
  annot <- read_annotations(status$dir) %>% mutate(path = file.path(dir, file, fsep = '/')) %>%
    select(path, file, plate_id, template, position)
  
  log_message("Processing paths and plates")
  paths <- unique(annot$path)
  plates <- left_join(annot, read_calibration_crop(dir), by = c('template', 'position')) %>%
    select(path, plate_id, starts_with('rough'), rotate, starts_with('fine'), invert)
  
  log_message("Processing grids")
  grids <- left_join(annot, read_calibration_grid(dir), by = c('template', 'position')) %>%
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
  
  log_message("Starting measurements")
  time <- Sys.time()
  message('Measuring ', length(paths), ' images')
  progress <- progress_bar$new(total = length(paths))
  cores <- ifelse(.Platform$OS.type == 'windows', 1, max(1, detectCores() - 1, na.rm = T))
  
  results <- lapply(paths, function(pth) {
    progress$tick()
    log_message(paste("Processing path:", pth))
    img <- read_greyscale(pth)
    coords <- filter(plates, path == pth)
    plate_ids <- unique(coords$plate_id)
    
    measurements <- mclapply(plate_ids, function(p) {
      log_message(paste("Processing plate ID:", p))
      #Log to identify last plate measured before completion or a crash.
      cat(p, file = "logs/last_plate_id.txt")
     
      # Crop plates
      crop <- filter(plates, plate_id == p)
      log_message(paste("Cropping plates for plate ID:", p))
      rough <- with(crop, img[rough_l:rough_r, rough_t:rough_b])
      rotated <- EBImage::rotate(rough, crop$rotate)
      fine <- with(crop, rotated[fine_l:fine_r, fine_t:fine_b])
      if (crop$invert) fine <- 1 - fine
      log_message(paste("Cropping and rotation completed for plate ID:", p))
      
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
        log_message(paste("Saved cropped plate for plate ID:", p))
      }
      
      # ---- Measure colonies ----
      grid <- filter(grids, plate_id == p)
      log_message(paste("Measuring colonies for plate ID:", p))
      result <- with(grid, measureColonies(fine, l, r, t, b, background))
      grid$size <- result$measurements
      log_message(paste("Colony measurement completed for plate ID:", p))
      
      # Save colonies in desired format
      if (save.colonies) {
        target <- paste0(dir, '/colonies/')
        if (!dir.exists(target)) dir.create(target)
        saveRDS(result$colonies, paste0(target, p, '.rds'))
        log_message(paste("Saved colony data for plate ID:", p))
      }
      
      return(grid %>%
               select(
                 plate_id, strain_collection_id, plate, row, column, replicate,
                 colony_row, colony_col, colony_num, size
               ))
    }, mc.cores = cores) %>% bind_rows
    
    write_csv(measurements, status$path$measurements, append = file.exists(status$path$measurements))
    log_message(paste("Measurements written to file for path:", pth))
  })
  
  message('Finished measuring in ', format(round(Sys.time() - time, 2)))
  log_message("Measurement process completed")
  return(invisible(status$dir))
}

measure_addin <- function() {
  log_message("measure_addin function started")
  message('Choose a file in the directory of images you wish to process.')
  dir <- dirname(file.choose())
  log_message(paste("Directory chosen:", dir))
  measure(dir, overwrite = TRUE)
  log_message("measure_addin function completed")
}

background <- function(colony) {
  colony <- as.vector(colony)
  centers <- kmeans(colony, centers = 2)$centers
  bg <- colony[which(colony < mean(centers))]
  result <- mean(bg)
  
  # Check if result is NaN or below the threshold
  threshold <- 0.00001
  if (is.nan(result) || result < threshold) {
    log_message(paste("background result below threshold or NaN:", result, 
                      "- Adjusted to:", threshold))
    result <- threshold
  } else {
    log_message(paste("background calculation result:", result))
  }
  
  return(result)
}
