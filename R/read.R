#' Read screenmill files in a directory
#'
#' @param dir Directory containing screenmill files.
#' @param file File name with extension (not typically necessary as defaults are provided).
#'
#' @name read_screenmill
#' @rdname read_screenmill
#' @importFrom readr read_csv col_character col_integer col_logical col_double cols cols_only
#' @export

read_screenmill <- function(dir) {

  assert_that(assertthat::is.dir(dir), assertthat::is.readable(dir))

  read_collection_keys(dir) %>%
    select(-excluded) %>%
    left_join(
      read_annotations(dir),
      by = c('strain_collection_id', 'plate')
    ) %>%
    left_join(
      read_calibration_grid(dir),
      by = c('strain_collection_id', 'plate', 'row', 'column', 'group', 'position', 'template')
    ) %>%
    left_join(
      read_measurements(dir),
      by = c('strain_collection_id', 'plate', 'row', 'column', 'plate_id', 'replicate', 'colony_row', 'colony_col')
    ) %>%
    left_join(
      read_queries(dir),
      by = 'query_id'
    ) %>%
    left_join(
      read_treatments(dir),
      by = 'treatment_id'
    ) %>%
    filter(!excluded & !excluded_key) %>%
    select(
      plate_id, hours_growth, size,
      strain_name, query_name, treatment_name,
      strain_id, query_id, treatment_id, media_id, temperature,
      control_query_id, control_treatment_id, plate_control,
      date, group, position, timepoint,
      strain_collection_id, plate, row, column, replicate,
      colony_row, colony_col, colony_num,
      everything()
    )
}

#' @rdname read_screenmill
#' @export

read_annotations <- function(dir, file = 'screenmill-annotations.csv') {
  anno <- read_csv(
    file.path(dir, file),
    col_types = cols_only( # strict columns
      plate_id             = readr::col_character(),
      date                 = readr::col_date(),
      group                = readr::col_integer(),
      position             = readr::col_integer(),
      timepoint            = readr::col_integer(),
      hours_growth         = readr::col_double(),
      file                 = readr::col_character(),
      template             = readr::col_character(),
      strain_collection_id = readr::col_character(),
      plate                = readr::col_integer(),
      query_id             = readr::col_character(),
      treatment_id         = readr::col_character(),
      media_id             = readr::col_character(),
      temperature          = readr::col_double(),
      time_series          = readr::col_character(),
      end                  = readr::col_datetime(),
      start                = readr::col_datetime(),
      owner                = readr::col_character(),
      email                = readr::col_character()
    )
  )

  validate_annotations(anno)
  return(anno)
}

#' @rdname read_screenmill
#' @export

read_collection_keys <- function(dir, file = 'screenmill-collection-keys.csv') {
  read_csv(
    file.path(dir, file),
    col_types = cols( # additional columns are retained and type will be guessed
      strain_collection_id = readr::col_character(),
      strain_id            = readr::col_character(),
      strain_name          = readr::col_character(),
      plate                = readr::col_integer(),
      row                  = readr::col_integer(),
      column               = readr::col_integer(),
      plate_control        = readr::col_logical(),
      excluded             = readr::col_logical(),
      .default             = readr::col_guess()
    )
  )
}

#' @rdname read_screenmill
#' @export

read_collections <- function(dir, file = 'screenmill-collections.csv') {
  read_csv(
    file.path(dir, file),
    col_types = cols_only( # strict columns
      strain_collection_id = readr::col_character(),
      description          = readr::col_character()
    )
  )
}

#' @rdname read_screenmill
#' @export

read_calibration_crop <- function(dir, file = 'screenmill-calibration-crop.csv') {
  read_csv(
    file.path(dir, file),
    col_types = cols_only( # strict columns
      template  = readr::col_character(),
      position  = readr::col_integer(),
      plate_row = readr::col_integer(),
      plate_col = readr::col_integer(),
      plate_x   = readr::col_integer(),
      plate_y   = readr::col_integer(),
      rough_l   = readr::col_integer(),
      rough_r   = readr::col_integer(),
      rough_t   = readr::col_integer(),
      rough_b   = readr::col_integer(),
      rotate    = readr::col_double(),
      fine_l    = readr::col_integer(),
      fine_r    = readr::col_integer(),
      fine_t    = readr::col_integer(),
      fine_b    = readr::col_integer(),
      invert    = readr::col_logical()
    )
  )
}

#' @rdname read_screenmill
#' @export

read_calibration_grid <- function(dir, file = 'screenmill-calibration-grid.csv') {

  grid <-
    read_csv(
    file.path(dir, file),
    col_types = cols_only( # strict columns
      template             = readr::col_character(),
      position             = readr::col_integer(),
      group                = readr::col_integer(),
      strain_collection_id = readr::col_character(),
      plate                = readr::col_integer(),
      row                  = readr::col_integer(),
      column               = readr::col_integer(),
      replicate            = readr::col_integer(),
      colony_row           = readr::col_integer(),
      colony_col           = readr::col_integer(),
      x                    = readr::col_integer(),
      y                    = readr::col_integer(),
      l                    = readr::col_integer(),
      r                    = readr::col_integer(),
      t                    = readr::col_integer(),
      b                    = readr::col_integer(),
      excluded             = readr::col_logical()
    )
  )

  # Key exclusions will always propagate to the grid calibration file.
  keys <-
    read_collection_keys(dir) %>%
    select(strain_collection_id, plate, row, column, excluded_key = excluded)

  left_join(
    grid,
    keys,
    by = c('strain_collection_id', 'plate', 'row', 'column')
  ) %>%
  mutate(excluded = excluded_key | excluded)
}

#' @rdname read_screenmill
#' @export

read_measurements <- function(dir, file = 'screenmill-measurements.csv') {
  read_csv(
    file.path(dir, file),
    col_types = cols_only( # strict columns
      plate_id             = readr::col_character(),
      strain_collection_id = readr::col_character(),
      plate                = readr::col_integer(),
      row                  = readr::col_integer(),
      column               = readr::col_integer(),
      replicate            = readr::col_integer(),
      colony_row           = readr::col_integer(),
      colony_col           = readr::col_integer(),
      colony_num           = readr::col_integer(),
      size                 = readr::col_double()
    )
  )
}

#' @rdname read_screenmill
#' @export

read_queries <- function(dir, file = 'screenmill-queries.csv') {
  read_csv(
    file.path(dir, file),
    col_types = cols_only( # strict columns
      query_id = readr::col_character(),
      query_name = readr::col_character(),
      query_annotation = readr::col_character(),
      control_query_id = readr::col_character()
    )
  )
}

#' @rdname read_screenmill
#' @export

read_treatments <- function(dir, file = 'screenmill-treatments.csv') {
  read_csv(
    file.path(dir, file),
    col_types = cols_only( # strict columns
      treatment_id          = readr::col_character(),
      treatment_name        = readr::col_character(),
      control_treatment_id  = readr::col_character(),
      treatment_description = readr::col_character()
    )
  )
}

#' @rdname read_screenmill
#' @export

read_media <- function(dir, file = 'screenmill-media.csv') {
  read_csv(
    file.path(dir, file),
    col_types = cols_only( # strict columns
      media_id          = readr::col_character(),
      media_name        = readr::col_character(),
      media_description = readr::col_character()
    )
  )
}

# Read plate layout -------------------------------------------------------------------------------
#' @md
#' @title Read plate layout
#'
#' Reads a directory of plate layout CSVs exported from Numbers (for mac) into a dataframe.
#'
#' @param dir A directory.
#'
#' @details To generate a plate layout directory with Numbers, make a separate sheet for each plate
#' and name each sheet "plate`<number>`" (Replace `<number>` with the plate number). Then create one
#' table for each annotation with columns named "row", "1", "2", "3" etc. for the dimension of the
#' plate. In the first "row" column, number each row "1", "2", "3" etc. for the dimension of the
#' plate. Name each tabel with the intended annotation (e.g. "strain_id" for a table annotating
#' strain IDs for each position in the plate). Once you have annotated your plates, you can export
#' them to CSV (`File>Export To>CSV...`). Name the resulting directory with the desired strain
#' collection ID (some combination of the date and a descriptive word is recommended). The files
#' in this directory will be named `<sheet>-<table>.csv` (e.g. `plate1-strain_id.csv`).
#'
#' These tables are required for every plate:
#'
#' - **strain_id** - The strain identifier.
#' - **strain_name** - The human-readable name of the strain.
#' - **plate_control** - (T/F) is position intended to be used as a control for plate normalization?
#' - *excluded* - positions to exclude from analysis.
#'
#' These tables are recommended (depending on the experiment):
#'
#' - *plasmid_id* - A plasmid identifier
#' - *OD600* - The culture density used for pinning.
#' - *pinnings* - The number of pinnings from source plate to target plate.
#' - *start_temp* - The starting temperature at the beginning of the experiment.
#' - *end_temp* - The ending temperature at the end of the experiment.
#' - *start_humidity* - The starting humidity at the beginning of the experiment.
#' - *end_humidity* - The ending humidity at the end of the experiment.
#'
#' @importFrom stringr str_extract
#' @importFrom purrr map reduce map_df
#' @export

read_plate_layout <- function(dir) {
  assert_that(is.dir(dir))
  list.files(dir, pattern = '^plate') %>% # only read files starting with plate
    # annotation based on file name as name of table in sheet (between "-" and ".csv")
    str_extract('(?<=-).*(?=([.]csv$))') %>%
    unique() %>%
    map(function(annotation) {
      list.files(dir, pattern = annotation, full.names = T) %>%
        map_df(plate_gather, annotation)  # see utils.R
    }) %>%
    reduce(left_join, by = c('row', 'column', 'plate')) %>%
    mutate(strain_collection_id = basename(dir)) %>%
    select(
      strain_collection_id, strain_id, strain_name, plate, row, column, plate_control, excluded, everything()
    )
}


# Read CM engine log file -------------------------------------------------------------------------
#' Read CM engine log file.
#'
#' Reads ScreenMill Colony Measurement engine log file.
#'
#' @param path Path to a CM engine log file.
#' @param replicates Integer. The number of replicates for each strain.
#' @param dim Numeric vector of length 2. The aspect ratio of the colonies on a
#' plate. Defaults to 2 rows for every 3 columns.
#'
#' @return A dataframe with columns:
#' \itemize{
#'   \item{\bold{scan_name:}} The name of the scanned plate.
#'   \item{\bold{scan_cond:}} The condition of the scanned plate.
#'   \item{\bold{plate:}} The strain library plate number.
#'   \item{\bold{row:}} The strain library row.
#'   \item{\bold{column:}} The strain library column.
#'   \item{\bold{replicate:}} The strain replicate number.
#'   \item{\bold{size:}} The size of the colony (unadjusted).
#'   \item{\bold{circ:}} The circularity of the colony.
#' }
#'
#' @export

read_cm <- function(path, replicates, dim = c(2, 3)) {

  lines <- readLines(path)

  # parse and clean plate attributes
  plates <- grep('[A-Za-z]', lines, value = TRUE) %>% parse_names(by = ',')

  # drop plate names and parse measurements
  cm <- lines[-grep('[A-Za-z]', lines)] %>% parse_measurements(by = '\t')

  # Detect density and dimensions based on dim argument
  nobs    <- nrow(cm)
  density <- nobs / nrow(plates)
  nrows   <- dim[1] * sqrt(density / (dim[1] * dim[2]))
  ncols   <- dim[2] * sqrt(density / (dim[1] * dim[2]))
  libdens <- density / replicates
  librows <- dim[1] * sqrt(libdens / (dim[1] * dim[2]))
  libcols <- dim[2] * sqrt(libdens / (dim[1] * dim[2]))

  test <- cm %>%
    mutate(id = rep(plates$id, length.out = nobs, each = density)) %>%
    left_join(plates, by = 'id') %>%
    mutate(
      row       = map_row(librows, replicates, nobs),
      column    = map_column(libcols, replicates, nrows, ncols, nobs),
      replicate = map_replicate(librows, libcols, replicates, nobs)
    ) %>%
    select(
      'id', 'scan_name', 'scan_cond', 'plate', 'row', 'column', 'replicate',
      'size', 'circ'
    )
}

#--------------------------------------------------------------------------------------------------
#' Read DR stats file.
#'
#' Reads ScreenMill Data Review tables.
#'
#' @param path Path to a DR stats file. Defaults to interactive file selection.
#' @param match regex used to find the line containing the table header.
#'
#' @note For 16 colony replicates, data review engine numbers colonies in an
#' inconsistent way (rowwise for the upper-left quadrant and columnwise for
#' remaining colonies). `read_dr` translates these colony numbers to count
#' rowwise for a 4x4 matrix to match the rowwise counting for 4 colony
#' replicates.
#'
#' @return A dataframe with columns:
#' \itemize{
#'   \item{\bold{scan_name:}} The name of the scanned plate.
#'   \item{\bold{scan_cond:}} The condition of the scanned plate.
#'   \item{\bold{plate:}} The strain library plate number.
#'   \item{\bold{row:}} The strain library row.
#'   \item{\bold{column:}} The strain library column.
#'   \item{\bold{replicate:}} The strain replicate number.
#'   \item{\bold{size_dr:}} Size returned by DR engine.
#'   \item{\bold{excluded_query:}} Whether this observation was excluded in
#'   review.
#'   \item{\bold{excluded_control:}} Whether this observation's control was
#'   excluded in review.
#' }
#'
#' @importFrom tidyr gather
#' @importFrom rlang !!! .data
#' @export

read_dr <- function(path, match = 'Query\tCondition\tPlate #\tRow\tColumn') {

  # Somtimes colnames are missing so they have to be filled in after read
  header <- find_header(path, match, delim = '\t')
  if (any(grepl('ID Column', header$header))) {
    colnames <- header$header[1:grep('ID Column', header$header)]
  } else {
    colnames <- header$header
  }
  replicates <- length(grep('Colony Size', colnames))
  if (replicates == 16) {
    # 16 replicates get mapped strangely (Data review engine weirdness)
    replicate_map <- 1:16
    names(replicate_map) <- c(1,2,5,6,9,10,13,14,3,4,7,8,11,12,15,16)
  } else {
    replicate_map <- 1:replicates
    names(replicate_map) <- 1:replicates
  }

  dr <- (readLines(path)[-(1:header$line)]) %>%
    # Split lines by tab
    strsplit('\t') %>%
    # Keep only leading columns (there are often issues with last few cols)
    lapply(function(line, ncol = length(colnames)) line[1:ncol]) %>%
    # Build data frame
    do.call(rbind, .) %>%
    assign_names(colnames) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    # Select and rename important columns
    select(
      scan_name = 'Query',
      scan_cond = 'Condition',
      plate     = 'Plate #',
      row       = 'Row',
      column    = 'Column',
      tidyselect::contains('Colony Size')
    ) %>%
    # Gather colony size into single column
    gather('colony_dr', 'size_dr', tidyselect::contains('Colony Size')) %>%
    # Fix columns
    mutate(
      scan_name = as.character(.data$scan_name),
      scan_cond = ifelse(is.na(.data$scan_cond) | .data$scan_cond == '', 'none', .data$scan_cond),
      plate     = as.integer(gsub('[][]', '', .data$plate)), # remove brackets
      column    = as.integer(.data$column),
      colony_dr = as.integer(gsub('[^0-9]*', '', .data$colony_dr)), # remove text
      replicate = as.integer(names(replicate_map[.data$colony_dr])),
      excluded_query   = grepl('\\*', .data$size_dr),
      excluded_control = grepl('\\^', .data$size_dr),
      size_dr   = as.numeric(gsub('\\*|\\^', '', .data$size_dr)) # remove *^ flags
    ) %>%
    # Sort the data
    arrange(!!!rlang::syms(
      c('scan_name', 'scan_cond', 'plate', 'row', 'column', 'replicate')
    )) %>%
    select(
      'scan_name', 'scan_cond', 'plate', 'row', 'column', 'replicate',
      'size_dr', 'excluded_query', 'excluded_control'
    ) %>%
    na.omit()
}


# ---- Validators ----

validate_annotations <- function(x) {
  counts <- x %>%
    select(file, end, group) %>%
    distinct() %>%
    count(file) %>%
    filter(n > 1)

  if (nrow(counts) > 0) {
    print(df[which(df$file %in% counts$file), ])
    stop(
      paste(
        'Files do not uniquely map to end time and group number. Please',
        'fix these issues in the previously saved annotation data located at:\n',
        path
      )
    )
  }
}
