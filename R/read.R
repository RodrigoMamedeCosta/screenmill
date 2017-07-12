#' Read screenmill files in a directory
#'
#' @param dir Directory containing screenmill files
#'
#' @importFrom readr read_csv col_character col_integer col_logical col_double cols cols_only
#' @export

read_screenmill <- function(dir) {

  assert_that(
    is.dir(dir),
    file.exists(file.path(dir, 'screenmill-collection-keys.csv')),
    file.exists(file.path(dir, 'screenmill-measurements.csv')),
    file.exists(file.path(dir, 'screenmill-annotations.csv')),
    file.exists(file.path(dir, 'screenmill-calibration-grid.csv'))
  )

  key <- read_csv(
    file.path(dir, 'screenmill-collection-keys.csv'),
    col_types = cols( # extra columns allowed
      strain_collection_id = col_character(),
      strain_id            = col_character(),
      strain_name          = col_character(),
      plate                = col_integer(),
      row                  = col_integer(),
      column               = col_integer(),
      plate_control        = col_logical()
    )
  )

  msmt <- read_csv(
    file.path(dir, 'screenmill-measurements.csv'),
    col_types = cols_only(
      strain_collection_id = col_character(),
      plate_id   = col_character(),
      plate      = col_integer(),
      row        = col_integer(),
      column     = col_integer(),
      replicate  = col_double(),
      colony_row = col_integer(),
      colony_col = col_integer(),
      colony_num = col_integer(),
      size       = col_double()
    )
  )

  anno <- read_csv(
    file.path(dir, 'screenmill-annotations.csv'),
    col_types = cols_only(
      date         = col_character(),
      strain_collection_id = col_character(),
      query_id     = col_character(),
      treatment_id = col_character(),
      media_id     = col_character(),
      temperature  = col_double(),
      template     = col_character(),
      plate_id     = col_character(),
      group        = col_integer(),
      position     = col_integer(),
      hours_growth = col_double(),
      timepoint    = col_integer()
    )
  )

  grid <- read_csv(
    file.path(dir, 'screenmill-calibration-grid.csv'),
    col_types = cols_only(
      template  = col_character(),
      position  = col_integer(),
      group     = col_integer(),
      plate     = col_integer(),
      row       = col_integer(),
      column    = col_integer(),
      replicate = col_integer(),
      excluded  = col_logical()
    )
  )

  quer <- read_csv(
    file.path(dir, 'screenmill-queries.csv'),
    col_types = cols_only(
      query_id         = col_character(),
      query_name       = col_character(),
      control_query_id = col_character()
    )
  )

  trea <- read_csv(
    file.path(dir, 'screenmill-treatments.csv'),
    col_types = cols_only(
      treatment_id         = col_character(),
      treatment_name       = col_character(),
      control_treatment_id = col_character()
    )
  )

  key %>%
    left_join(msmt, by = c('strain_collection_id', 'plate', 'row', 'column')) %>%
    left_join(anno, by = c('strain_collection_id', 'plate_id')) %>%
    left_join(grid, by = c('plate', 'row', 'column', 'replicate', 'group', 'position', 'template')) %>%
    left_join(quer, by = 'query_id') %>%
    left_join(trea, by = 'treatment_id') %>%
    filter(!excluded) %>%
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

read_annotation <- function(path) {
  read_csv(
    path,
    col_types = cols( # additional columns are retained and type will be guessed
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
      email                = readr::col_character(),
      .default             = readr::col_guess()
    )
  )
}

read_key <- function(path) {
  read_csv(
    path,
    col_types = cols( # additional columns are retained and type will be guessed
      strain_collection_id = readr::col_character(),
      strain_id            = readr::col_character(),
      strain_name          = readr::col_character(),
      plate                = readr::col_integer(),
      row                  = readr::col_integer(),
      column               = readr::col_integer(),
      plate_control        = readr::col_logical(),
      .default             = readr::col_guess()
    )
  )
}

read_crop <- function(path) {
  read_csv(
    path,
    col_types = cols( # additional columns are retained and type will be guessed
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
      invert    = readr::col_logical(),
      .default  = readr::col_guess()
    )
  )
}

read_grid <- function(path) {
  read_csv(
    path,
    col_types = cols( # additional columns are retained and type will be guessed
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
      .default             = readr::col_guess()
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
#'
#' These tables are recommended (depending on the experiment):
#'
#' - *plasmid_id* - A plasmid identifier
#' - *OD600* - The culture density used for pinning.
#' - *pinnings* - The number of pinnings from source plate to target plate.
#' - *excluded* - positions to exclude from analysis.
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
  list.files(dir) %>%
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
      strain_collection_id, strain_id, strain_name, plate, row, column, plate_control, everything()
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
    mutate_(id = ~rep(plates$id, length.out = nobs, each = density)) %>%
    left_join(plates, by = 'id') %>%
    mutate_(
      row       = ~map_row(librows, replicates, nobs),
      column    = ~map_column(libcols, replicates, nrows, ncols, nobs),
      replicate = ~map_replicate(librows, libcols, replicates, nobs)
    ) %>%
    select_(~id, ~scan_name, ~scan_cond, ~plate, ~row, ~column, ~replicate, ~size, ~circ)
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
    select_(
      scan_name = ~Query,
      scan_cond = ~Condition,
      plate     = ~`Plate #`,
      row       = ~Row,
      column    = ~Column,
      ~contains('Colony Size')
    ) %>%
    # Gather colony size into single column
    gather('colony_dr', 'size_dr', contains('Colony Size')) %>%
    # Fix columns
    mutate_(
      scan_name = ~as.character(scan_name),
      scan_cond = ~ifelse(is.na(scan_cond) | scan_cond == '', 'none', scan_cond),
      plate     = ~as.integer(gsub('[][]', '', plate)), # remove brackets
      column    = ~as.integer(column),
      colony_dr = ~as.integer(gsub('[^0-9]*', '', colony_dr)), # remove text
      replicate = ~as.integer(names(replicate_map[colony_dr])),
      excluded_query   = ~grepl('\\*', size_dr),
      excluded_control = ~grepl('\\^', size_dr),
      size_dr   = ~as.numeric(gsub('\\*|\\^', '', size_dr)) # remove *^ flags
    ) %>%
    # Sort the data
    arrange_(~scan_name, ~scan_cond, ~plate, ~row, ~column, ~replicate) %>%
    select_(~scan_name, ~scan_cond, ~plate, ~row, ~column, ~replicate,
            ~size_dr, ~excluded_query, ~excluded_control) %>%
    na.omit
}


