#
# ************************************************************
#
#  Function to collect strain annotation data from CSV files
#
#  Original code from Eric Edward Bryant modified by RJDR
#
# ------------------------------------------------------------
#
#                   create_key_from_csv_tables
#
# reads a collection of CSV files in `path` labeled as 'plate%num%-%annotation%'.
# For example 'plate1-strain_id'. Required annotations for a screenmill strain
# collection are:
#
# - strain_collection_id
# - strain_id
# - strain_name
# - plate_control
# - plate
# - row
# - column
#
# `strain_collection_id` is passed as a function parameter. `plate`, `row`, and
# `column` are derived from the CSV files (see below). Thus the `path` folder
# needs to contain CSV files specifying at least `strain_id`, `strain_name` and
# `plate_control` for each plate that makes up a custom collection. Additional
# arbitrary annotations to describe a strain library are captured from any
# additional CSV files.
#
# CSV files: for a custom collection of strains, it is often easier to document
# annotation data in a format that resembles the physical format. For instance, a
# 96 well plate might be documented as a 8 X 12 grid in a spread sheet program.
#
#
#
#' Gather annotation information in row by column format and convert into long format.
#'
#' Given a table with columns "row", "1", "2", ... gather everything but the "row"
#' column into a table with variables "row", "column", `value`, "plate".
#'
#' @param path Path to CSV to gather
#' @param value Name of value variable
#' @param plate_pattern Regex used to extract plate number from `path`
#'
#' @details It is easiest to annotate e.g. a 96 well plate with strain, plasmid,
#' OD600 etc. information in a way that looks visually similar to the plate.
#' However this results in multiple tables (one for each annotated variable) in
#' wide format. This function will convert such tables into long format such
#' that each variable is in a single column rather than spread out in the grid
#' style plate layout. Multiple gathered annotations can easily be joined to
#' a single table
#'
#' @param dir directory containing strain annotation csv files
#' @param strain_collection_id string containing collection id
#' @importFrom readr read_csv cols
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @aliases create_key_from_numbers_export
#' @export

create_key_from_csv_tables <- function(dir, strain_collection_id) {
  # Get all annotation names from exported CSV file names
  list.files(dir, pattern = "^plate") %>% # limit capture of annotation csv to files starting with 'plate'
    # annotation is name of table in sheet (between "-" and ".csv")
    # filtering by plate removes a common error that crops up when extra csv files are produced
    # may need to refactor to make this more readable
    str_extract('(?<=-).*(?=([.]csv$))') %>%
    unique() %>%
    # For each annotation type, read and combine into data frames
    map(function(annotation) {
      list.files(dir, pattern = paste0("^plate\\d+\\-",annotation), full.names = T) %>% # needs to be done here too
        map_df(plate_gather, annotation)
    }) %>%
    # Reduce list of data frames through successive joins
    reduce(left_join) %>%
    # Add strain collection ID
    mutate(strain_collection_id = strain_collection_id) %>%
    # arrange columns
    select(strain_collection_id, strain_id, strain_name, plate, row, column, plate_control, everything())
}

plate_gather <- function(path, value, plate_pattern = '(?<=(plate)).') {
  read_csv(path, col_types = cols()) %>%
    pivot_longer(
      -row,
      names_to = "column",
      values_to = value,
      names_transform = list(column = as.integer)
    ) %>%
    mutate(plate = str_extract(path, plate_pattern) %>% as.integer)
}
