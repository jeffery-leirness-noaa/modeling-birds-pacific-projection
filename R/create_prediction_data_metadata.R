#' Create Metadata for Prediction Data Files
#'
#' This function creates a metadata data frame for NetCDF (*.nc) files 
#' by extracting information from their file paths. It identifies Earth System Model (ESM),
#' variable, year, date, and path information by parsing the directory structure
#' and filenames.
#'
#' @param .path Character string. Root directory path containing NetCDF files to process.
#' @param path_remove Character string. Optional. A prefix to remove from file paths
#'   in the returned data frame. Useful for creating relative paths.
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{esm}: Earth System Model identifier extracted from the directory structure
#'     \item \code{variable}: Variable name extracted from the directory structure
#'     \item \code{year}: Year extracted from the filename
#'     \item \code{date}: Date calculated from the year and day information in the filename
#'     \item \code{path}: Path to the NetCDF file, optionally with \code{path_remove} prefix removed
#'   }
#'
#' @details
#' The function assumes a specific directory and filename structure:
#' \itemize{
#'   \item Directory structure: /path/to/data/ESM_NAME/VARIABLE_NAME/
#'   \item Filename format: YEAR_DAY.nc (e.g., 2020_32.nc for day 32 of 2020)
#' }
create_prediction_data_metadata <- function(.path, path_remove = NULL) {
  df <- tibble::tibble(
    path = fs::dir_ls(.path, recurse = TRUE, type = "file", glob = "*.nc"),
    esm = fs::path_dir(path) |>
      fs::path_dir() |>
      fs::path_file(),
    variable = fs::path_dir(path) |>
      fs::path_file(),
    year = fs::path_file(path) |>
      fs::path_ext_remove() |>
      stringr::str_split_i(pattern = "_", i = 1) |>
      as.numeric(),
    day = fs::path_file(path) |>
      fs::path_ext_remove() |>
      stringr::str_split_i(pattern = "_", i = 2) |>
      as.numeric(),
    date = lubridate::as_date(day, origin = lubridate::as_date(stringr::str_c(year, "-01-01")) - 1)
  ) |>
    dplyr::select(esm, variable, year, date, path)
  if (!is.null(path_remove)) {
    df <- dplyr::mutate(
      df,
      path = stringr::str_replace(path, pattern = paste0(path_remove, "/"),
                                  replacement = "")
    )
  }
  df
}
