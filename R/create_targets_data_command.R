#' Create Targets Data Loading Command
#'
#' This function generates an expression to load data files for use with the {targets} package.
#' It can handle different file types (CSV, raster, GeoPackage) and can load from either
#' local storage or Azure Blob Storage.
#'
#' @param file_name Character string. Name of the file to load.
#' @param local Logical. If TRUE (default), load from local filesystem. If FALSE, load from Azure Blob Storage.
#' @param container_name Character string. Default is "raw". Specifies the container name when using
#'   local storage (maps to directory) or Azure Blob Storage (maps to container).
#'
#' @return An unevaluated expression (created with rlang::expr()) that, when evaluated,
#'   will load the specified file with the appropriate function based on file extension:
#'   \itemize{
#'     \item .csv files are loaded with readr::read_csv()
#'     \item .tif, .tiff, .nc files are loaded with terra::rast()
#'     \item .gpkg files are loaded with sf::read_sf()
#'   }
#'
#' @details
#' When `local = TRUE`, the function uses local file paths based on global options:
#' \itemize{
#'   \item For container_name = "raw", uses opt$dir_in
#'   \item For container_name = "processing", uses opt$dir_processing
#' }
#'
#' When `local = FALSE`, the function loads data from Azure Blob Storage using:
#' \itemize{
#'   \item Azure authentication via azure_auth_token()
#'   \item Storage endpoint from TARGETS_ENDPOINT environment variable
#'   \item The specified container_name
#' }
#'
#' The function is designed to work with the {targets} package for creating data
#' pipeline workflows with consistent data loading mechanisms.
create_targets_data_command <- function(file_name, local = TRUE,
                                        container_name = "raw") {
  if (local) {
    ext <- fs::path_ext(file_name)
    if (container_name == "raw") {
      dir_use <- opt$dir_in
    } else if (container_name == "processing") {
      dir_use <- opt$dir_processing
    }
    if (ext == "csv") {
      fs::path(!!dir_use, !!file_name) |>
        readr::read_csv() |>
        rlang::expr()
    } else if (ext %in% c("tif", "tiff", "nc")) {
      fs::path(!!dir_use, !!file_name) |>
        terra::rast() |>
        rlang::expr()
    } else if (ext == "gpkg") {
      fs::path(!!dir_use, !!file_name) |>
        sf::read_sf() |>
        rlang::expr()
    }
  } else {
    token <- azure_auth_token()
    AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"), token = !!token) |>
      AzureStor::storage_container(name = !!container_name) |>
      storage_read(!!file_name) |>
      rlang::expr()
  }
}
