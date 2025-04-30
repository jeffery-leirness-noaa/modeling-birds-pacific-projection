#' Read Spatial {sf} Data from Azure Storage
#'
#' This function downloads and reads spatial data from Azure Storage via the {sf} package.
#'
#' @param container Azure storage container object.
#' @param file Character string. Path to the file in the container.
#'
#' @return An sf object.
#'
#' @examples
#' # Read a GeoPackage file from Azure Storage
#' spatial_data <- storage_read_sf(my_container, "path/to/data.gpkg")
storage_read_sf <- function(container, file) {
  fl <- fs::file_temp()
  on.exit(try(fs::file_delete(fl), silent = TRUE))
  AzureStor::storage_download(container, src = file, dest = fl)
  sf::read_sf(fl)
}

#' Read QS Format Data from Azure Storage
#'
#' This function downloads and reads data in QS format from Azure Storage via the {qs} package.
#'
#' @param container Azure storage container object.
#' @param file Character string. Path to the file in the container.
#'
#' @return The object stored in the QS file.
#'
#' @examples
#' # Read a qs file from Azure Storage
#' data <- storage_read_qs(my_container, "path/to/data.qs")
storage_read_qs <- function(container, file) {
  fl <- fs::file_temp()
  on.exit(try(fs::file_delete(fl), silent = TRUE))
  AzureStor::storage_download(container, src = file, dest = fl)
  qs::qread(fl)
}

#' Read Data from Azure Storage
#'
#' This function downloads and reads data from Azure Storage in various formats,
#' automatically detecting the format based on file extension.
#'
#' @param container Azure storage container object.
#' @param file Character string. Path to the file in the container.
#' @param ext Character string. Optional file extension override.
#' @param ... Additional arguments passed to the format-specific reading function.
#'
#' @return The data from the file, in an appropriate R object.
#'
#' @examples
#' # Read a CSV file from Azure Storage
#' data <- storage_read(my_container, "path/to/data.csv")
#'
#' # Read a NetCDF file from Azure Storage
#' raster_data <- storage_read(my_container, "path/to/data.nc")
storage_read <- function(container, file, ext = NULL, ...) {
  if (is.null(ext)) ext <- fs::path_ext(file)
  if (!(ext %in% c("csv", "txt", "rds"))) {
    fl <- fs::file_temp()
    on.exit(try(fs::file_delete(fl), silent = TRUE))
    AzureStor::storage_download(container, src = file, dest = fl)
  }
  if (ext == "csv") {
    AzureStor::storage_read_csv(container, file, ...)
  } else if (ext == "txt") {
    AzureStor::storage_read_delim(container, file, ...)
  } else if (ext == "rds") {
    AzureStor::storage_load_rds(container, file)
  } else if (ext == "qs") {
    qs::qread(fl)
  } else if (ext == "gpkg") {
    sf::read_sf(fl)
  } else if (ext %in% c("tif", "tiff", ".nc")) {
    r <- terra::rast(fl)
    terra::set.values(r)
    r
  }
}

#' Download a Directory from Azure Storage
#'
#' This function downloads an entire directory from Azure Storage.
#'
#' @param container Azure storage container object.
#' @param src Character string. Source directory in the container.
#' @param dest Character string. Destination directory on the local system.
#'
#' @return Invisible. The function is called for its side effects.
#'
#' @examples
#' # Download a directory from Azure Storage
#' storage_download_dir(my_container, "path/to/dir", "local/path")
storage_download_dir <- function(container, src, dest) {
  is_dir <- AzureStor::list_storage_files(container) |>
    tibble::as_tibble() |>
    dplyr::filter(name == src) |>
    dplyr::pull(isdir)
  if (is_dir) {
    src <- AzureStor::list_storage_files(container, src) |>
      tibble::as_tibble() |>
      dplyr::filter(!isdir) |>
      dplyr::pull(name)
    dest <- fs::path(dest, src)
    AzureStor::storage_multidownload(container, src = src, dest = dest, recursive = TRUE)
  }
}
