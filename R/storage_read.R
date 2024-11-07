# custom function to load sf file
storage_read_sf <- function(container, file) {
  fl <- fs::file_temp()
  on.exit(try(fs::file_delete(fl), silent = TRUE))
  AzureStor::storage_download(container, src = file, dest = fl)
  sf::read_sf(fl)
}

# custom function to load qs file
storage_read_qs <- function(container, file) {
  fl <- fs::file_temp()
  on.exit(try(fs::file_delete(fl), silent = TRUE))
  AzureStor::storage_download(container, src = file, dest = fl)
  qs::qread(fl)
}

# custom function to load generic file
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
    terra::rast(fl) |>
      terra::wrap()
  }
}

# custom function to download entire directory
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
