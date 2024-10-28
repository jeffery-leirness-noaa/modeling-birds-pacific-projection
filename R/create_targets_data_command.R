create_targets_data_command <- function(file_name, local = TRUE) {
  if (local) {
    ext <- fs::path_ext(file_name)
    if (ext == "csv") {
      fs::path(opt$dir_in, !!file_name) |>
        readr::read_csv() |>
        rlang::expr()
    } else if (ext %in% c("tif", "tiff")) {
      fs::path(opt$dir_in, !!file_name) |>
        terra::rast() |>
        terra::wrap() |>
        rlang::expr()
    } else if (ext == "gpkg") {
      fs::path(opt$dir_in, !!file_name) |>
        sf::read_sf() |>
        rlang::expr()
    }
  } else {
    token <- azure_auth_token()
    AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"), token = !!token) |>
      AzureStor::storage_container(name = "raw") |>
      storage_read(!!file_name) |>
      rlang::expr()
  }
}
