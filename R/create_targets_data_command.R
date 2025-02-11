create_targets_data_command <- function(file_name, local = TRUE,
                                        container_name = "raw") {
  if (local) {
    ext <- fs::path_ext(file_name)
    if (ext == "csv") {
      fs::path(opt$dir_in, !!file_name) |>
        readr::read_csv() |>
        rlang::expr()
    } else if (ext %in% c("tif", "tiff", "nc")) {
      fs::path(opt$dir_in, !!file_name) |>
        terra::rast() |>
        rlang::expr()
    } else if (ext == "gpkg") {
      fs::path(opt$dir_in, !!file_name) |>
        sf::read_sf() |>
        rlang::expr()
    }
  } else {
    token <- azure_auth_token()
    AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"), token = !!token) |>
      AzureStor::storage_container(name = container_name) |>
      storage_read(!!file_name) |>
      rlang::expr()
  }
}
