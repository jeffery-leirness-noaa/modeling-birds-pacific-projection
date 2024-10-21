# this will fail for .tiff files when local = TRUE!
create_targets_data_command <- function(file_name, local = TRUE) {
  if (local) {
    fs::path(opt$dir_in, !!file_name) |>
      readr::read_csv() |>
      rlang::expr()
  } else {
    token <- azure_auth_token()
    AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"), token = !!token) |>
      AzureStor::storage_container(name = "raw") |>
      storage_read(!!file_name) |>
      rlang::expr()
  }
}
