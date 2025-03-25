
targets::tar_load_globals()
meta <- targets::tar_meta(fields = tidyselect::any_of("data"),
                          targets_only = TRUE, store = store)

targets::tar_source()
token <- azure_auth_token()
keys <- AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                                    token = token) |>
  AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER")) |>
  AzureStor::list_storage_files(fs::path(targets::tar_path_store())) |>
  tibble::as_tibble()

dat_key <- fs::path_file(keys$name)
ck <- dplyr::filter(meta, data %in% dat_key) |>
  dplyr::filter(stringr::str_starts(name, "model_workflows"))
dat_rm <- ck$data

nm <- "data_prediction_date"
dat_rm <- dplyr::filter(meta, stringr::str_starts(name, nm)) |>
  dplyr::pull(data)
remove <- dplyr::filter(keys, !isdir & fs::path_file(name) %in% dat_rm)
if (nrow(remove) > 0) {
  container <- AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                                           token = token) |>
    AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER"))
  purrr::walk(remove$name,
              .f = \(x) AzureStor::delete_blob(container,
                                               blob = x,
                                               confirm = FALSE))
}
