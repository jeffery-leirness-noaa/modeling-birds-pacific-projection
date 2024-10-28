# targets azure garbage collection
tar_repository_cas_gc <- function(path = targets::tar_path_store(),
                                  store = targets::tar_config_get("store")) {

  # get names of all files in targets::tar_path_store() that are not in
  # targets::tar_meta() (i.e., all files not part of the most current targets
  # specification)
  meta <- targets::tar_meta(fields = tidyselect::any_of("data"),
                            targets_only = TRUE, store = store)

  targets::tar_source()
  token <- azure_auth_token()
  keys <- AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                                      token = token) |>
    AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER")) |>
    AzureStor::list_storage_files(fs::path(targets::tar_path_store())) |>
    tibble::as_tibble()

  # delete files not part of the most current targets specification
  if (nrow(keys) > 0) {
    remove <- dplyr::filter(keys, !isdir & !(fs::path_file(name) %in% meta$data))
    if (nrow(remove) > 0) {
      container <- AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                                               token = token) |>
        AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER"))
      purrr::walk(remove$name,
                  .f = \(x) AzureStor::delete_blob(container,
                                                   blob = x,
                                                   confirm = FALSE))
    }
  }
  invisible()

}
