# targets azure garbage collection

# get names of all files in targets::tar_path_store() that are not in
# targets::tar_meta() (i.e., all files not part of the most current targets
# specification)
targets::tar_source()
token <- azure_auth_token()
fl <- AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                                  token = token) |>
  AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER")) |>
  AzureStor::list_storage_files(fs::path(targets::tar_path_store())) |>
  dplyr::filter(!isdir & !(fs::path_file(name) %in% targets::tar_meta()$data)) |>
  tibble::as_tibble()

# delete files not part of the most current targets specification
container <- AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                                         token = token) |>
  AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER"))
purrr::walk(fl$name, .f = \(x) AzureStor::delete_blob(container, blob = x,
                                                      confirm = FALSE))
