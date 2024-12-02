fl <- fs::path("data/segmented-data-10km.csv")

targets::tar_load_globals()
targets::tar_read(data_bird_10km) |>
  readr::write_csv(file = fl)

AzureStor::upload_to_url(fl,
                         dest = paste(Sys.getenv("TARGETS_ENDPOINT"),
                                      "raw",
                                      "species-data", fs::path_file(fl), sep = "/"),
                         token = azure_auth_token())

fs::file_delete(fl)
