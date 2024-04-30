# load targets package
library(targets)
library(tarchetypes)

# set target options
tar_option_set(
  packages = c("qs"),
  format = "qs",
  controller = crew::crew_controller_local(workers = parallel::detectCores() - 1, seconds_idle = 10)
)

# source necessary R scripts
tar_source("R/_functions.R")

# # transfer files from blob to compute
# file_transfer <- reticulate::import_from_path("file_transfer", path = "../File-Transfer-Solution")
# account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
# container_name <- "raw"
# local_folder <- "data"
# cloud_folder <- ""
# ftc <- file_transfer$FileTransferClient(account_url,
#                                         container_name = container_name,
#                                         local_folder = local_folder,
#                                         cloud_folder = cloud_folder)
# ftc$transfer_from_blob_to_compute()

# targets
data_source <- fs::path("data", c("wcra31", "wcnrt")) |>
  fs::dir_ls()
output_fname <- fs::path_file(data_source) |>
  fs::path_ext_remove() |>
  stringr::str_split(pattern = "_") |>
  purrr::map_vec(.f = \(x) paste(x[1:2], collapse = "-"))
values <- tibble::tibble(data_source = data_source,
                         output_fname = output_fname)
values <- values[c(5, 14), ]


list(
  tar_target(data_path, command = "data/segmented-data.csv", format = "file",
             deployment = "main"),
  tar_target(data,
             command = prepare_data(data_path) |>
               dplyr::mutate(yrmon = lubridate::floor_date(date, "month")),
             deployment = "main"),
  tar_map(values = values,
          names = output_fname,
          tar_target(file, command = data_source, format = "file"),
          tar_target(extract_covs,
                     command = extract_covariate_data(file,
                                                      at = data,
                                                      time_column = "yrmon",
                                                      round_dt = TRUE)))
)



# targets <- tarchetypes::tar_map(
#   values = values,
#   names = output_fname,
#   tar_target(file, command = data_source, format = "file", deployment = "main"),
#   tar_target(data_path, command = "data/segmented-data.csv", format = "file"),
#   tar_target(data,
#              command = prepare_data(data_path) |>
#                dplyr::mutate(yrmon = lubridate::floor_date(date, "month")) |>
#                dplyr::group_by(yrmon)),
#   tar_target(extract_covs,
#              command = data |>
#                dplyr::mutate("{output_fname}" := extract_covariate_data(geometry |> terra::vect(),
#                                                                         file = data_source,
#                                                                         start = unique(yrmon),
#                                                                         end = lubridate::rollforward(unique(yrmon)))) |>
#                dplyr::pull())
#   # tar_target(int_monthly, command = create_intervals_monthly(file, round_dt = TRUE)),
#   # tar_target(covariates,
#   #            command = create_covariate_output(file,
#   #                                              start = int_monthly$start,
#   #                                              end = int_monthly$end,
#   #                                              fname = paste0(output_fname, "-monthly"),
#   #                                              label = int_monthly$label,
#   #                                              round_dt = TRUE),
#   #            pattern = map(int_monthly),
#   #            format = "file",
#   #            iteration = "vector"
#   # )
# )
# list(targets)
