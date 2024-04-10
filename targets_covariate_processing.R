# load targets package
library(targets)

# set target options
tar_option_set(
  format = "qs",
  controller = crew::crew_controller_local(workers = parallel::detectCores() - 1, seconds_idle = 5)
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
values <- tibble::tibble(
  data_source = fs::dir_ls(fs::path("data", c("wcra31", "wcnrt", "future"))),
  output_fname = fs::path_file(data_source) |>
    fs::path_ext_remove() |>
    stringr::str_split(pattern = "_") |>
    purrr::map_vec(\(x) paste(x[1:2], collapse = "-"))
)
targets <- tarchetypes::tar_map(
  values = values,
  names = output_fname,
  tar_target(file, command = data_source, format = "file", deployment = "main"),
  tar_target(int_daily, command = create_intervals_daily(file, round_dt = TRUE)),
  tar_target(covariates,
             command = create_covariate_output(file,
                                               start = int_daily$start,
                                               end = int_daily$end,
                                               fname = paste0(output_fname, "-daily"),
                                               label = int_daily$label,
                                               round_dt = TRUE),
             pattern = map(int_daily),
             format = "file",
             iteration = "vector"
  )
)
list(targets)
