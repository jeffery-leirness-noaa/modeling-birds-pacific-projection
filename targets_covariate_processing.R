# load targets package
library(targets)

# set target options
tar_option_set(
  format = "qs",
  controller = crew::crew_controller_local(workers = 10, seconds_idle = 30)
)

# source necessary R scripts
tar_source("R/_functions.R")

values <- tibble::tibble(
  data_source = file.path("data", c("wcra31_bbv_200_daily_1980_2010.nc", "wcra31_sst_daily_1980_2010.nc")),
  output_fname = fs::path_file(data_source) |>
    fs::path_ext_remove() |>
    stringr::str_split(pattern = "_") |>
    purrr::map_vec(\(x) paste(x[1:2], collapse = "-"))
)
targets <- tarchetypes::tar_map(
  values = values,
  names = output_fname,
  tar_target(file, command = data_source, format = "file", deployment = "main"),
  tar_target(int_monthly, command = head(create_intervals_monthly(file)), deployment = "main"),
  tar_target(covariates,
             command = create_covariate_output(file,
                                               start = int_monthly$start,
                                               end = int_monthly$end,
                                               fname = paste0(output_fname, "-monthly"),
                                               label = int_monthly$label),
             pattern = map(int_monthly),
             format = "file",
             iteration = "vector"
  )
)
list(targets)

# list(
#   # wcra31_sst_daily
#   tar_target(wcra31_sst_daily_file, command = "data/wcra31_sst_daily_1980_2010.nc", format = "file", deployment = "main"),
#   tar_target(wcra31_sst_int_monthly, command = head(create_intervals_monthly(wcra31_sst_daily_file)), deployment = "main"),
#   tar_target(covariates_wcra31_sst,
#              command = create_covariate_output(wcra31_sst_daily_file,
#                                                start = wcra31_sst_int_monthly$start,
#                                                end = wcra31_sst_int_monthly$end,
#                                                fname = "wcra31-sst-monthly",
#                                                label = wcra31_sst_int_monthly$label),
#              pattern = map(wcra31_sst_int_monthly),
#              format = "file",
#              iteration = "vector"
#   )
# )
