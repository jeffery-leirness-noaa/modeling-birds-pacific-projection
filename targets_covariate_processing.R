# source targets helper file
if (fs::file_exists("_targets_helper.R")) {
  source("_targets_helper.R")
}

# load targets package
library(targets)
library(tarchetypes)

# set target options
tar_option_set(
  packages = c("qs", "sf"),
  format = "qs",
  controller = crew::crew_controller_local(workers = 2, seconds_idle = 10),
  memory = "transient",
  garbage_collection = TRUE
)

# source R scripts in the R/ folder
tar_source()

# create objects needed for certain targets
data_source <- fs::path(opt$dir_in, c("wcra31", "wcnrt")) |>
  fs::dir_ls()
var_name <- fs::path_file(data_source) |>
  fs::path_ext_remove() |>
  stringr::str_split(pattern = "_") |>
  purrr::map_vec(.f = \(x) paste(x[1:2], collapse = "_"))
values <- tibble::tibble(data_source = data_source,
                         var_name = var_name)

# targets
target1 <- tar_target(data_path,
                      command = fs::path(opt$dir_in, "segmented-data.rds"),
                      format = "file",
                      deployment = "main")
target2 <- tar_target(data,
                      command = prepare_data(data_path),
                      deployment = "main")
target3 <- tar_map(values = values,
                   names = var_name,
                   tar_target(file, command = data_source, format = "file"),
                   tar_target(extract,
                              command = extract_covariate_data(file,
                                                               at = data,
                                                               time_column = "date",
                                                               aggregate_by = "months",
                                                               name = var_name,
                                                               round_dt = TRUE) |>
                                tibble::as_tibble() |>
                                dplyr::select(var_name)))
target4 <- tar_combine(data_covariates,
                       target3[["extract"]],
                       command = dplyr::bind_cols(data, !!!.x) |>
                         dplyr::mutate(bbv = rowMeans(dplyr::pick(dplyr::ends_with("_bbv")), na.rm = TRUE),
                                       curl = rowMeans(dplyr::pick(dplyr::ends_with("_curl")), na.rm = TRUE),
                                       ild = rowMeans(dplyr::pick(dplyr::ends_with("_ild")), na.rm = TRUE),
                                       ssh = rowMeans(dplyr::pick(dplyr::ends_with("_ssh")), na.rm = TRUE),
                                       sst = rowMeans(dplyr::pick(dplyr::ends_with("_sst")), na.rm = TRUE),
                                       sustr = rowMeans(dplyr::pick(dplyr::ends_with("_sustr")), na.rm = TRUE),
                                       su = rowMeans(dplyr::pick(dplyr::ends_with("_su")), na.rm = TRUE),
                                       svstr = rowMeans(dplyr::pick(dplyr::ends_with("_svstr")), na.rm = TRUE),
                                       sv = rowMeans(dplyr::pick(dplyr::ends_with("_sv")), na.rm = TRUE),
                                       .keep = "unused") |>
                         tidyr::drop_na(!seastate) |>
                         dplyr::relocate(geometry, .after = dplyr::last_col()))
target5 <- tar_target(data_covariates_dev,
                      command = {
                        set.seed(20240424)
                        data_covariates |>
                          sample_data(platform, prop = 0.1)
                      })
target6 <- tar_target(data_covariates_test,
                      command = {
                        set.seed(20240424)
                        data_covariates |>
                          sample_data(platform, prop = 0.4)
                      })
list(
  target1,
  target2,
  target3,
  target4,
  target5,
  target6
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
