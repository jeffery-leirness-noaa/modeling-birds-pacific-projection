# source targets helper file
if (fs::file_exists("_targets_helper.R")) {
  source("_targets_helper.R")
}

# get configuration values
config <- config::get(file = "config.yaml")

# source R scripts in the R/ folder
targets::tar_source()

# set target options
if (!exists("targets_cas_local")) {
  targets_cas_local <- FALSE
}
if (targets_cas_local) {
  repository <- targets::tar_repository_cas_local(opt$dir_targets_cas)
} else {
  repository <- targets::tar_repository_cas(upload = azure_upload,
                                            download = azure_download,
                                            exists = azure_exists)
  token <- azure_auth_token()
  resources <- targets::tar_resources(
    repository_cas = targets::tar_resources_repository_cas(
      envvars = c(TARGETS_AUTH_TOKEN = token$credentials$access_token)
    ))
}
targets::tar_option_set(
  packages = c("qs", "sf"),
  format = "qs",
  repository = repository,
  memory = "transient",
  garbage_collection = TRUE,
  resources = if (targets_cas_local) NULL else resources,
  cue = targets::tar_cue(repository = FALSE),
  controller = crew::crew_controller_local(workers = parallel::detectCores() - 1, seconds_idle = 10)
)

# create objects needed for certain targets
simple_model_func <- function(data, sp, dayofyear_k = -1, mgcv_gamma = 1, basis = "tp") {
  form <- count ~ platform +
    # s(julianday, bs = basis) +
    # s(dayofyear, bs = "cc", k = dayofyear_k) +
    s(depth, bs = basis)
  mgcv::gam(form,
            data = dplyr::rename(data, "count" = tolower(sp)),
            family = mgcv::nb(),
            gamma = mgcv_gamma)
}
values <- tibble::tibble(sp = c("bfal", "blki", "comu"))


# specify targets ---------------------------------------------------------
target_raw_data_bird <- targets::tar_target(
  raw_data_bird,
  command = create_targets_data_command("segmented-data.csv",
                                        local = targets_cas_local,
                                        token = Sys.getenv("TARGETS_AUTH_TOKEN")) |>
    eval(),
  cue = targets::tar_cue(mode = "always")
)
target_raw_data_wc12 <- targets::tar_target(
  raw_data_wc12,
  command = create_targets_data_command("segmented-data-wc12_3.csv",
                                        local = targets_cas_local,
                                        token = Sys.getenv("TARGETS_AUTH_TOKEN")) |>
    eval(),
  cue = targets::tar_cue(mode = "always")
)
target_raw_data_wcra31 <- targets::tar_target(
  raw_data_wcra31,
  command = create_targets_data_command("segmented-data-wcra_2.csv",
                                        local = targets_cas_local,
                                        token = Sys.getenv("TARGETS_AUTH_TOKEN")) |>
    eval(),
  cue = targets::tar_cue(mode = "always")
)
target_raw_data_wcnrt <- targets::tar_target(
  raw_data_wcnrt,
  command = create_targets_data_command("segmented-data-wcnrt.csv",
                                        local = targets_cas_local,
                                        token = Sys.getenv("TARGETS_AUTH_TOKEN")) |>
    eval(),
  cue = targets::tar_cue(mode = "always")
)
target_depth_100m <- targets::tar_target(
  depth_100m,
  command = create_targets_data_command("depth-100m.tif",
                                        local = targets_cas_local,
                                        token = Sys.getenv("TARGETS_AUTH_TOKEN")) |>
    eval()
)
target_slope_100m <- targets::tar_target(
  slope_100m,
  command = MultiscaleDTM::SlpAsp(terra::unwrap(depth_100m), w = c(3, 3),
                                  method = "queen", metrics = "slope") |>
    terra::wrap()
)
# block average 100-m resolution depth layer to 10-km resolution
target_depth_10km <- targets::tar_target(
  depth_10km,
  command = terra::aggregate(terra::unwrap(depth_100m), fact = 100,
                             fun = "mean", na.rm = TRUE)
)
target_depth_10km <- targets::tar_target(
  slope_10km,
  command = terra::aggregate(terra::unwrap(slope_100m), fact = 100,
                             fun = "mean", na.rm = TRUE)
)
target_data_covariates <- targets::tar_target(
  data_covariates,
  command = {
    by <- c("date", "survey_id", "transect_id", "segment_id")
    hindcast <- prepare_data_covariates(raw_data_wc12) |>
      dplyr::rename_with(~ stringr::str_replace(.x, pattern = "monthly_",
                                                replacement = "hindcast_"),
                         .cols = tidyselect::starts_with("monthly_"))
    reanalysis <- dplyr::bind_rows(prepare_data_covariates(raw_data_wcra31),
                                   prepare_data_covariates(raw_data_wcnrt)) |>
      dplyr::rename_with(~ stringr::str_replace(.x, pattern = "monthly_",
                                                replacement = "reanalysis_"),
                         .cols = tidyselect::starts_with("monthly_"))
    dplyr::left_join(raw_data_bird, y = hindcast, by = by) |>
      dplyr::left_join(y = reanalysis, by = by) |>
      prepare_data_analysis()
  }
)
target_data_covariates_dev <- targets::tar_target(
  data_covariates_dev,
  command = {
    # set.seed(20240424)
    data_covariates |>
      sample_data(platform, prop = 0.1)
  }
)
target_data_covariates_test <- targets::tar_target(
  data_covariates_test,
  command = {
    # set.seed(20240424)
    data_covariates |>
      sample_data(platform, prop = 0.4)
  }
)
# target4 <- tar_target(
#   rfile,
#   command = "_test_simple_model_func.R",
#   format = "file"
# )
# # the following target will always rerun
# # possible solution: first time, run without `format = "file"``
# # once all runs have completed successfully, run all subsequent with `format = "file"`
# # target5 <- tar_map(values = values,
# #                    tar_target(simple_model_test,
# #                               command = {
# #                                 submit_job_rfile(rfile = rfile,
# #                                                  additional_args = paste0("--sp='", sp, "'"),
# #                                                  dir_in = Sys.getenv("AML_DATASTORE_RAW"),
# #                                                  dir_out = Sys.getenv("AML_DATASTORE_PROCESSING"),
# #                                                  environment = config$aml_env,
# #                                                  compute = "nccos-vm-cluster-ds2",
# #                                                  experiment_name = "test-simple-model",
# #                                                  display_name = paste0("test-simple-model-", sp),
# #                                                  description = "Test running simple model on separate nodes of compute cluster.")
# #                                 fs::path(opt$dir_out, paste0(sp, ".rds"))
# #                               },
# #                               format = "file"
# #                    ))
# target5 <- tar_target(
#   simple_model_test_laal,
#   command = {
#     submit_job_rfile(rfile = rfile,
#                      additional_args = "--sp='laal'",
#                      dir_in = Sys.getenv("AML_DATASTORE_RAW"),
#                      dir_out = Sys.getenv("AML_DATASTORE_PROCESSING"),
#                      environment = config$aml_env,
#                      compute = "nccos-vm-cluster-ds2",
#                      experiment_name = "test-simple-model",
#                      display_name = "test-simple-model-laal",
#                      description = "Test running simple model on separate nodes of compute cluster.")
#     fs::path(opt$dir_out, "laal.rds")
#   },
#   format = "file"
# )
# fs::file_exists(fs::path(opt$dir_out, "laal.rds"))
# fs::dir_tree(opt$dir_out)


# submit targets ----------------------------------------------------------
list(
  target_raw_data_bird,
  target_raw_data_wc12,
  target_raw_data_wcra31,
  target_raw_data_wcnrt,
  target_depth_100m,
  target_slope_100m,
  target_depth_10km,
  target_slope_10km,
  target_data_covariates,
  target_data_covariates_dev,
  target_data_covariates_test
)
