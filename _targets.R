# source targets helper file
if (fs::file_exists("_targets_helper.R")) {
  source("_targets_helper.R")
}

# get configuration values
config <- config::get(file = "config.yaml")

# load targets package
library(targets)
library(tarchetypes)

# source R scripts in the R/ folder
tar_source()

# set target options
if (!exists("targets_cas_local")) {
  targets_cas_local <- FALSE
}
if (targets_cas_local) {
  repository <- fs::path(opt$dir_out, targets::tar_path_store()) |>
    tar_repository_cas_local()
  command1 <- readr::read_csv(fs::path(opt$dir_in, "segmented-data.csv")) |>
    expression()
} else {
  repository <- tar_repository_cas(upload = azure_upload,
                                   download = azure_download,
                                   exists = azure_exists)
  token <- azure_auth_token()
  resources <- tar_resources(
    repository_cas = tar_resources_repository_cas(
      envvars = c(TARGETS_AUTH_TOKEN = token$credentials$access_token)
    ))
  command1 <- AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net",
                                          token = token) |>
    AzureStor::storage_container(name = "raw") |>
    AzureStor::storage_read_csv("segmented-data.csv") |>
    expression()
}
tar_option_set(
  packages = c("qs", "sf"),
  format = "qs",
  repository = repository,
  memory = "transient",
  garbage_collection = TRUE,
  resources = if (targets_cas_local) NULL else resources,
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

# specify targets
target1 <- tar_target(
  data_covariates,
  command = eval(command1),
  cue = tar_cue(mode = "always")
)
target2 <- tar_target(
  data_covariates_dev,
  command = {
    set.seed(20240424)
    data_covariates |>
      sample_data(platform, prop = 0.1)
  }
)
target3 <- tar_target(
  data_covariates_test,
  command = {
    set.seed(20240424)
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

# submit targets
list(
  target1,
  target2,
  target3
)
