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
token <- azure_auth_token()
tar_option_set(
  packages = c("qs", "sf"),
  format = "qs",
  repository = tar_repository_cas(
    upload = function(key, path) {
      if (fs::is_dir(path)) {
        stop("This CAS repository does not support directory outputs.")
      }
      AzureStor::upload_to_url(path,
                               dest = paste(Sys.getenv("TARGETS_ENDPOINT"),
                                            Sys.getenv("TARGETS_CONTAINER"),
                                            targets::tar_path_store(), key, sep = "/"),
                               token = Sys.getenv("TARGETS_AUTH_TOKEN"))
    },
    download = function(key, path) {
      AzureStor::download_from_url(paste(Sys.getenv("TARGETS_ENDPOINT"),
                                         Sys.getenv("TARGETS_CONTAINER"),
                                         targets::tar_path_store(), key, sep = "/"),
                                   dest = path,
                                   token = Sys.getenv("TARGETS_AUTH_TOKEN"),
                                   overwrite = TRUE)
    },
    exists = function(key) {
      AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                                  token = Sys.getenv("TARGETS_AUTH_TOKEN")) |>
        AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER")) |>
        AzureStor::storage_file_exists(fs::path(targets::tar_path_store(), key))
    }
  ),
  memory = "transient",
  garbage_collection = TRUE,
  resources = tar_resources(repository_cas = tar_resources_repository_cas(
    envvars = c(TARGETS_AUTH_TOKEN = token$credentials$access_token)
  )),
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
  command = AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net",
                                        token = token) |>
    AzureStor::storage_container(name = "raw") |>
    AzureStor::storage_read_csv("segmented-data.csv"),
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
