# source targets helper file
if (fs::file_exists("_targets_helper.R")) {
  source("_targets_helper.R")
}

# get configuration values
config <- config::get(file = "config.yaml")

# load targets package
library(targets)
library(tarchetypes)

# set target options
tar_option_set(
  packages = c("qs", "sf"),
  format = "qs",
  controller = crew::crew_controller_local(workers = parallel::detectCores() - 1, seconds_idle = 10),
  memory = "transient",
  garbage_collection = TRUE
)

# source R scripts in the R/ folder
tar_source()

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

# targets
target1 <- tar_target(data,
                      command = targets::tar_read_raw(config$target, store = targets::tar_config_get("store", project = "covariate_processing")))
target2 <- tar_target(mods,
                      command = tibble::tibble(sp = c("bfal", "blki", "comu")))
target3 <- tar_target(simple_model,
                      command = simple_model_func(data = data, sp = mods$sp),
                      pattern = map(mods),
                      iteration = "list")
list(
  target1,
  target2,
  target3
)
