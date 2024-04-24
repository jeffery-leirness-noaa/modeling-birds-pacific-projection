# get configuration values
config <- config::get(file = "config.yaml")

# load targets package
library(targets)

# set target options
tar_option_set(
  format = "qs",
  controller = crew::crew_controller_local(workers = parallel::detectCores() - 1, seconds_idle = 10)
)

# source R scripts in the R/ folder
tar_source()

simple_model_func <- function(.data, sp, dayofyear_k = -1, mgcv_gamma = 1, basis = "tp") {
  form <- count ~ platform +
    s(julianday, bs = basis) +
    s(dayofyear, bs = "cc", k = dayofyear_k) +
    s(depth, bs = basis)
  mgcv::gam(form,
            data = dplyr::rename(.data, "count" = toupper(sp)),
            family = mgcv::nb(),
            gamma = mgcv_gamma)
}

list(
  tar_target(data_path, command = config$dataset, format = "file"),
  tar_target(data, command = prepare_data(data_path)),
  tar_target(mods, command = tibble::tibble(sp = c("atpu", "blki", "coei", "noga", "rtlo"))),
  tar_target(simple_model,
             command = simple_model_func(.data = data, sp = mods$sp),
             pattern = map(mods),
             iteration = "list")
)
