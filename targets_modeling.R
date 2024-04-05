# load targets package
library(targets)

# set target options
tar_option_set(
  format = "qs",
  controller = crew::crew_controller_local(workers = 8, seconds_idle = 30)
)

# source R scripts in the R/ folder
tar_source()

simple_model_func <- function(sp, dayofyear_k = -1, mgcv_gamma = 1, basis = "tp", dat_fn) {
  dat <- readRDS(dat_fn) |>
    tibble::as_tibble() |>
    dplyr::rename("count" = toupper(sp))
  form <- count ~ platform +
    s(julianday, bs = basis) +
    s(dayofyear, bs = "cc", k = dayofyear_k) +
    s(depth, bs = basis)
  mgcv::gam(form, data = dat, family = mgcv::nb(), gamma = mgcv_gamma)
}

list(
  tar_target(atlantic_data_file, command = "data/segmented_data_analysis_2023-02-15.rds", format = "file"),
  tar_target(mods, command = tibble::tibble(sp = c("atpu", "blki", "coei", "noga", "rtlo"))),
  tar_target(simple_model,
             command = simple_model_func(sp = mods$sp, dat_fn = atlantic_data_file),
             pattern = map(mods),
             iteration = "list")
)
