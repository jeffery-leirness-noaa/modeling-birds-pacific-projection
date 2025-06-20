source("R/azure_utils.R")

path <- tempdir()
for (file in c("crew", "meta", "process", "progress")) {
  azure_download(fs::path("meta", file), path = fs::path(path, "meta", file))
}

meta <- targets::tar_meta(store = path)

dplyr::filter(
  meta,
  stringr::str_starts(name, pattern = "model_predictions_daily_gfdl_")
) |>
  dplyr::pull(seconds) |>
  summary()

n_completed <- stringr::str_starts(meta$name, pattern = "model_predictions_daily_gfdl_") |>
  sum()
n_models <- stringr::str_starts(meta$name, pattern = "model_fits_") |>
  sum()
n_years <- length(1980:2017)

(comp <- n_completed / (n_models * n_years))

(1 - comp) / (comp - 0.5792982)
