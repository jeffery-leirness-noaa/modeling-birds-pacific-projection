targets::tar_make()

targets::tar_load_everything()

targets::tar_load_globals()
tmp <- c("grid_10km",
         "data_species_info",
         "data_bird_raw",
         "data_bird_10km",
         "data_bird_10km_wc12",
         "data_bathy_10km",
         "data_slope_10km",
         "data_analysis",
         "data_analysis_dev",
         "data_analysis_test",
         "species_to_model",
         "models_to_run",
         "model_fits")
for (i in seq(along = tmp)) {
  assign(tmp[i], targets::tar_read_raw(tmp[i]))
}

workflows::extract_preprocessor(model_workflows[[1]]) |>
  recipes::prep()

data_bird_10km |>
  dplyr::mutate(grp = ifelse(date >= "2011-01-01", "2011_2017", "1980_2010")) |>
  janitor::tabyl(grp)

thresh <- 50
dat_temp <- data_bird_10km |>
  dplyr::mutate(grp = ifelse(date >= "2011-01-01", "2011_2017", "1980_2010")) |>
  dplyr::group_by(grp) |>
  dplyr::summarise(dplyr::across(anmu:wgwh, ~ sum(.x > 0,
                                                  na.rm = TRUE))) |>
  tidyr::pivot_longer(cols = anmu:wgwh,
                      names_to = "code",
                      names_transform = list(code = stringr::str_to_upper),
                      values_to = "cells_with_sightings") |>
  tidyr::pivot_wider(names_from = grp,
                     values_from = cells_with_sightings,
                     names_prefix = "cells_with_sightings_") |>
  dplyr::mutate(cells_with_sightings_total = cells_with_sightings_1980_2010 + cells_with_sightings_2011_2017) |>
  dplyr::filter(cells_with_sightings_total >= thresh) |>
  dplyr::inner_join(y = data_species_info, by = "code") |>
  tidyr::drop_na(sortorder) |>
  dplyr::mutate(code = stringr::str_to_lower(code),
                size_class = stringr::str_to_lower(size_class)) |>
  dplyr::select(!marine_bird) |>
  dplyr::arrange(sortorder)

dat_temp |>
  dplyr::filter(cells_with_sightings_1980_2010 < thresh) |>
  print(n = Inf)

dat_temp |>
  dplyr::mutate(dplyr::across(c(cells_with_sightings_1980_2010, cells_with_sightings_2011_2017), ~ .x / cells_with_sightings_total)) |>
  dplyr::select(code:cells_with_sightings_total, common_nm) |>
  print(n = Inf)

dat1 <- data_bird_10km |>
  dplyr::filter(date < lubridate::as_date("2011-01-01")) |>
  dplyr::pull(cell)
dat2 <- data_bird_10km |>
  dplyr::filter(date >= lubridate::as_date("2011-01-01")) |>
  dplyr::pull(cell)

r <- terra::unwrap(grid_10km)
val <- terra::values(r) |>
  tibble::as_tibble()
val$lyr.2 <- ifelse(val$lyr.1 %in% dat1, 1, 0)
val$lyr.3 <- ifelse((val$lyr.1 %in% dat2) & !(val$lyr.1 %in% dat1), 2, val$lyr.2)
terra::values(r) <- val$lyr.3
terra::plot(r)


# spatial cross-validation
dat <- sf::st_as_sf(data_bird_10km, coords = c("lon", "lat"), crs = "WGS84") |>
  sf::st_transform(crs = sf::st_crs(terra::unwrap(grid_10km)))

# cluster_folds <- spatialsample::spatial_clustering_cv(dat)
# spatialsample::autoplot(cluster_folds)

gam_split <- rsample::initial_split(dat)
# gam_split <- rsample::initial_split(dat, strata = survey_id)

rsample::training(gam_split) |> janitor::tabyl(survey_id)
rsample::testing(gam_split) |> janitor::tabyl(survey_id)

gam_grid <- dials::grid_regular(dials::adjust_deg_free(), levels = 6)

block_folds <- rsample::training(gam_split) |>
  spatialsample::spatial_block_cv(v = 5)
# block_folds <- spatialsample::spatial_block_cv(dat, square = FALSE)
spatialsample::autoplot(block_folds)

# mod <- formula("bfal ~ offset(survey_area_km2) + platform + s(date_doy, bs = \"cc\") + s(date_decimal, bs = \"tp\")") |>
#   fit_model(data = dat, species_size_class = "lg", mgcv_select = TRUE)

mod_workflow <- formula("bfal ~ offset(survey_area_km2) + platform + s(date_doy, bs = \"cc\") + s(date_decimal, bs = \"tp\")") |>
  define_model_workflow(data = rsample::training(gam_split),
                        species_size_class = "lg",
                        mgcv_select = TRUE,
                        mgcv_gamma = 1)

ck <- tune::fit_resamples(
  mod_workflow,
  resamples = block_folds,
  control = tune::control_resamples(save_workflow = TRUE)
)
tune::extract_workflow(ck)
tune::extract_workflow(ck) |>
  workflows::extract_fit_engine()
ck <- tune::fit_resamples(
  mod_workflow,
  resamples = block_folds,
  control = tune::control_resamples(
    extract = function(x) list(workflows::extract_recipe(x),
                               workflows::extract_fit_engine(x)),
    save_workflow = TRUE
  )
)
tune::extract_workflow(ck)
tune::collect_extracts(ck)$.extracts[[1]]
tune::collect_metrics(ck)

ck2 <- tune::tune_grid(mod_workflow, resamples = block_folds, grid = gam_grid,
                       control = tune::control_grid(extract = function(x) list(workflows::extract_recipe(x),
                                                                               workflows::extract_fit_engine(x))))
tune::collect_metrics(ck2)
tune::collect_extracts(ck2)$.extracts[[1]]
tune::autoplot(ck2)

tune::show_best(ck2, metric = "rmse")
mod_best <- tune::select_best(ck2, metric = "rmse")

mod_workflow_final <- tune::finalize_workflow(mod_workflow, parameters = mod_best)

final_fit <- tune::last_fit(mod_workflow_final, split = gam_split)
tune::collect_metrics(final_fit)
tune::collect_predictions(final_fit)

final_mod <- tune::extract_workflow(final_fit)
