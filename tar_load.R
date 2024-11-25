targets::tar_make()

targets::tar_load_globals()
meta <- targets::tar_meta(targets_only = TRUE)
c(tidyselect::any_of(c("data_species_info",
                       "species_to_model",
                       "models_to_run")),
  tidyselect::starts_with("data_bird"),
  tidyselect::starts_with("data_analysis"),
  tidyselect::starts_with("model_")) |>
  tar_load_azure_store()

tar_load_azure_store("data_analysis")
lobstr::obj_size(data_analysis)
ck <- rsample::bootstraps(data_analysis, times = 1000)
lobstr::obj_size(ck)
# tar_load_azure_store("data_analysis_resamples_bootstrap")


analysis <- data_analysis_resamples_spatial |>
  dplyr::pull(splits) |>
  purrr::map(.f = rsample::analysis) |>
  purrr::list_rbind(names_to = "split") |>
  dplyr::mutate(set = "analysis")

temp <- data_analysis_resamples_spatial |>
  dplyr::pull(splits) |>
  purrr::map(.f = rsample::assessment) |>
  purrr::list_rbind(names_to = "split") |>
  dplyr::mutate(set = "assessment") |>
  dplyr::bind_rows(analysis) |>
  tibble::as_tibble() |>
  sf::st_as_sf()

ggplot2::ggplot(temp) +
  ggplot2::geom_sf(mapping = ggplot2::aes(color = set)) +
  ggplot2::facet_wrap(~ split)



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



model_fit_resamples_temporal_combined[[1]] |>
  tune::extract_workflow()

model_fit_resamples_temporal_combined[[1]] |>
  dplyr::select(id, .metrics) |>
  tidyr::unnest(cols = .metrics) |>
  dplyr::filter(.metric == "rmse") |>
  ggplot2::ggplot(mapping = ggplot2::aes(id, .estimate, group = 1)) +
  ggplot2::geom_line()

purrr::map(model_fit_resamples_spatial_combined,
           \(x) tune::collect_metrics(x)) |>
  purrr::list_rbind()


metrics <- list()
for (i in seq(along = model_fit_resamples_spatial_combined)) {
  metrics_i <- tune::collect_metrics(model_fit_resamples_spatial_combined[[i]])
  # pde <- tune::collect_extracts(model_fit_resamples_spatial_combined[[i]]) |>
  #   dplyr::pull(.extracts) |>
  #   purrr::map_vec(\(x) summary(x[[2]]$fit)$dev.expl)
  # metrics_i <- dplyr::bind_rows(metrics_i,
  #                               tibble::tibble(.metric = "pde",
  #                                              mean = mean(pde),
  #                                              n = length(pde),
  #                                              std_err = sd(pde)))
  metrics[[i]] <- dplyr::slice(models_to_run, i) |>
    dplyr::select(!model_formula) |>
    dplyr::bind_cols(metrics_i)
}
metrics <- purrr::list_rbind(metrics)
ggplot2::ggplot(metrics |>
                  dplyr::filter(code == "wwsc",
                                .metric == "rmse"),
                mapping = ggplot2::aes(mgcv_gamma, mean,
                                       group = covariate_prefix,
                                       color = covariate_prefix)) +
  ggplot2::geom_line()

metrics |>
  dplyr::filter(.metric == "rmse") |>
  dplyr::group_by(code, covariate_prefix) |>
  dplyr::summarise(rmse_min = min(mean))

metrics |>
  dplyr::filter(.metric == "rmse") |>
  dplyr::group_by(code) |>
  dplyr::summarise(min_rmse = min(mean),
                   min_covariate = covariate_prefix[which.min(mean)],
                   min_gamma = mgcv_gamma[which.min(mean)]) |>
  janitor::tabyl(min_covariate)



purrr::map(model_fit_resamples_spatial_combined,
           \(x) {
             temp <- tune::collect_extracts(x) |>
               dplyr::pull(.extracts)
             select <- temp[[1]][[2]]$fit$call$select |>
               rlang::quo_get_expr()
             gamma <- temp[[1]][[2]]$fit$call$gamma |>
               rlang::quo_get_expr()
             tibble::tibble(select = select, gamma = gamma)
           }) |>
  purrr::list_rbind()

ck <- purrr::map(model_fit_resamples_temporal_combined,
                 \(x) tune::collect_extracts(x) |>
                   dplyr::pull(.extracts))

model_fit_resamples_temporal_combined[[1]] |>
  tune::collect_extracts() |>
  dplyr::pull(.extracts)

model_fit_resamples_temporal_combined[[1]] |>
  dplyr::filter(id == "Slice1") |>
  dplyr::pull(splits) |>
  purrr::pluck(1) |>
  rsample::assessment()





tar_load_azure_store(c("data_analysis_dev", "model_workflows_combined"))
resamples <- data_analysis_dev |>
  spatialsample::spatial_block_cv(v = 2)
fit_resamples <- tune::fit_resamples(
  model_workflows_combined[[1]],
  resamples = resamples,
  control = tune::control_resamples(
    extract = function(x) list(workflows::extract_recipe(x),
                               workflows::extract_fit_parsnip(x)),
    save_pred = TRUE,
    save_workflow = TRUE
  )
)



fit_resamples |>
  dplyr::pull(.predictions) |>
  purrr::pluck(1) |>
  summary()

fit_resamples |>
  dplyr::filter(id == "Fold1") |>
  dplyr::pull(splits) |>
  purrr::pluck(1) |>
  rsample::analysis() |>
  dplyr::pull(survey_id) |>
  unique() |>
  as.character() |>
  sort()
fit_resamples |>
  dplyr::filter(id == "Fold1") |>
  dplyr::pull(splits) |>
  purrr::pluck(1) |>
  rsample::assessment() |>
  dplyr::pull(survey_id) |>
  unique() |>
  as.character() |>
  sort()


newdata <- fit_resamples |>
  dplyr::filter(id == "Fold1") |>
  dplyr::pull(splits) |>
  purrr::pluck(1) |>
  rsample::assessment() |>
  tibble::as_tibble()
newdata <- fit_resamples |>
  tune::collect_extracts() |>
  dplyr::filter(id == "Fold1") |>
  dplyr::pull(.extracts) |>
  purrr::pluck(1) |>
  purrr::pluck(1) |>
  recipes::bake(new_data = newdata)

newdata <- fit_resamples |>
  tune::collect_extracts() |>
  dplyr::filter(id == "Fold1") |>
  dplyr::pull(.extracts) |>
  purrr::pluck(1) |>
  purrr::pluck(2) |>
  predict(new_data = newdata) |>
  dplyr::bind_cols(newdata)

newdata$pred2 <- fit_resamples |>
  tune::collect_extracts() |>
  dplyr::filter(id == "Fold1") |>
  dplyr::pull(.extracts) |>
  purrr::pluck(1) |>
  purrr::pluck(2) |>
  predict(new_data = newdata, type = "raw",
          opts = list(type = "response", exclude = "s(survey_id)"))

newdata |>
  # dplyr::filter(survey_id == "WW") |>
  dplyr::select(.pred, pred2)

fit_resamples |>
  tune::collect_predictions() |>
  dplyr::filter(id == "Fold1")




fit_resamples1 <- tune::fit_resamples(
  model_workflows_combined[[1]],
  resamples = {
    temp <- dplyr::filter(resamples, id == "Fold1")
    rsample::manual_rset(temp$splits, temp$id)
  },
  control = tune::control_resamples(
    extract = function(x) list(workflows::extract_recipe(x),
                               workflows::extract_fit_parsnip(x)),
    save_pred = TRUE,
    save_workflow = TRUE
  )
)
fit_resamples2 <- tune::fit_resamples(
  model_workflows_combined[[1]],
  resamples = {
    temp <- dplyr::filter(resamples, id == "Fold2")
    rsample::manual_rset(temp$splits, temp$id)
  },
  control = tune::control_resamples(
    extract = function(x) list(workflows::extract_recipe(x),
                               workflows::extract_fit_parsnip(x)),
    save_pred = TRUE,
    save_workflow = TRUE
  )
)
dplyr::bind_rows(fit_resamples1, fit_resamples2) |>
  tune::collect_metrics()

fit_resamples |>
  tune::collect_metrics()

