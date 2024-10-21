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
  token$refresh()
  resources <- targets::tar_resources(
    repository_cas = targets::tar_resources_repository_cas(
      envvars = c(TARGETS_AUTH_TOKEN = token$credentials$access_token)
    ))
}
targets::tar_option_set(
  packages = c("qs", "sf", "terra"),
  format = "qs",
  repository = repository,
  memory = "transient",
  garbage_collection = TRUE,
  resources = if (targets_cas_local) NULL else resources,
  cue = targets::tar_cue(repository = FALSE),
  controller = crew::crew_controller_local(workers = parallel::detectCores() - 1, seconds_idle = 10)
)


# specify targets ---------------------------------------------------------
# 10-km prediction grid
target_grid_10km <- targets::tar_target(
  grid_10km,
  command = create_targets_data_command("grid-10km.tif",
                                        local = targets_cas_local) |>
    eval()
)

# "raw" marine bird data
target_data_bird_raw <- targets::tar_target(
  data_bird_raw,
  command = create_targets_data_command("species-data/segmented-data.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names)
)

# project marine bird data onto 10-km grid and aggregate by <grid-cell, date, survey_id>
target_data_bird_10km <- targets::tar_target(
  data_bird_10km,
  command = prepare_data(data_bird_raw, grid = terra::unwrap(grid_10km))
)

# 1980-2010 hindcast predictor data sampled at marine bird data locations and months
target_data_wc12 <- targets::tar_target(
  data_wc12,
  command = create_targets_data_command("species-data/segmented-data-wc12.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names)
)

# 1980-2010 reanalysis predictor data sampled at marine bird data locations and months
target_data_wcra31 <- targets::tar_target(
  data_wcra31,
  command = create_targets_data_command("species-data/segmented-data-wcra31.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names)
)

# 2011-24 reanalysis predictor data sampled at marine bird data locations and months
target_data_wcnrt <- targets::tar_target(
  data_wcnrt,
  command = create_targets_data_command("species-data/segmented-data-wcnrt.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names)
)

# "raw" depth raster layer
target_data_depth_raw <- targets::tar_target(
  data_depth_raw,
  command = create_targets_data_command("depth-100m.tif",
                                        local = targets_cas_local) |>
    eval()
)

# create slope raster layer (100-m resolution)
target_slope_100m <- targets::tar_target(
  slope_100m,
  command = MultiscaleDTM::SlpAsp(terra::unwrap(depth_100m), w = c(3, 3),
                                  method = "queen", metrics = "slope") |>
    terra::wrap(),
  storage = "worker",
  retrieval = "worker"
)

# block average 100-m resolution depth layer to 10-km resolution
target_depth_10km <- targets::tar_target(
  depth_10km,
  command = terra::aggregate(terra::unwrap(depth_100m), fact = 100,
                             fun = "mean", na.rm = TRUE) |>
    terra::wrap(),
  storage = "worker",
  retrieval = "worker"
)

# block average 100-m resolution slope layer to 10-km resolution
target_slope_10km <- targets::tar_target(
  slope_10km,
  command = terra::aggregate(terra::unwrap(slope_100m), fact = 100,
                             fun = "mean", na.rm = TRUE) |>
    terra::wrap(),
  storage = "worker",
  retrieval = "worker"
)

# combine covariate data with marine bird data
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
    dat <- dplyr::left_join(raw_data_bird, y = hindcast, by = by) |>
      dplyr::left_join(y = reanalysis, by = by) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
    r <- list(depth_10km, slope_10km) |>
      purrr::map(.f = terra::unwrap) |>
      terra::rast()
    names(r) <- c("depth", "slope")
    dat <- sf::st_transform(dat, crs = terra::crs(r))
    temp <- terra::extract(r, dat, cells = TRUE, xy = TRUE, ID = FALSE)
    dplyr::bind_cols(dat, temp) |>
      dplyr::relocate(geometry, .after = tidyselect::last_col())
  }
)

# create analysis dataset
target_data_analysis <- targets::tar_target(
  data_analysis,
  command = prepare_data_analysis(data_covariates)
)

# subset of analysis dataset to use for development purposes
target_data_analysis_dev <- targets::tar_target(
  data_analysis_dev,
  command = data_analysis |>
    sample_data(platform, prop = 0.1)
)

# subset of analysis dataset to use for testing purposes
target_data_analysis_test <- targets::tar_target(
  data_analysis_test,
  command = data_analysis |>
    sample_data(platform, prop = 0.4)
)

# bird species codes
target_data_species_info <- targets::tar_target(
  data_species_info,
  command = create_targets_data_command("species-data/species-info.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names)
)

# create data frame of species to model
target_species_to_model <- targets::tar_target(
  species_to_model,
  command = create_species_to_model_df(data_analysis,
                                       species_info_df = data_species_info,
                                       threshold = 50)
)

# create data frame of models to run
target_models_to_run <- targets::tar_target(
  models_to_run,
  command = create_models_to_run_df(species_to_model)
)

# fit models
# target_model <- tarchetypes::tar_map(
#   values = models_to_run |>
#     dplyr::filter(spatial_random_effect == FALSE,
#                   stringr::str_starts(code, pattern = "grp_", negate = TRUE)),
#   targets::tar_target(
#     model,
#     command = {}
#   )
# )
target_model_fits <- targets::tar_target(
  model_fits,
  command = fit_model(as.formula(models_to_run$model_formula),
                      data = data_analysis_dev,
                      species_size_class = models_to_run$size_class,
                      mgcv_select = TRUE,
                      mgcv_gamma = models_to_run$mgcv_gamma),
  pattern = map(models_to_run),
  iteration = "list"
)

# create prediction rasters from fitted models
# target_model_predictions <- targets::tar_target(
#   model_predictions,
#   command = predict(model_fits, new_data = new_data, type = "raw",
#                     opts = list(type = "response", exclude = "s(survey_id)"))
# )

# submit targets ----------------------------------------------------------
list(
  target_grid_10km,
  target_data_bird_raw,
  target_data_bird_10km,
  # target_raw_data_wc12,
  # target_raw_data_wcra31,
  # target_raw_data_wcnrt,
  # target_depth_100m,
  # target_slope_100m,
  # target_depth_10km,
  # target_slope_10km,
  # target_data_covariates,
  # target_data_analysis,
  # target_data_analysis_dev,
  # target_data_analysis_test,
  target_data_species_info
  # target_species_to_model,
  # target_models_to_run,
  # target_model_fits
)
