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
    )
  )
}
targets::tar_option_set(
  packages = c("qs", "sf", "terra", "workflows"),
  format = "qs",
  repository = repository,
  memory = "transient",
  garbage_collection = TRUE,
  resources = if (targets_cas_local) NULL else resources,
  cue = targets::tar_cue(repository = FALSE),
  controller = crew::crew_controller_local(
    workers = parallel::detectCores() - 1,
    seconds_idle = 10,
    garbage_collection = TRUE
  )
)


# specify targets ---------------------------------------------------------
# 10-km prediction grid
target_grid_10km <- targets::tar_target(
  grid_10km,
  command = create_targets_data_command("grid-10km.tiff",
                                        local = targets_cas_local) |>
    eval(),
  cue = targets::tar_cue("never"),
  deployment = "main"
)

# bird species codes
target_data_species_info <- targets::tar_target(
  data_species_info,
  command = create_targets_data_command("species-data/species-info.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names),
  cue = targets::tar_cue("never")
)

# "raw" marine bird data
target_data_bird_raw <- targets::tar_target(
  data_bird_raw,
  command = create_targets_data_command("species-data/segmented-data.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names),
  cue = targets::tar_cue("never")
)

# project marine bird data onto 10-km grid and aggregate by <grid-cell, date, survey_id>
target_data_bird_10km <- targets::tar_target(
  data_bird_10km,
  command = prepare_data_bird(data_bird_raw, grid = terra::unwrap(grid_10km)),
  deployment = "main"
)

# 1980-2010 hindcast predictor data sampled at marine bird data locations and months
target_data_bird_10km_wc12 <- targets::tar_target(
  data_bird_10km_wc12,
  command = create_targets_data_command("species-data/segmented-data-10km-daily-wc12.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "hindcast"),
  cue = targets::tar_cue("never")
)

# 1980-2010 reanalysis predictor data sampled at marine bird data locations and months
target_data_bird_10km_wcra31 <- targets::tar_target(
  data_bird_10km_wcra31,
  command = create_targets_data_command("species-data/segmented-data-10km-daily-wcra31.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "reanalysis"),
  cue = targets::tar_cue("never")
)

# 2011-24 reanalysis predictor data sampled at marine bird data locations and months
target_data_bird_10km_wcnrt <- targets::tar_target(
  data_bird_10km_wcnrt,
  command = create_targets_data_command("species-data/segmented-data-10km-daily-wcnrt.csv",
                                        local = targets_cas_local) |>
    eval() |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "reanalysis"),
  cue = targets::tar_cue("never")
)

# daily gfdl climate projection data
values_gfdl <- tibble::tibble(
  source = "gfdl",
  variable = c("bbv_200",
               "chl_surf",
               "curl",
               "eke",
               "ild_05",
               "ssh",
               "sst",
               "su",
               "sustr",
               "sv",
               "svstr",
               "zoo_50m_int",
               "zoo_100m_int",
               "zoo_200m_int"),
  path = fs::path("environmental-data",
                  source,
                  stringr::str_c(variable, "_daily.nc"))
) |>
  head(n = 1)
target_data_gfdl <- tarchetypes::tar_map(
  values = values_gfdl,
  targets::tar_target(
    data_gfdl_raw,
    command = create_targets_data_command(path,
                                          local = targets_cas_local) |>
      eval(),
    deployment = "main",
    cue = targets::tar_cue("never")
  ),
  targets::tar_target(
    data_gfdl_10km,
    command = terra::project(terra::unwrap(data_gfdl_raw),
                             y = terra::unwrap(grid_10km)) |>
      terra::wrap(),
    deployment = "main"
  )
)

# "raw" bathymetry raster layer
target_data_bathy_raw <- targets::tar_target(
  data_bathy_raw,
  command = create_targets_data_command("environmental-data/gebco_2024_sub_ice_n90.0_s0.0_w-180.0_e-90.0.tiff",
                                        local = targets_cas_local) |>
    eval(),
  cue = targets::tar_cue("never"),
  deployment = "main"
)

# project bathymetry layer onto 10-km grid
target_data_bathy_10km <- targets::tar_target(
  data_bathy_10km,
  command = terra::project(terra::unwrap(data_bathy_raw),
                           y = terra::unwrap(grid_10km)) |>
    terra::wrap(),
  deployment = "main"
)

# create slope raster layer
target_data_slope_10km <- targets::tar_target(
  data_slope_10km,
  command = MultiscaleDTM::SlpAsp(terra::unwrap(data_bathy_10km), w = c(3, 3),
                                  method = "queen", metrics = "slope") |>
    terra::wrap(),
  deployment = "main"
)

# create analysis dataset
target_data_analysis <- targets::tar_target(
  data_analysis,
  command = prepare_data_analysis(
    data_bird_10km,
    data_covariates = list(data_bird_10km_wc12,
                           dplyr::bind_rows(data_bird_10km_wcra31,
                                            data_bird_10km_wcnrt)),
    add = list(depth = data_bathy_10km,
               slope = data_slope_10km)
  )
)

# subset of analysis dataset to use for development purposes
target_data_analysis_dev <- targets::tar_target(
  data_analysis_dev,
  command = rsample::initial_split(data_analysis, prop = 0.1, strata = platform) |>
    rsample::training()
)

# subset of analysis dataset to use for testing purposes
target_data_analysis_test <- targets::tar_target(
  data_analysis_test,
  command = rsample::initial_split(data_analysis, prop = 0.4, strata = platform) |>
    rsample::training()
)

# define initial data split
target_data_analysis_split <- targets::tar_target(
  data_analysis_split,
  command = rsample::initial_split(data_analysis)
)

# define spatial data resamples
target_data_analysis_resamples_spatial <- targets::tar_target(
  data_analysis_resamples_spatial,
  command = data_analysis |>
    spatialsample::spatial_block_cv(v = 5) |>
    filter_rset_data(date < "2011-01-01", .split = "assessment")
)

# define temporal data resamples
target_data_analysis_resamples_temporal <- targets::tar_target(
  data_analysis_resamples_temporal,
  command = data_analysis |>
    dplyr::filter(date < "2011-01-01") |>
    dplyr::arrange(date, survey_id) |>
    rolling_origin_prop_splits(prop = 0.15)
)

# define bootstrap resamples
target_data_analysis_resamples_bootstrap <- targets::tar_target(
  data_analysis_resamples_bootstrap,
  command = rsample::bootstraps(data_analysis, times = 10),
  deployment = "main"
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
  command = create_models_to_run_df(species_to_model) |>
    dplyr::filter(!spatial_random_effect,
                  covariate_prefix == "hindcast",
                  stringr::str_starts(code, pattern = "grp_", negate = TRUE)) |>
    dplyr::slice(1:2)
)

# define model workflows
target_model_workflows <- targets::tar_target(
  model_workflows,
  command = define_model_workflow(as.formula(models_to_run$model_formula),
                                  data = data_analysis,
                                  species_size_class = models_to_run$size_class,
                                  mgcv_select = TRUE,
                                  mgcv_gamma = models_to_run$mgcv_gamma),
  pattern = map(models_to_run),
  iteration = "list"
)

# # compare hindcast vs. reanalysis
#
#
# # testing
# target_test <- targets::tar_target(
#   test,
#   command = {workflowsets::workflow_set()}
# )
# target_model_tests <- targets::tar_target(
#   model_tests,
#   command = {},
#   pattern = workflowsets::workflow_map(),
#   iteration = "list"
# )

# fit models
target_model_fits <- targets::tar_target(
  model_fits,
  command = parsnip::fit(model_workflows, data = data_analysis_dev),
  pattern = map(model_workflows),
  iteration = "list"
)

# fit models via spatial resampling
target_model_fit_resamples_spatial <- targets::tar_target(
  model_fit_resamples_spatial,
  command = tune::fit_resamples(
    model_workflows,
    resamples = data_analysis_resamples_spatial,
    control = tune::control_resamples(
      extract = function(x) list(workflows::extract_recipe(x),
                                 workflows::extract_fit_engine(x)),
      save_workflow = TRUE
    )
  ),
  pattern = map(model_workflows),
  iteration = "list"
)

# fit models via temporal resampling
target_model_fit_resamples_temporal <- targets::tar_target(
  model_fit_resamples_temporal,
  command = tune::fit_resamples(
    model_workflows,
    resamples = data_analysis_resamples_temporal,
    control = tune::control_resamples(
      extract = function(x) list(workflows::extract_recipe(x),
                                 workflows::extract_fit_engine(x)),
      save_workflow = TRUE
    )
  ),
  pattern = map(model_workflows),
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
  target_data_species_info,
  target_data_bird_raw,
  target_data_bird_10km,
  target_data_bird_10km_wc12,
  target_data_bird_10km_wcra31,
  target_data_bird_10km_wcnrt,
  # target_data_gfdl,
  target_data_bathy_raw,
  target_data_bathy_10km,
  target_data_slope_10km,
  target_data_analysis,
  target_data_analysis_dev,
  target_data_analysis_test,
  target_data_analysis_split,
  target_data_analysis_resamples_spatial,
  target_data_analysis_resamples_temporal,
  target_data_analysis_resamples_bootstrap,
  target_species_to_model,
  target_models_to_run,
  target_model_workflows,
  target_model_fits
  # target_model_fit_resamples_spatial,
  # target_model_fit_resamples_temporal
)
