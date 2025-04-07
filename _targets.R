# source targets helper file
if (fs::file_exists("_targets_helper.R")) {
  source("_targets_helper.R")
}

# source R scripts in the R/ folder
targets::tar_source()

# set target options
targets::tar_option_set(
  packages = c("qs2", "rsample", "sf", "spatialsample", "terra", "workflows"),
  format = "qs",
  error = "abridge",
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  controller = crew::crew_controller_local(
    workers = 95,
    seconds_idle = 30,
    garbage_collection = TRUE
  )
)


# specify targets ---------------------------------------------------------

# 10-km prediction grid
target_grid_10km_file <- targets::tar_target(
  grid_10km_file,
  command = fs::path(opt$dir_in, "grid-10km.tiff"),
  format = "file"
)
target_grid_10km <- targets::tar_target(
  grid_10km,
  command = terra::rast(grid_10km_file),
  format = define_tar_format_terra_rast("GTiff")
)

# study area polygon
target_study_polygon_file <- targets::tar_target(
  study_polygon_file,
  command = fs::path(opt$dir_in, "study-area.gpkg"),
  format = "file"
)
target_study_polygon <- targets::tar_target(
  study_polygon,
  command = sf::read_sf(study_polygon_file) |>
    sf::st_transform(crs = sf::st_crs(grid_10km))
)

# bird species codes
target_data_species_info_file <- targets::tar_target(
  data_species_info_file,
  command = fs::path(opt$dir_in, "species-data", "species-info.csv"),
  format = "file"
)
target_data_species_info <- targets::tar_target(
  data_species_info,
  command = readr::read_csv(data_species_info_file) |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names)
)

# "raw" marine bird data
target_data_bird_raw_file <- targets::tar_target(
  data_bird_raw_file,
  command = fs::path(opt$dir_in, "species-data", "segmented-data.csv"),
  format = "file"
)
target_data_bird_raw <- targets::tar_target(
  data_bird_raw,
  command = readr::read_csv(data_bird_raw_file) |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names)
)

# project marine bird data onto 10-km grid and aggregate by <grid-cell, date, survey_id>
target_data_bird_10km <- targets::tar_target(
  data_bird_10km,
  command = prepare_data_bird(data_bird_raw, grid = grid_10km)
)

# 1980-2010 hindcast predictor data sampled at marine bird data locations and months
target_data_bird_10km_wc12_file <- targets::tar_target(
  data_bird_10km_wc12_file,
  command = fs::path(opt$dir_in, "species-data",
                     "segmented-data-10km-daily-wc12.csv"),
  format = "file"
)
target_data_bird_10km_wc12 <- targets::tar_target(
  data_bird_10km_wc12,
  command = readr::read_csv(data_bird_10km_wc12_file) |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "hindcast")
)

# 1980-2010 reanalysis predictor data sampled at marine bird data locations and months
target_data_bird_10km_wcra31_file <- targets::tar_target(
  data_bird_10km_wcra31_file,
  command = fs::path(opt$dir_in, "species-data",
                     "segmented-data-10km-daily-wcra31.csv"),
  format = "file"
)
target_data_bird_10km_wcra31 <- targets::tar_target(
  data_bird_10km_wcra31,
  command = readr::read_csv(data_bird_10km_wcra31_file) |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "reanalysis")
)

# 2011-24 reanalysis predictor data sampled at marine bird data locations and months
target_data_bird_10km_wcnrt_file <- targets::tar_target(
  data_bird_10km_wcnrt_file,
  command = fs::path(opt$dir_in, "species-data",
                     "segmented-data-10km-daily-wcnrt.csv"),
  format = "file"
)
target_data_bird_10km_wcnrt <- targets::tar_target(
  data_bird_10km_wcnrt,
  command = readr::read_csv(data_bird_10km_wcnrt_file) |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "reanalysis")
)

# mask predictor data near edges
target_data_climate_mask_file <- targets::tar_target(
  data_climate_mask_file,
  command = fs::path(opt$dir_in, "environmental-data", "gfdl", "sst_daily.nc"),
  format = "file"
)
target_data_climate_mask <- targets::tar_target(
  data_climate_mask,
  command = {
    r <- terra::rast(data_climate_mask_file) |>
      terra::subset(subset = 1)
    mat <- terra::as.matrix(r, wide = TRUE)
    mat[c(1:5, (nrow(mat) - 4):nrow(mat)), ] <- NA
    mat[, 1:5] <- NA
    terra::values(r) <- mat
    r_proj <- terra::project(r, y = grid_10km)
    r_proj[!is.na(r_proj)] <- 1
    r_proj
  },
  format = define_tar_format_terra_rast("GTiff")
)

# project bathymetry layer onto 10-km grid
target_data_bathy_10km_file <- targets::tar_target(
  data_bathy_10km_file,
  command = fs::path(opt$dir_in, "environmental-data",
                     "gebco_2024_sub_ice_n90.0_s0.0_w-180.0_e-90.0.tiff"),
  format = "file"
)
target_data_bathy_10km <- targets::tar_target(
  data_bathy_10km,
  command = terra::rast(data_bathy_10km_file) |>
    terra::project(y = grid_10km),
  format = define_tar_format_terra_rast("GTiff")
)

# create slope raster layer
target_data_slope_10km <- targets::tar_target(
  data_slope_10km,
  command = MultiscaleDTM::SlpAsp(data_bathy_10km, w = c(3, 3),
                                  method = "queen", metrics = "slope"),
  format = define_tar_format_terra_rast("GTiff")
)

# create analysis dataset
target_data_analysis <- targets::tar_target(
  data_analysis,
  command = {
    data <- prepare_data_analysis(
      data_bird_10km,
      data_covariates = list(data_bird_10km_wc12,
                             dplyr::bind_rows(data_bird_10km_wcra31,
                                              data_bird_10km_wcnrt)),
      add = list(depth = data_bathy_10km,
                 slope = data_slope_10km)
    )
    temp <- terra::extract(data_climate_mask, data, ID = FALSE) |>
      dplyr::pull()
    data[!is.na(temp), ]
  }
)

# define spatial data resamples
target_data_analysis_resamples_spatial_5 <- targets::tar_target(
  data_analysis_resamples_spatial_5,
  command = data_analysis |>
    spatialsample::spatial_block_cv(v = 5)
)
target_data_analysis_resamples_spatial_10 <- targets::tar_target(
  data_analysis_resamples_spatial_10,
  command = data_analysis |>
    spatialsample::spatial_block_cv(v = 10)
)

# define bootstrap resamples
target_data_analysis_resamples_bootstrap <- targets::tar_target(
  data_analysis_resamples_bootstrap,
  command = rsample::bootstraps(data_analysis, times = 1000)
)

# create data frame of species to model
target_species_to_model <- targets::tar_target(
  species_to_model,
  command = create_species_to_model_df(data_analysis,
                                       species_info_df = data_species_info,
                                       threshold = 50) |>
    dplyr::filter(stringr::str_starts(code, pattern = "grp_", negate = TRUE))
)

# create data frame of models to run
target_models_to_run <- targets::tar_target(
  models_to_run,
  command = create_models_to_run_df(species_to_model) |>
    tibble::rowid_to_column(var = "model_id") |>
    dplyr::filter(!spatial_effect,
                  stringr::str_starts(code, pattern = "grp_", negate = TRUE))
)

# define model metrics
target_model_metrics <- targets::tar_target(
  model_metrics,
  command = yardstick::metric_set(yardstick::ccc,
                                  yardstick::huber_loss,
                                  yardstick::huber_loss_pseudo,
                                  yardstick::iic,
                                  yardstick::mae,
                                  yardstick::mape,
                                  yardstick::mase,
                                  yardstick::mpe,
                                  yardstick::msd,
                                  yardstick::poisson_log_loss,
                                  yardstick::rmse,
                                  yardstick::rpd,
                                  yardstick::rpiq,
                                  yardstick::rsq,
                                  yardstick::rsq_trad,
                                  yardstick::smape)
)

# define model workflows
target_model_workflows <- targets::tar_target(
  model_workflows,
  command = tibble::tibble(model_id = models_to_run$model_id,
                           .workflow = list(
                             define_model_workflow(
                               as.formula(models_to_run$model_formula),
                               data = data_analysis,
                               species_size_class = models_to_run$size_class,
                               mgcv_select = TRUE,
                               mgcv_gamma = models_to_run$mgcv_gamma
                             ))),
  pattern = map(models_to_run),
  iteration = "list"
)

# example of how to subset a dynamic branch
# target_test_combine <- targets::tar_target(
#   test_combine,
#   command = {
#     ids <- dplyr::filter(models_to_run, code %in% c("comu", "bfal", "pfsh")) |>
#       dplyr::pull(model_id)
#     dplyr::bind_rows(model_workflows) |>
#       dplyr::filter(model_id %in% ids)
#   }
# )

# fit models
target_model_fits <- targets::tar_target(
  model_fits,
  command = tibble::tibble(model_id = model_workflows$model_id,
                           .fit = list(
                             generics::fit(model_workflows$.workflow[[1]],
                                           data = data_analysis)
                           )),
  pattern = map(model_workflows),
  iteration = "list"
)

# fit models via 5-fold spatial resampling
target_model_fit_resamples_spatial_5 <- targets::tar_target(
  model_fit_resamples_spatial_5,
  command = tibble::tibble(
    model_id = model_workflows$model_id,
    .fit = list(
      tune::fit_resamples(
        model_workflows$.workflow[[1]],
        resamples = data_analysis_resamples_spatial_5,
        metrics = model_metrics,
        control = tune::control_resamples(
          extract = function(x) list(workflows::extract_recipe(x),
                                     workflows::extract_fit_parsnip(x)),
          save_pred = TRUE,
          save_workflow = TRUE
        )
      )
    )
  ),
  pattern = map(model_workflows),
  iteration = "list"
)

# fit models via 10-fold spatial resampling
target_model_fit_resamples_spatial_10 <- targets::tar_target(
  model_fit_resamples_spatial_10,
  command = tibble::tibble(
    model_id = model_workflows$model_id,
    .fit = list(
      tune::fit_resamples(
        model_workflows$.workflow[[1]],
        resamples = data_analysis_resamples_spatial_10,
        metrics = model_metrics,
        control = tune::control_resamples(
          extract = function(x) list(workflows::extract_recipe(x),
                                     workflows::extract_fit_parsnip(x)),
          save_pred = TRUE,
          save_workflow = TRUE
        )
      )
    )
  ),
  pattern = map(model_workflows),
  iteration = "list"
)

# create prediction datasets (by year)
values_data_prediction <- tidyr::expand_grid(
  v_esm = c("gfdl", "hadl", "ipsl"),
  v_year = 1980:2100
)
target_data_prediction <- tarchetypes::tar_map(
  values = values_data_prediction,
  targets::tar_target(
    data_prediction_file,
    command = fs::path(opt$dir_processing, "environmental-data", v_esm,
                       paste0(v_esm, "_daily_pcs_", v_year, ".qs")),
    format = "file"
  ),
  targets::tar_target(
    data_prediction,
    command = qs2::qs_read(data_prediction_file),
  )
)

# create predictions from fitted models
values_model_predictions <- values_data_prediction |>
  dplyr::mutate(
    v_target = glue::glue("data_prediction_{v_esm}_{v_year}") |>
      rlang::syms()
  ) |>
  dplyr::filter(v_esm == "gfdl") |>
  dplyr::slice(26:35)
target_model_predictions <- tarchetypes::tar_map(
  values = values_model_predictions,
  targets::tar_target(
    model_predictions_daily,
    command = {
      new_data <- prepare_data_prediction(v_target,
                                          label = "reanalysis",
                                          add = list(depth = data_bathy_10km,
                                                     slope = data_slope_10km),
                                          mask = study_polygon)
      pred <- predict(model_fits$.fit[[1]], new_data = new_data, type = "raw",
                      opts = list(type = "response", exclude = "s(survey_id)")) |>
        tibble::as_tibble() |>
        dplyr::rename(.pred = value)
      dplyr::select(new_data, cell, date) |>
        dplyr::bind_cols(pred) |>
        dplyr::mutate(model_id = model_fits$model_id, .before = 1)
    },
    pattern = map(model_fits),
    iteration = "list"
  ),
  targets::tar_target(
    model_predictions_monthly,
    command = dplyr::mutate(model_predictions_daily,
                            year = lubridate::year(date),
                            month = lubridate::month(date)) |>
      dplyr::group_by(model_id, cell, year, month) |>
      dplyr::summarise(.ndays = dplyr::n(),
                       .mean_pred = mean(.pred)) |>
      dplyr::ungroup(),
    pattern = map(model_predictions_daily),
    iteration = "list"
  ),
  names = tidyselect::all_of(c("v_esm", "v_year"))
)


# submit targets ----------------------------------------------------------

list(
  target_grid_10km_file,
  target_grid_10km,
  target_study_polygon_file,
  target_study_polygon,
  target_data_species_info_file,
  target_data_species_info,
  target_data_bird_raw_file,
  target_data_bird_raw,
  target_data_bird_10km,
  target_data_bird_10km_wc12_file,
  target_data_bird_10km_wc12,
  target_data_bird_10km_wcra31_file,
  target_data_bird_10km_wcra31,
  target_data_bird_10km_wcnrt_file,
  target_data_bird_10km_wcnrt,
  target_data_climate_mask_file,
  target_data_climate_mask,
  target_data_bathy_10km_file,
  target_data_bathy_10km,
  target_data_slope_10km,
  target_data_analysis,
  target_data_analysis_resamples_spatial_5,
  target_data_analysis_resamples_spatial_10,
  target_data_analysis_resamples_bootstrap,
  target_species_to_model,
  target_models_to_run,
  target_model_metrics,
  target_model_workflows,
  target_model_fits,
  # target_model_fit_resamples_spatial_5,
  # target_model_fit_resamples_spatial_10,
  target_data_prediction
  # target_model_predictions
  # target_model_predictions_climatology
)
