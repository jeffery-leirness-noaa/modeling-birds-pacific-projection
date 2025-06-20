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
    # workers = 14,
    # workers = 206,  # use when running target_model_predictions
    workers = 80,  # use when not running target_model_predictions
    seconds_idle = 30,
    garbage_collection = TRUE
  )
)


# specify data targets ----------------------------------------------------

# 10-km prediction grid
target_grid_10km <- targets::tar_target(
  grid_10km,
  command = fs::path(opt$dir_in, "grid-10km.tiff") |>
    terra::rast(),
  format = define_tar_format_terra_rast("GTiff"),
  cue = targets::tar_cue(depend = FALSE)
)

# study area polygon
target_study_polygon <- targets::tar_target(
  study_polygon,
  command = fs::path(opt$dir_in, "study-area.gpkg") |>
    sf::read_sf() |>
    sf::st_transform(crs = sf::st_crs(grid_10km)),
  cue = targets::tar_cue(depend = FALSE)
)

# shoreline polygon
target_shoreline_polygon <- targets::tar_target(
  shoreline_polygon,
  command = {
    land <- fs::path(opt$dir_in, "GSHHS_i_L1.shp") |>
      sf::read_sf()
    clip <- terra::ext(grid_10km) |>
      terra::as.polygons(crs = terra::crs(grid_10km)) |>
      sf::st_as_sf() |>
      sf::st_buffer(dist = 1e+06) |>
      sf::st_transform(crs = sf::st_crs(land))
    sf::sf_use_s2(FALSE)
    sf::st_intersection(land, clip) |>
      sf::st_transform(crs = sf::st_crs(grid_10km))
  },
  cue = targets::tar_cue(depend = FALSE)
)

# country boundary lines
target_country_lines <- targets::tar_target(
  country_lines,
  command = fs::path(opt$dir_in, "canada_us_border_wgs84.shp") |>
    sf::read_sf() |>
    sf::st_transform(crs = sf::st_crs(grid_10km)),
  # command = fs::path(opt$dir_in, "ne_10m_admin_0_boundary_lines_land.shp") |>
  #   sf::read_sf() |>
  #   sf::st_transform(crs = sf::st_crs(grid_10km)) |>
  #   dplyr::filter(TYPE == "Land"),
  cue = targets::tar_cue(depend = FALSE)
)

# state boundary lines
target_state_lines <- targets::tar_target(
  state_lines,
  command = fs::path(opt$dir_in, "state_boundaries_no_coastlines_wgs84.shp") |>
    sf::read_sf() |>
    sf::st_transform(crs = sf::st_crs(grid_10km)),
  cue = targets::tar_cue(depend = FALSE)
)

# project bathymetry layer onto 10-km grid
target_data_bathy_10km <- targets::tar_target(
  data_bathy_10km,
  command = {
    r <- fs::path(opt$dir_in, "environmental-data",
                  "gebco_2024_sub_ice_n90.0_s0.0_w-180.0_e-90.0.tiff") |>
      terra::rast()
    terra::project(r * 1, y = grid_10km)
  },
  format = define_tar_format_terra_rast("GTiff"),
  cue = targets::tar_cue(depend = FALSE)
)

# create slope raster layer
target_data_slope_10km <- targets::tar_target(
  data_slope_10km,
  command = MultiscaleDTM::SlpAsp(data_bathy_10km, w = c(3, 3),
                                  method = "queen", metrics = "slope"),
  format = define_tar_format_terra_rast("GTiff")
)

# prediction data info
target_data_prediction_metadata <- targets::tar_target(
  data_prediction_metadata,
  command = fs::path(opt$dir_in, "environmental-data") |>
    create_prediction_data_metadata(path_remove = opt$dir_in),
  cue = targets::tar_cue(depend = FALSE)
)

# prediction data (stored by year)
values_data_prediction <- tidyr::expand_grid(
  v_esm = c("gfdl", "had", "ipsl"),
  v_year = 1980:2100
)
target_data_prediction <- tarchetypes::tar_map(
  values = values_data_prediction,
  targets::tar_target(
    data_prediction,
    command = dplyr::filter(data_prediction_metadata, esm == v_esm, year == v_year) |>
      create_prediction_data_df(grid = grid_10km, path_prefix = opt$dir_in),
    cue = targets::tar_cue(depend = FALSE)
  )
)

# bird species metadata
target_data_species_info <- targets::tar_target(
  data_species_info,
  command = fs::path(opt$dir_in, "species-data", "species-info.csv") |>
    readr::read_csv(name_repair = janitor::make_clean_names),
  cue = targets::tar_cue(depend = FALSE)
)

# "raw" marine bird data
target_data_bird_raw <- targets::tar_target(
  data_bird_raw,
  command = fs::path(opt$dir_in, "species-data", "segmented-data.csv") |>
    readr::read_csv(name_repair = janitor::make_clean_names),
  cue = targets::tar_cue(depend = FALSE)
)

# project marine bird data onto 10-km grid and aggregate by <grid-cell, date, survey_id>
target_data_bird_10km <- targets::tar_target(
  data_bird_10km,
  command = prepare_data_bird(data_bird_raw, grid = grid_10km)
)

# 1980-2010 hindcast predictor data sampled at marine bird data locations and months
target_data_bird_10km_wc12 <- targets::tar_target(
  data_bird_10km_wc12,
  command = fs::path(opt$dir_in, "species-data",
                     "segmented-data-10km-daily-wc12.csv") |>
    readr::read_csv(name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "hindcast"),
  cue = targets::tar_cue(depend = FALSE)
)

# 1980-2010 reanalysis predictor data sampled at marine bird data locations and months
target_data_bird_10km_wcra31 <- targets::tar_target(
  data_bird_10km_wcra31,
  command = fs::path(opt$dir_in, "species-data",
                     "segmented-data-10km-daily-wcra31.csv") |>
    readr::read_csv(name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "reanalysis"),
  cue = targets::tar_cue(depend = FALSE)
)

# 2011-24 reanalysis predictor data sampled at marine bird data locations and months
target_data_bird_10km_wcnrt <- targets::tar_target(
  data_bird_10km_wcnrt,
  command = fs::path(opt$dir_in, "species-data",
                     "segmented-data-10km-daily-wcnrt.csv") |>
    readr::read_csv(name_repair = janitor::make_clean_names) |>
    prepare_data_covariates(label = "reanalysis"),
  cue = targets::tar_cue(depend = FALSE)
)

# mask predictor data near edges
target_data_mask <- targets::tar_target(
  data_mask,
  command = {
    r <- fs::path(opt$dir_in, data_prediction_metadata$path[1]) |>
      terra::rast() |>
      remove_raster_edges(edges = c(5, 5)) |>
      terra::project(y = grid_10km, method = "bilinear")
    r[!is.na(r)] <- 1
    r
  },
  format = define_tar_format_terra_rast("GTiff"),
  cue = targets::tar_cue(depend = FALSE)
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
    temp <- terra::extract(data_mask, data, ID = FALSE) |>
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
  command = rsample::bootstraps(data_analysis, times = 1000) |>
    dplyr::mutate(
      in_id = purrr::map(splits, \(x) x$in_id)
    ) |>
    dplyr::select(id, in_id)
)


# specify model fitting targets -------------------------------------------

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
    tibble::rowid_to_column(var = "model_id")
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
                                  rmsle,
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

# create marginal effects plots for fitted models
target_model_fit_plots <- targets::tar_target(
  model_fit_plots,
  command = create_marginal_effects_plot(
    model = model_fits,
    model_info = models_to_run,
    se = FALSE,
    dir_out = fs::path(opt$dir_processing, "output")
  ),
  pattern = map(model_fits),
  format = "file",
  iteration = "list"
)
target_model_fit_plots_se <- targets::tar_target(
  model_fit_plots_se,
  command = create_marginal_effects_plot(
    model = model_fits,
    model_info = models_to_run,
    se = TRUE,
    dir_out = fs::path(opt$dir_processing, "output")
  ),
  pattern = map(model_fits),
  format = "file",
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


# specify model prediction targets ----------------------------------------

# create predictions from fitted models
values_model_predictions <- values_data_prediction |>
  dplyr::mutate(
    v_target = glue::glue("data_prediction_{v_esm}_{v_year}") |>
      rlang::syms()
  ) |>
  dplyr::filter(v_esm == "gfdl",
                v_year %in% 1980:2017)
target_model_predictions <- tarchetypes::tar_map(
  values = values_model_predictions,
  targets::tar_target(
    model_predictions_daily,
    command = make_predictions(model_fits$.fit[[1]],
                               data = v_target,
                               label = "reanalysis",
                               add = list(depth = data_bathy_10km,
                                          slope = data_slope_10km),
                               mask = study_polygon) |>
      dplyr::mutate(model_id = model_fits$model_id, .before = 1),
    pattern = map(model_fits |> head(n = 300)),
    iteration = "list"
  ),
  targets::tar_target(
    model_predictions_monthly,
    command = dplyr::mutate(model_predictions_daily,
                            year = lubridate::year(date),
                            month = lubridate::month(date)) |>
      dplyr::group_by(model_id, esm, cell, x, y, year, month) |>
      dplyr::summarise(.ndays = dplyr::n(),
                       .mean_pred = mean(.pred)) |>
      dplyr::ungroup(),
    pattern = map(model_predictions_daily),
    iteration = "list"
  ),
  names = tidyselect::all_of(c("v_esm", "v_year"))
)

# combine/summarize predictions (i.e., create monthly "climatologies") for each model
# target_model_predictions_climatology <- values_model_predictions |>
#   dplyr::mutate(
#     v_period = dplyr::case_match(v_year,
#                                  1985:1986 ~ "0_test",
#                                  # 1985:2014 ~ "1_historical",
#                                  2035:2064 ~ "2_midcentury",
#                                  2070:2099 ~ "3_endcentury"),
#     v_target = glue::glue("model_predictions_monthly_{v_esm}_{v_year}")
#   ) |>
#   tidyr::drop_na() |>
#   tidyr::nest(.by = c(v_esm, v_period)) |>
#   dplyr::group_split(v_esm, v_period) |>
#   purrr::map(.f = \(x) create_target_model_predictions_climatology(
#     rlang::syms(x$data[[1]]$v_target),
#     v_esm = x$v_esm,
#     v_period = x$v_period)
#   )
target_model_predictions_climatology_gfdl_1_historical <- targets::tar_target(
  model_predictions_climatology_gfdl_1_historical,
  command = dplyr::bind_rows(model_predictions_monthly_gfdl_1985,
                             model_predictions_monthly_gfdl_1986,
                             model_predictions_monthly_gfdl_1987,
                             model_predictions_monthly_gfdl_1988,
                             model_predictions_monthly_gfdl_1989,
                             model_predictions_monthly_gfdl_1990,
                             model_predictions_monthly_gfdl_1991,
                             model_predictions_monthly_gfdl_1992,
                             model_predictions_monthly_gfdl_1993,
                             model_predictions_monthly_gfdl_1994,
                             model_predictions_monthly_gfdl_1995,
                             model_predictions_monthly_gfdl_1996,
                             model_predictions_monthly_gfdl_1997,
                             model_predictions_monthly_gfdl_1998,
                             model_predictions_monthly_gfdl_1999,
                             model_predictions_monthly_gfdl_2000,
                             model_predictions_monthly_gfdl_2001,
                             model_predictions_monthly_gfdl_2002,
                             model_predictions_monthly_gfdl_2003,
                             model_predictions_monthly_gfdl_2004,
                             model_predictions_monthly_gfdl_2005,
                             model_predictions_monthly_gfdl_2006,
                             model_predictions_monthly_gfdl_2007,
                             model_predictions_monthly_gfdl_2008,
                             model_predictions_monthly_gfdl_2009,
                             model_predictions_monthly_gfdl_2010,
                             model_predictions_monthly_gfdl_2011,
                             model_predictions_monthly_gfdl_2012,
                             model_predictions_monthly_gfdl_2013,
                             model_predictions_monthly_gfdl_2014) |>
    dplyr::group_by(model_id, esm, cell, x, y, month) |>
    dplyr::summarise(.mean_pred = stats::weighted.mean(.mean_pred, w = .ndays)) |>
    dplyr::ungroup() |>
    dplyr::mutate(period = "1_historical", .after = esm),
  pattern = map(model_predictions_monthly_gfdl_1985,
                model_predictions_monthly_gfdl_1986,
                model_predictions_monthly_gfdl_1987,
                model_predictions_monthly_gfdl_1988,
                model_predictions_monthly_gfdl_1989,
                model_predictions_monthly_gfdl_1990,
                model_predictions_monthly_gfdl_1991,
                model_predictions_monthly_gfdl_1992,
                model_predictions_monthly_gfdl_1993,
                model_predictions_monthly_gfdl_1994,
                model_predictions_monthly_gfdl_1995,
                model_predictions_monthly_gfdl_1996,
                model_predictions_monthly_gfdl_1997,
                model_predictions_monthly_gfdl_1998,
                model_predictions_monthly_gfdl_1999,
                model_predictions_monthly_gfdl_2000,
                model_predictions_monthly_gfdl_2001,
                model_predictions_monthly_gfdl_2002,
                model_predictions_monthly_gfdl_2003,
                model_predictions_monthly_gfdl_2004,
                model_predictions_monthly_gfdl_2005,
                model_predictions_monthly_gfdl_2006,
                model_predictions_monthly_gfdl_2007,
                model_predictions_monthly_gfdl_2008,
                model_predictions_monthly_gfdl_2009,
                model_predictions_monthly_gfdl_2010,
                model_predictions_monthly_gfdl_2011,
                model_predictions_monthly_gfdl_2012,
                model_predictions_monthly_gfdl_2013,
                model_predictions_monthly_gfdl_2014),
  iteration = "list"
)

# save summarized predictions as raster GeoTIFFs
target_model_predictions_climatology_gfdl_1_historical_rasters <- targets::tar_target(
  model_predictions_climatology_gfdl_1_historical_rasters,
  command = {
    save_raster(
      model_predictions_climatology_gfdl_1_historical,
      model_info = models_to_run,
      grid = grid_10km,
      crop = study_polygon,
      dir_out = fs::path(opt$dir_processing, "output")
    )
  },
  pattern = model_predictions_climatology_gfdl_1_historical,
  format = "file",
  iteration = "list"
)

# save summarized predictions as monthly map panel PNG files
target_model_predictions_climatology_gfdl_1_historical_maps <- targets::tar_target(
  model_predictions_climatology_gfdl_1_historical_maps,
  command = {
    create_map(
      model_predictions_climatology_gfdl_1_historical,
      model_info = models_to_run,
      species_info = data_species_info,
      land = shoreline_polygon,
      borders = list(country_lines, state_lines),
      dir_out = fs::path(opt$dir_processing, "output")
    )
  },
  pattern = model_predictions_climatology_gfdl_1_historical,
  format = "file",
  iteration = "list"
)


# specify model bootstrapping targets -------------------------------------

# fit models via bootstrap resampling
target_model_workflows_final <- targets::tar_target(
  model_workflows_final,
  command = {
    idx <- dplyr::filter(models_to_run, spatial_effect) |>
      dplyr::pull(model_id)
    dplyr::bind_rows(model_workflows) |>
      dplyr::filter(model_id %in% idx) |>
      dplyr::slice_sample(n = 3)
  }
)
target_model_fits_bootstraps <- targets::tar_target(
  model_fits_bootstraps,
  command = tibble::tibble(
    model_id = model_workflows_final$model_id,
    bootstrap_id = data_analysis_resamples_bootstrap$id,
    .fit = list(
      generics::fit(
        model_workflows_final$.workflow[[1]],
        # data = rsample::analysis(data_analysis_resamples_bootstrap$splits[[1]])
        data = data_analysis[data_analysis_resamples_bootstrap$in_id[[1]], ]
      )
    )
  ),
  pattern = cross(model_workflows_final, head(data_analysis_resamples_bootstrap, n = 4)),
  iteration = "list"
)

# create predictions from bootstrapped models
target_model_predictions_bootstraps <- tarchetypes::tar_map(
  values = values_model_predictions,
  targets::tar_target(
    model_predictions_bootstraps_daily,
    command = make_predictions(model_fits_bootstraps$.fit[[1]],
                               data = v_target,
                               label = "reanalysis",
                               add = list(depth = data_bathy_10km,
                                          slope = data_slope_10km),
                               mask = study_polygon) |>
      dplyr::mutate(model_id = model_fits_bootstraps$model_id,
                    bootstrap_id = model_fits_bootstraps$bootstrap_id,
                    .before = 1),
    pattern = map(model_fits_bootstraps),
    iteration = "list"
  ),
  targets::tar_target(
    model_predictions_bootstraps_monthly,
    command = dplyr::mutate(model_predictions_bootstraps_daily,
                            year = lubridate::year(date),
                            month = lubridate::month(date)) |>
      dplyr::group_by(model_id, bootstrap_id, esm, cell, x, y, year, month) |>
      dplyr::summarise(.ndays = dplyr::n(),
                       .mean_pred = mean(.pred)) |>
      dplyr::ungroup(),
    pattern = map(model_predictions_bootstraps_daily),
    iteration = "list"
  ),
  names = tidyselect::all_of(c("v_esm", "v_year"))
)

# combine/summarize predictions (i.e., create monthly "climatologies") for each bootstrap
target_model_predictions_bootstraps_climatology_gfdl_1_historical <- targets::tar_target(
  model_predictions_bootstraps_climatology_gfdl_1_historical,
  command = dplyr::bind_rows(model_predictions_bootstraps_monthly_gfdl_1985,
                             model_predictions_bootstraps_monthly_gfdl_1986,
                             model_predictions_bootstraps_monthly_gfdl_1987,
                             model_predictions_bootstraps_monthly_gfdl_1988,
                             model_predictions_bootstraps_monthly_gfdl_1989,
                             model_predictions_bootstraps_monthly_gfdl_1990,
                             model_predictions_bootstraps_monthly_gfdl_1991,
                             model_predictions_bootstraps_monthly_gfdl_1992,
                             model_predictions_bootstraps_monthly_gfdl_1993,
                             model_predictions_bootstraps_monthly_gfdl_1994,
                             model_predictions_bootstraps_monthly_gfdl_1995,
                             model_predictions_bootstraps_monthly_gfdl_1996,
                             model_predictions_bootstraps_monthly_gfdl_1997,
                             model_predictions_bootstraps_monthly_gfdl_1998,
                             model_predictions_bootstraps_monthly_gfdl_1999,
                             model_predictions_bootstraps_monthly_gfdl_2000,
                             model_predictions_bootstraps_monthly_gfdl_2001,
                             model_predictions_bootstraps_monthly_gfdl_2002,
                             model_predictions_bootstraps_monthly_gfdl_2003,
                             model_predictions_bootstraps_monthly_gfdl_2004,
                             model_predictions_bootstraps_monthly_gfdl_2005,
                             model_predictions_bootstraps_monthly_gfdl_2006,
                             model_predictions_bootstraps_monthly_gfdl_2007,
                             model_predictions_bootstraps_monthly_gfdl_2008,
                             model_predictions_bootstraps_monthly_gfdl_2009,
                             model_predictions_bootstraps_monthly_gfdl_2010,
                             model_predictions_bootstraps_monthly_gfdl_2011,
                             model_predictions_bootstraps_monthly_gfdl_2012,
                             model_predictions_bootstraps_monthly_gfdl_2013,
                             model_predictions_bootstraps_monthly_gfdl_2014) |>
    dplyr::group_by(model_id, bootstrap_id, esm, cell, x, y, month) |>
    dplyr::summarise(.mean_pred = stats::weighted.mean(.mean_pred, w = .ndays)) |>
    dplyr::ungroup() |>
    dplyr::mutate(period = "1_historical", .after = esm),
  pattern = map(model_predictions_bootstraps_monthly_gfdl_1985,
                model_predictions_bootstraps_monthly_gfdl_1986,
                model_predictions_bootstraps_monthly_gfdl_1987,
                model_predictions_bootstraps_monthly_gfdl_1988,
                model_predictions_bootstraps_monthly_gfdl_1989,
                model_predictions_bootstraps_monthly_gfdl_1990,
                model_predictions_bootstraps_monthly_gfdl_1991,
                model_predictions_bootstraps_monthly_gfdl_1992,
                model_predictions_bootstraps_monthly_gfdl_1993,
                model_predictions_bootstraps_monthly_gfdl_1994,
                model_predictions_bootstraps_monthly_gfdl_1995,
                model_predictions_bootstraps_monthly_gfdl_1996,
                model_predictions_bootstraps_monthly_gfdl_1997,
                model_predictions_bootstraps_monthly_gfdl_1998,
                model_predictions_bootstraps_monthly_gfdl_1999,
                model_predictions_bootstraps_monthly_gfdl_2000,
                model_predictions_bootstraps_monthly_gfdl_2001,
                model_predictions_bootstraps_monthly_gfdl_2002,
                model_predictions_bootstraps_monthly_gfdl_2003,
                model_predictions_bootstraps_monthly_gfdl_2004,
                model_predictions_bootstraps_monthly_gfdl_2005,
                model_predictions_bootstraps_monthly_gfdl_2006,
                model_predictions_bootstraps_monthly_gfdl_2007,
                model_predictions_bootstraps_monthly_gfdl_2008,
                model_predictions_bootstraps_monthly_gfdl_2009,
                model_predictions_bootstraps_monthly_gfdl_2010,
                model_predictions_bootstraps_monthly_gfdl_2011,
                model_predictions_bootstraps_monthly_gfdl_2012,
                model_predictions_bootstraps_monthly_gfdl_2013,
                model_predictions_bootstraps_monthly_gfdl_2014),
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


# submit targets ----------------------------------------------------------

list(
  target_grid_10km,
  target_study_polygon,
  target_shoreline_polygon,
  target_country_lines,
  target_state_lines,
  target_data_bathy_10km,
  target_data_slope_10km,
  target_data_prediction_metadata,
  target_data_prediction,
  target_data_species_info,
  target_data_bird_raw,
  target_data_bird_10km,
  target_data_bird_10km_wc12,
  target_data_bird_10km_wcra31,
  target_data_bird_10km_wcnrt,
  target_data_mask,
  target_data_analysis,
  target_data_analysis_resamples_spatial_5,
  target_data_analysis_resamples_spatial_10,
  target_data_analysis_resamples_bootstrap,
  target_species_to_model,
  target_models_to_run,
  target_model_metrics,
  target_model_workflows,
  target_model_fits,
  # target_model_fit_plots,
  # target_model_fit_plots_se,
  # target_model_fit_resamples_spatial_5,
  # target_model_fit_resamples_spatial_10,
  # target_model_predictions
  # target_model_predictions_climatology_gfdl_1_historical,
  # target_model_predictions_climatology_gfdl_1_historical_rasters,
  # target_model_predictions_climatology_gfdl_1_historical_maps,
  target_model_workflows_final,
  target_model_fits_bootstraps
)
