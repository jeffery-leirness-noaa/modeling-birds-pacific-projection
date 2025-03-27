# source targets helper file
if (fs::file_exists("_targets_helper.R")) {
  source("_targets_helper.R")
}

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
  packages = c("qs", "qs2", "rsample", "sf", "spatialsample", "terra", "workflows"),
  format = "qs",
  repository = repository,
  error = "abridge",
  memory = "transient",
  garbage_collection = TRUE,
  resources = if (targets_cas_local) NULL else resources,
  storage = "worker",
  retrieval = "worker",
  cue = targets::tar_cue(repository = FALSE),
  controller = crew::crew_controller_local(
    workers = 24,
    seconds_idle = 30,
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
  format = define_tar_format_terra_rast("GTiff"),
  cue = targets::tar_cue("never")
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
  command = prepare_data_bird(data_bird_raw, grid = grid_10km)
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

# mask predictor data near edges
target_data_climate_mask <- targets::tar_target(
  data_climate_mask,
  command = {
    r <- create_targets_data_command(
      fs::path("environmental-data", "gfdl", "sst_daily.nc"),
      local = targets_cas_local
    ) |>
      eval() |>
      terra::subset(subset = 1)
    mat <- terra::as.matrix(r, wide = TRUE)
    mat[c(1:5, (nrow(mat) - 4):nrow(mat)), ] <- NA
    mat[, 1:5] <- NA
    terra::values(r) <- mat
    r_proj <- terra::project(r, y = grid_10km)
    r_proj[!is.na(r_proj)] <- 1
    r_proj
  },
  format = define_tar_format_terra_rast("GTiff"),
  cue = targets::tar_cue("never")
)

# project bathymetry layer onto 10-km grid
target_data_bathy_10km <- targets::tar_target(
  data_bathy_10km,
  command = create_targets_data_command(
    "environmental-data/gebco_2024_sub_ice_n90.0_s0.0_w-180.0_e-90.0.tiff",
    local = targets_cas_local
  ) |>
    eval() |>
    terra::project(y = grid_10km),
  format = define_tar_format_terra_rast("GTiff"),
  cue = targets::tar_cue("never")
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
    data_prediction,
    command = create_prediction_dataset(fs::path("environmental-data", v_esm),
                                        v_year),
    cue = targets::tar_cue("never")
  )
)
# target_data_prediction_year <- tarchetypes::tar_map(
#   values = values_data_prediction_year,
#   targets::tar_target(
#     data_prediction,
#     command = create_prediction_dataset(fs::path("environmental-data", esm),
#                                         year),
#     cue = targets::tar_cue("never")
#   ),
#   target_data_prediction_group_date <- tarchetypes::tar_group_by(
#     data_prediction_group_date,
#     command = data_prediction,
#     date
#   ),
#   target_data_prediction_date <- targets::tar_target(
#     data_prediction_date,
#     command = data_prediction_group_date,
#     pattern = map(data_prediction_group_date) |>
#       head(n = 3)
#   )
# )

# create prediction datasets (by date)
# values_data_prediction_date <- tidyr::expand_grid(
#   esm = c("gfdl", "hadl", "ipsl"),
#   vdate = (lubridate::as_date("1980-01-01"):lubridate::as_date("2100-12-31")) |>
#     lubridate::as_date()
# ) |>
#   dplyr::mutate(
#     year = lubridate::year(vdate),
#     month = lubridate::month(vdate),
#     day = lubridate::day(vdate),
#     target_name_in = rlang::syms(glue::glue("data_prediction_{esm}_{year}"))
#   )
# # target_data_prediction_date <- tarchetypes::tar_map(
# #   values = values_data_prediction_date |>
# #     head(n = 5),
# #   targets::tar_target(
# #     data_prediction,
# #     command = dplyr::filter(target_name_in, date == vdate)
# #   ),
# #   names = tidyselect::all_of(c("esm", "year", "month", "day"))
# # )
# target_data_prediction_group <- tarchetypes::tar_group_by(
#   data_prediction_group,
#   command = data_prediction_gfdl_1980,
#   date
# )
# target_data_prediction_date <- targets::tar_target(
#   data_prediction_date,
#   command = data_prediction_group,
#   pattern = map(data_prediction_group) |>
#     head(n = 5)
# )
# name_data <- paste0("data_prediction_gfdl_1980_1_", 1:3)
# sym_data <- rlang::syms(name_data)
# command_test <- substitute(dplyr::bind_rows(data), env = list(data = sym_data))
# target_test <- targets::tar_target_raw(
#   "test_combine",
#   # command = list(data_prediction_gfdl_1980_1_1,
#   #                data_prediction_gfdl_1980_1_2,
#   #                data_prediction_gfdl_1980_1_3) |>
#   #   dplyr::bind_rows()
#   # command = paste0("data_prediction_gfdl_1980_1_", 1:3) |>
#   #   rlang::syms() |>
#   #   dplyr::bind_rows()
#   # command = paste0("data_prediction_gfdl_1980_1_", 1:3) |>
#   #   targets::tar_read_raw() |>
#   #   dplyr::bind_rows()
#   command = command_test
# )

# create prediction rasters from fitted models
# values_model_predictions <- values_data_prediction |>
#   dplyr::mutate(
#     target = rlang::syms(glue::glue("data_prediction_{esm}_{year}"))
#   ) |>
#   dplyr::filter(esm == "gfdl", year == 1980)
# # target_model_predictions <- targets::tar_target(
# #   model_predictions,
# #   command = {
# #     new_data <- prepare_data_prediction(data_prediction_gfdl_2000,
# #                                         label = "reanalysis",
# #                                         add = list(depth = data_bathy_10km,
# #                                                    slope = data_slope_10km))
# #     pred <- predict(model_fits, new_data = new_data, type = "raw",
# #                     opts = list(type = "response", exclude = "s(survey_id)")) |>
# #       tibble::as_tibble() |>
# #       dplyr::rename(.pred = value)
# #   },
# #   pattern = map(model_fits) |>
# #     head(n = 1),
# #   iteration = "list"
# # )
# target_model_predictions <- tarchetypes::tar_map(
#   values = values_model_predictions,
#   targets::tar_target(
#     model_predictions,
#     command = {
#       new_data <- prepare_data_prediction(target,
#                                           label = "reanalysis",
#                                           add = list(depth = data_bathy_10km,
#                                                      slope = data_slope_10km))
#       pred <- predict(model_fits, new_data = new_data, type = "raw",
#                       opts = list(type = "response", exclude = "s(survey_id)")) |>
#         tibble::as_tibble() |>
#         dplyr::rename(.pred = value)
#     },
#     pattern = map(model_fits) |>
#       head(n = 2),
#     iteration = "list"
#   ),
#   names = tidyselect::all_of()
# )

# create predictions from fitted models
values_model_predictions <- values_data_prediction |>
  dplyr::mutate(
    target_name = rlang::syms(glue::glue("data_prediction_{.esm}_{.year}"))
  )
target_model_predictions <- tarchetypes::tar_map(
  values = values_model_predictions,
  targets::tar_target(
    model_predictions_daily,
    command = {
      new_data <- prepare_data_prediction(target_name,
                                          label = "reanalysis",
                                          add = list(depth = data_bathy_10km,
                                                     slope = data_slope_10km))
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
  names = tidyselect::all_of(c(".esm", ".year"))
)

# combine/summarize predictions (i.e., create monthly "climatologies") for each model
values_model_predictions_climatology <- values_data_prediction |>
  dplyr::mutate(v_period = dplyr::case_match(
    .year,
    1980:1982 ~ "0_test",
    1986:2015 ~ "1_historical",
    2036:2065 ~ "2_midcentury",
    2071:2100 ~ "3_endcentury"
  )) |>
  dplyr::mutate(
    v_esm = .esm,
    v_year = .year,
    v_target = stringr::str_c("model_predictions_monthly_gfdl_", v_year)
  ) |>
  dplyr::group_by(v_esm, v_period) |>
  dplyr::summarise(v_string = stringr::str_flatten_comma(v_target)) |>
  dplyr::mutate(
    v_command = stringr::str_c("dplyr::bind_rows(", v_string, ")") |>
      stringr::str_c("dplyr::group_by(model_id, cell, month)",
                     "dplyr::summarise(.mean_pred = stats::weighted.mean(.mean_pred, w = .ndays))",
                     "dplyr::ungroup()",
                     sep = " |> "),
    v_pattern = stringr::str_c("map(", v_string, ")")
  )
# summary_years <- 1980:1982
# target_names <- stringr::str_c("model_predictions_monthly_gfdl_", summary_years)
# command <- stringr::str_c(
#   "dplyr::bind_rows(",
#   stringr::str_flatten_comma(target_names),
#   ") |>
#   dplyr::group_by(model_id, cell, month) |>
#   dplyr::summarise(.mean_pred = stats::weighted.mean(.mean_pred, w = .ndays)) |>
#   dplyr::ungroup()"
# )
# pattern <- stringr::str_c(
#   "map(",
#   stringr::str_flatten_comma(target_names),
#   ")"
# )
v_command <- values_model_predictions_climatology$v_command
v_pattern <- values_model_predictions_climatology$v_pattern
target_model_predictions_climatology <- targets::tar_target_raw(
  "model_predictions_climatology",
  command = rlang::parse_expr(v_command),
  pattern = rlang::parse_expr(v_pattern),
  iteration = "list"
)
# target_model_predictions_climatology <- tarchetypes::tar_map(
#   values = values_model_predictions_climatology,
#   targets::tar_target_raw(
#     "model_predictions_climatology",
#     command = rlang::parse_expr(v_command),
#     pattern = rlang::parse_expr(v_pattern),
#     iteration = "list"
#   ),
#   # target_model_predictions_climatology_output <- targets::tar_target(
#   #   model_predictions_climatology_output,
#   #   command = {
#   #     m_id <- unique(model_predictions_climatology$model_id)
#   #     info <- dplyr::filter(models_to_run, model_id == m_id)
#   #
#   #     file_name <- paste0(
#   #       "model-predictions-",
#   #       paste(info$code, info$covariate_prefix, info$basis, info$mgcv_gamma,
#   #             info$spatial_effect, sep = "-"),
#   #       glue::glue("-{v_esm}-monthly-climatology-{v_period}.tiff")) |>
#   #       stringr::str_replace_all(pattern = '_', replacement = '-')
#   #     file_path <- fs::path(info$code, file_name)
#   #     r_temp <- grid_10km
#   #     values(r_temp) <- NA
#   #     r <- purrr::map(1:12, \(x) {
#   #       temp <- dplyr::filter(model_predictions_climatology, month == x)
#   #       r_i <- r_temp
#   #       r_i[temp$cell] <- temp$.mean_pred
#   #       names(r_i) <- month.name[x]
#   #       r_i
#   #     }) |>
#   #       terra::rast()
#   #     terra::varnames(r) <- info$code
#   #
#   #     if (local) {
#   #       terra::writeRaster(r, filename = fs::path(opt$dir_output, file_path))
#   #       fs::path(!!opt$dir_output, file_path) |>
#   #         as.character()
#   #     } else {
#   #       fl <- fs::file_temp() |>
#   #         paste0(".tiff")
#   #       terra::writeRaster(r, filename = fl)
#   #       AzureStor::upload_to_url(fl,
#   #                                dest = paste(Sys.getenv("TARGETS_ENDPOINT"),
#   #                                             "output",
#   #                                             file_path,
#   #                                             sep = "/"),
#   #                                token = azure_auth_token())
#   #     }
#   #   },
#   #   pattern = model_predictions_climatology,
#   #   format = "file",
#   #   iteration = "list"
#   # ),
#   names = tidyselect::all_of(c("v_esm", "v_period"))
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
  target_data_climate_mask,
  target_data_bathy_10km,
  target_data_slope_10km,
  target_data_analysis,
  target_data_analysis_resamples_spatial_5,
  target_data_analysis_resamples_spatial_10,
  target_data_analysis_resamples_bootstrap,
  target_species_to_model,
  target_models_to_run,
  target_model_metrics,
  target_model_workflows
  # target_model_fits,
  # target_model_fit_resamples_spatial_5,
  # target_model_fit_resamples_spatial_10,
  # target_data_prediction,
  # target_model_predictions,
  # target_model_predictions_climatology
)
