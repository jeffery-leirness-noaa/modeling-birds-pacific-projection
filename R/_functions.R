# custom function to load sf file
storage_read_sf <- function(container, file) {
  fl <- fs::file_temp()
  on.exit(try(fs::file_delete(fl), silent = TRUE))
  AzureStor::storage_download(container, src = file, dest = fl)
  sf::read_sf(fl)
}

# custom function to load qs file
storage_read_qs <- function(container, file) {
  fl <- fs::file_temp()
  on.exit(try(fs::file_delete(fl), silent = TRUE))
  AzureStor::storage_download(container, src = file, dest = fl)
  qs::qread(fl)
}

# custom function to load generic file
storage_read <- function(container, file, ext = NULL, ...) {
  if (is.null(ext)) ext <- fs::path_ext(file)
  if (!(ext %in% c("csv", "txt", "rds"))) {
    fl <- fs::file_temp()
    on.exit(try(fs::file_delete(fl), silent = TRUE))
    AzureStor::storage_download(container, src = file, dest = fl)
  }
  if (ext == "csv") {
    AzureStor::storage_read_csv(container, file, ...)
  } else if (ext == "txt") {
    AzureStor::storage_read_delim(container, file, ...)
  } else if (ext == "rds") {
    AzureStor::storage_load_rds(container, file)
  } else if (ext == "qs") {
    qs::qread(fl)
  } else if (ext == "gpkg") {
    sf::read_sf(fl)
  } else if (ext %in% c("tif", "tiff")) {
    terra::rast(fl) |>
      terra::wrap()
  }
}

# this will fail for .tiff files when local = TRUE!
create_targets_data_command <- function(file_name, local = TRUE) {
  if (local) {
    fs::path(opt$dir_in, !!file_name) |>
      readr::read_csv() |>
      rlang::expr()
  } else {
    token <- azure_auth_token()
    AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"), token = !!token) |>
      AzureStor::storage_container(name = "raw") |>
      storage_read(!!file_name) |>
      rlang::expr()
  }
}

# custom function to download entire directory
storage_download_dir <- function(container, src, dest) {
  is_dir <- AzureStor::list_storage_files(container) |>
    tibble::as_tibble() |>
    dplyr::filter(name == src) |>
    dplyr::pull(isdir)
  if (is_dir) {
    src <- AzureStor::list_storage_files(container, src) |>
      tibble::as_tibble() |>
      dplyr::filter(!isdir) |>
      dplyr::pull(name)
    dest <- fs::path(dest, src)
    AzureStor::storage_multidownload(container, src = src, dest = dest, recursive = TRUE)
  }
}


# exclude data based on distance value?
prepare_data_covariates <- function(.data) {
  .data |>
    dplyr::select(c(date:segment_id, tidyselect::starts_with(c("monthly_", "distance")))) |>
    dplyr::mutate(distance_use = rowMeans(dplyr::across(tidyselect::starts_with("distance")))) |>
    # dplyr::filter(distance_use == 0) |>
    tidyr::drop_na() |>
    dplyr::select(!tidyselect::starts_with("distance"))
}


prepare_data_analysis <- function(.data) {
  .data |>
    dplyr::mutate(survey_id = stringr::str_split(survey_id, pattern = "_") |>
                    purrr::map_chr(.f = \(x) stringr::str_flatten(x[-length(x)], collapse = "_")) |>
                    forcats::as_factor(),
                  # jday = julian(date),
                  # yday = lubridate::yday(date),
                  survey_area_km2_sm = seg_length_km * seg_width_km_sm,
                  survey_area_km2_lg = seg_length_km * seg_width_km_lg) |>
    # dplyr::relocate(c(jday, yday), .after = date) |>
    dplyr::relocate(anmu:wgwh, .after = tidyselect::last_col()) |>
    dplyr::select(!c(seg_length_km, seg_width_km_sm, seg_width_km_lg, seastate))
  # tibble::as_tibble() |>
  # dplyr::select(!c(transect_id, segment_id, tidyselect::starts_with("seg_"), geometry)) |>
  # dplyr::group_by(dplyr::pick(date:y)) |>
  # dplyr::summarise(dplyr::across(survey_area_km2_sm:wgwh, sum))
}


sample_data <- function(.data, ..., n = NULL, prop = NULL) {
  if (!is.null(n)) {
    # readr::read_csv(path, show_col_types = FALSE) |>
    .data |>
      dplyr::group_by(...) |>
      dplyr::slice_sample(n = n)
  } else if (!is.null(prop)) {
    # readr::read_csv(path, show_col_types = FALSE) |>
    .data |>
      dplyr::group_by(...) |>
      dplyr::slice_sample(prop = prop)
  }
}

create_species_to_model_df <- function(.data, species_codes_df, threshold) {
  dplyr::summarise(.data,
                   dplyr::across(anmu:wgwh, ~ sum(.x > 0,
                                                  na.rm = TRUE))) |>
    tibble::as_tibble() |>
    dplyr::select(!geometry) |>
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "code",
                        names_transform = list(code = stringr::str_to_upper),
                        values_to = "cells_with_sightings") |>
    dplyr::filter(cells_with_sightings >= threshold) |>
    dplyr::inner_join(y = species_codes_df, by = "code") |>
    tidyr::drop_na(sortorder) |>
    dplyr::mutate(code = stringr::str_to_lower(code),
                  size_class = stringr::str_to_lower(size_class)) |>
    dplyr::select(!marine_bird) |>
    dplyr::arrange(sortorder)
}


create_model_formula_mgcv <- function(lhs, type, bs = "tp") {
  if (type == "hindcast") {
    vars <- c("hindcast_bbv_200",
              "hindcast_curl",
              "hindcast_ild_05",
              "hindcast_ssh",
              "hindcast_sst",
              "hindcast_su",
              "hindcast_sustr",
              "hindcast_sv",
              "hindcast_svstr",
              "hindcast_zoo_200m_int",
              "hindcast_zoo_100m_int",
              "hindcast_zoo_50m_int",
              "hindcast_chl_surf",
              "hindcast_eke")
  } else if (type == "reanalysis") {
    vars <- c("reanalysis_bbv_200",
              "reanalysis_curl",
              "reanalysis_ild_05",
              "reanalysis_ssh",
              "reanalysis_sst",
              "reanalysis_su",
              "reanalysis_sustr",
              "reanalysis_sv",
              "reanalysis_svstr")
  }
  form_base <- stringr::str_glue(
    lhs,
    stringr::str_c(" ~ offset(survey_area_km2)",
                   "platform",
                   "s(survey_id, bs = \"re\")",
                   "s(date_doy, bs = \"cc\")",
                   "s(date_decimal, bs = \"{bs}\")",
                   sep = " + ")
  )
  form_vars <- stringr::str_glue("s({vars}, bs = \"{bs}\")") |>
    stringr::str_flatten(collapse = " + ")
  stringr::str_c(form_base, form_vars, sep = " + ")
}

create_models_to_run_df <- function(df) {
  # vars <- purrr::map(purrr::set_names(c("hindcast_", "reanalysis_")),
  #                    .f = \(x) get(config$target_data_analysis) |>
  #                      tibble::as_tibble() |>
  #                      dplyr::select(tidyselect::starts_with(x)) |>
  #                      names())
  dplyr::select(df, sortorder, code, size_class) |>
    tidyr::expand_grid(covariate_prefix = c("hindcast", "reanalysis"),
                       mgcv_gamma = c(1, 2),
                       spatial_random_effect = c(FALSE, TRUE)) |>
    dplyr::rowwise() |>
    dplyr::mutate(model_formula = create_model_formula_mgcv(code, type = covariate_prefix)) |>
    dplyr::ungroup() |>
    dplyr::filter(spatial_random_effect == FALSE,
                  stringr::str_starts(code, pattern = "grp_", negate = TRUE)) |>
    dplyr::slice(1:3)
}


fit_model <- function(model_formula, data, species_size_class, mgcv_select = FALSE, mgcv_gamma = NULL) {

  survey_area_var <- stringr::str_c("survey_area_km2_", species_size_class)

  lhs <- formula.tools::lhs(model_formula) |>
    as.character()
  op <- formula.tools::op(model_formula) |>
    as.character()
  rhs <- formula.tools::rhs(model_formula) |>
    all.vars() |>
    stringr::str_replace_all(c("survey_area_km2" = survey_area_var,
                               "date_doy" = "date",
                               "date_decimal" = "date")) |>
    unique() |>
    stringr::str_flatten(collapse = " + ")
  preproc_formula <- stringr::str_c(lhs, op, rhs, sep = " ") |>
    as.formula()

  gam_recipe <- recipes::recipe(preproc_formula, data = tibble::as_tibble(data)) |>
    recipes::step_date(date, features = c("doy", "decimal")) |>
    recipes::step_rm(date) |>
    recipes::step_rename(survey_area_km2 = !!survey_area_var) |>
    recipes::step_log(survey_area_km2) |>
    recipes::step_center(survey_area_km2) |>
    recipes::step_normalize(recipes::all_numeric_predictors(), -survey_area_km2)

  # recipes::prep(gam_recipe) |>
  #   recipes::bake(new_data = NULL)

  gam_model <- parsnip::gen_additive_mod(select_features = mgcv_select,
                                         adjust_deg_free = mgcv_gamma) |>
    parsnip::set_engine("mgcv", family = mgcv::nb()) |>
    parsnip::set_mode("regression")

  gam_workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe = gam_recipe) |>
    workflows::add_model(gam_model, formula = model_formula)

  parsnip::fit(gam_workflow, data = data)

}


diagnostic_plot <- function(model, se = FALSE) {
  # marginal effects plots (i.e., term plots) with standard errors
  mgcv::plot.gam(model, rug = TRUE, se = se, pages = 1, scale = 0, shade = TRUE)
}
