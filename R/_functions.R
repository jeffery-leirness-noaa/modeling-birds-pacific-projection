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
    dplyr::select(!seastate) |>
    dplyr::mutate(survey_id = stringr::str_split(survey_id, pattern = "_") |>
                    purrr::map_chr(.f = \(x) stringr::str_flatten(x[-length(x)], collapse = "_")),
                  # time = julian(date),
                  yday = lubridate::yday(date),
                  survey_area_km2_sm = seg_length_km * seg_width_km_sm,
                  survey_area_km2_lg = seg_length_km * seg_width_km_lg) |>
    dplyr::relocate(yday, .after = date) |>
    dplyr::relocate(anmu:wgwh, .after = tidyselect::last_col()) |>
    tibble::as_tibble() |>
    dplyr::select(!c(transect_id, segment_id, tidyselect::starts_with("seg_"), geometry)) |>
    dplyr::group_by(dplyr::pick(date:y)) |>
    dplyr::summarise(dplyr::across(survey_area_km2_sm:wgwh, sum))
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

fit_model <- function(formula, data, species_code, mgcv_select = FALSE, mgcv_gamma) {
  parsnip::gen_additive_mod(select_features = mgcv_select,
                            adjust_deg_free = mgcv_gamma) |>
    parsnip::set_engine("mgcv", family = mgcv::nb()) |>
    parsnip::set_mode("regression") |>
    parsnip::fit(formula, data = data)
}


diagnostic_plot <- function(model, se = FALSE) {
  # marginal effects plots (i.e., term plots) with standard errors
  mgcv::plot.gam(model, rug = TRUE, se = se, pages = 1, scale = 0, shade = TRUE)
}
