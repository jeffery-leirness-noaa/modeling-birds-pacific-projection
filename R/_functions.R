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


prepare_data <- function(path) {
  readr::read_csv(path) |>
    # data.table::fread(path) |>
    #   tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    #   dplyr::mutate(date = lubridate::as_date(paste(year, month, day, sep = "-")),
    #                 .before = dplyr::everything()) |>
    #   dplyr::select(!c(year, month, day, season, chla:index_pdo_lag12)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
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
