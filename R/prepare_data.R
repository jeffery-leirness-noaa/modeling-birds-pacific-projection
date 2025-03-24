# need to use hablar::sum_ to ensure that columns with all NAs are not changed
# to 0s during the aggregation
prepare_data_bird <- function(data, grid) {
  data_proj <- data |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
    sf::st_transform(crs = sf::st_crs(grid))
  data_cells <- terra::extract(grid, data_proj, cells = TRUE, xy = TRUE, ID = FALSE) |>
    dplyr::select(cell, x, y) |>
    dplyr::bind_cols(data) |>
    tibble::as_tibble()
  data_tidy <- data_cells |>
    dplyr::mutate(survey_id = stringr::str_split(survey_id, pattern = "_") |>
                    purrr::map_chr(.f = \(x) stringr::str_flatten(x[-length(x)], collapse = "_")) |>
                    forcats::as_factor(),
                  survey_area_km2_sm = seg_length_km * seg_width_km_sm,
                  survey_area_km2_lg = seg_length_km * seg_width_km_lg) |>
    dplyr::relocate(anmu:wgwh, .after = tidyselect::last_col()) |>
    dplyr::select(!c(transect_id, segment_id, seg_length_km, seg_width_km_sm, seg_width_km_lg, seastate, lon, lat))
  data_aggregate <- data_tidy |>
    dplyr::group_by(dplyr::pick(cell:platform)) |>
    dplyr::summarise(dplyr::across(survey_area_km2_sm:wgwh, hablar::sum_)) |>
    dplyr::ungroup() |>
    dplyr::arrange(cell, date, survey_id)
  data_aggregate |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(grid)) |>
    sf::st_transform(crs = "WGS84") |>
    sf_as_df(names = c("lon", "lat")) |>
    tibble::rowid_to_column()
}


prepare_data_covariates <- function(data, label) {
  vars <- c("bbv_200", "curl", "ild_05", "ssh", "sst", "su", "sv", "sustr",
            "svstr", "eke", "chl_surf", "zoo_50m_int", "zoo_100m_int",
            "zoo_200m_int")
  data |>
    dplyr::rename_with(.f = ~ stringr::str_c(label, .x, sep = "_"),
                       .cols = tidyselect::any_of(vars)) |>
    dplyr::filter(distance == 0) |>
    dplyr::select(c(rowid, cell, date, survey_id,
                    tidyselect::starts_with(label)))
}


prepare_data_analysis <- function(data_bird, data_covariates, add) {
  join_by <- c("rowid", "cell", "date", "survey_id")
  data <- data_bird
  for (i in seq(along = data_covariates)) {
    data <- dplyr::left_join(data, y = data_covariates[[i]], by = join_by)
  }
  r <- terra::rast(add)
  names(r) <- names(add)
  data <- data |>
    dplyr::mutate(survey_id = stringr::str_replace(survey_id, pattern = "MAMU_zone2", replacement = "NWFP_MAMU_WA") |>
                    forcats::as_factor()) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
    sf::st_transform(crs = terra::crs(r))
  temp <- terra::extract(r, data, xy = TRUE, ID = FALSE)
  dplyr::bind_cols(data, temp) |>
    dplyr::relocate(geometry, .after = tidyselect::last_col())
}

prepare_data_prediction <- function(data, label, add) {
  vars <- c("bbv_200", "curl", "ild_05", "ssh", "sst", "su", "sv", "sustr",
            "svstr", "eke", "chl_surf", "zoo_50m_int", "zoo_100m_int",
            "zoo_200m_int")
  r <- terra::rast(add)
  names(r) <- names(add)
  r_df <- terra::as.data.frame(r, xy = TRUE, cells = TRUE) |>
    tibble::as_tibble()
  dplyr::select(data, !cell) |>
    dplyr::left_join(y = r_df, by = c("x", "y")) |>
    dplyr::mutate(survey_id = "CAC",
                  platform = "boat",
                  survey_area_km2_sm = 100,
                  survey_area_km2_lg = survey_area_km2_sm) |>
    dplyr::rename_with(.f = ~ stringr::str_c(label, .x, sep = "_"),
                       .cols = tidyselect::any_of(vars)) |>
    dplyr::select(c(cell, date, survey_id, platform, survey_area_km2_sm,
                    survey_area_km2_lg, tidyselect::starts_with(label),
                    tidyselect::all_of(names(add)), x, y))
}
