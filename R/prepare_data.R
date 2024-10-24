# need to use hablar::sum_ to ensure that columns with all NAs are not changed
# to 0s during the aggregation
prepare_data <- function(data, grid) {
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


# exclude data based on distance value?
prepare_data_covariates <- function(data) {
  data |>
    dplyr::select(c(date:segment_id, tidyselect::starts_with(c("monthly_", "distance")))) |>
    dplyr::mutate(distance_use = rowMeans(dplyr::across(tidyselect::starts_with("distance")))) |>
    # dplyr::filter(distance_use <= sqrt(200)) |>
    tidyr::drop_na() |>
    dplyr::select(!tidyselect::starts_with("distance"))
}
