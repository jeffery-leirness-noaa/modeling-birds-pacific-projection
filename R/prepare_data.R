#' Prepare Bird Observation Data for Analysis
#'
#' This function projects bird observation data onto a grid, calculates survey areas,
#' and aggregates observations by cell, date, and survey.
#'
#' @param data Data frame. Bird observation data.
#' @param grid SpatRaster. The spatial grid to project data onto.
#'
#' @return A data frame of prepared bird observation data.
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
    # need to use hablar::sum_ to ensure that columns with all NAs are not changed
    # to 0s during the aggregation
    dplyr::summarise(dplyr::across(survey_area_km2_sm:wgwh, hablar::sum_)) |>
    dplyr::ungroup() |>
    dplyr::arrange(cell, date, survey_id)
  data_aggregate |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(grid)) |>
    sf::st_transform(crs = "WGS84") |>
    sf_as_df(names = c("lon", "lat")) |>
    tibble::rowid_to_column()
}

#' Prepare Environmental Covariate Data
#'
#' This function prepares environmental covariate data by renaming variables
#' with a label prefix and filtering to relevant columns.
#'
#' @param data Data frame. Environmental covariate data.
#' @param label Character string. Label to prefix variables with (e.g., "hindcast").
#'
#' @return A data frame of prepared covariate data.
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

#' Prepare Analysis Dataset
#'
#' This function combines bird observation data and environmental covariates,
#' standardizes survey IDs, and extracts additional spatial data.
#'
#' @param data_bird Data frame. Prepared bird observation data.
#' @param data_covariates List of data frames. Prepared environmental covariate data.
#' @param add List of SpatRasters. Additional spatial data to extract (e.g., bathymetry).
#'
#' @return A data frame ready for analysis.
#'
#' @examples
#' # Prepare analysis dataset with bird data and covariates
#' analysis_data <- prepare_data_analysis(
#'   data_bird_10km,
#'   data_covariates = list(data_bird_10km_wc12, data_bird_10km_wcra31),
#'   add = list(depth = data_bathy_10km, slope = data_slope_10km)
#' )
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

#' Prepare Prediction Dataset
#'
#' This function prepares a dataset for model predictions by combining grid data
#' with environmental covariates and adding standard values for required fields.
#'
#' @param data Data frame. Base grid data.
#' @param label Character string. Label of covariate data (e.g., "reanalysis").
#' @param add List of SpatRasters. Additional spatial data to extract (e.g., bathymetry).
#' @param mask Spatial object. Optional mask to apply to the data.
#'
#' @return A data frame ready for model predictions.
#'
#' @examples
#' # Prepare prediction dataset for 2010
#' prediction_data <- prepare_data_prediction(
#'   data_prediction_gfdl_2010,
#'   label = "reanalysis",
#'   add = list(depth = data_bathy_10km, slope = data_slope_10km),
#'   mask = study_polygon
#' )
prepare_data_prediction <- function(data, label, add, mask = NULL) {
  vars <- c("bbv_200", "curl", "ild_05", "ssh", "sst", "su", "sv", "sustr",
            "svstr", "eke", "chl_surf", "zoo_50m_int", "zoo_100m_int",
            "zoo_200m_int")
  r <- terra::rast(add)
  names(r) <- names(add)
  if (!is.null(mask)) {
    r <- terra::mask(r, mask)
  }
  r_df <- terra::as.data.frame(r, xy = TRUE, cells = FALSE) |>
    tibble::as_tibble()
  dplyr::left_join(data, y = r_df, by = c("x", "y")) |>
    tidyr::drop_na() |>
    dplyr::mutate(survey_id = "CAC",
                  platform = "boat",
                  survey_area_km2_sm = 100,
                  survey_area_km2_lg = survey_area_km2_sm) |>
    dplyr::rename_with(.f = ~ stringr::str_c(label, .x, sep = "_"),
                       .cols = tidyselect::any_of(vars)) |>
    dplyr::select(c(esm, cell, date, survey_id, platform, survey_area_km2_sm,
                    survey_area_km2_lg, tidyselect::starts_with(label),
                    tidyselect::all_of(names(add)), x, y))
}
