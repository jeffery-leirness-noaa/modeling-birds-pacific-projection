create_prediction_data_df <- function(data, grid, path_prefix = NULL) {
  path <- data$path
  if (!is.null(path_prefix)) {
    path <- fs::path(path_prefix, path)
  }
  r <- terra::rast(path)
  terra::time(r) <- lubridate::as_date(data$date)
  names(r) <- data$variable

  # remove problematic edges of rasters
  r <- terra::split(r, f = 1:terra::nlyr(r)) |>
    purrr::map(remove_raster_edges, edges = c(5, 5)) |>
    terra::rast()

  # project the raster onto the prediction grid
  # multiplying by 1 results in accurate projected values
  # (see https://github.com/rspatial/terra/issues/1356)
  # without this fix, some raster values were incorrect after being projected
  # the `use_gdal = FALSE` argument also resulted in correct values, but appears
  # to be slower to run
  r_proj <- terra::project(r * 1, grid, method = "bilinear")

  # convert raster to data frame and reshape for prediction
  terra::as.data.frame(r_proj, xy = TRUE, cells = TRUE, time = TRUE, wide = FALSE) |>
    tibble::as_tibble() |>
    dplyr::rename(date = time) |>
    tidyr::pivot_wider(names_from = layer, values_from = values) |>
    dplyr::mutate(esm = unique(data$esm), .before = 1)
}
