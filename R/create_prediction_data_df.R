#' Create a Prediction Data Frame from Raster Data
#'
#' This function loads spatial raster data, processes it, and converts it to a
#' data frame suitable for making predictions with models. The function handles
#' date formatting, raster edge correction, projection to a prediction grid,
#' and reshaping of data.
#'
#' @param data A data frame containing at minimum the columns:
#'   \itemize{
#'     \item \code{path}: Path to the raster file
#'     \item \code{date}: Date associated with the raster
#'     \item \code{variable}: Names to assign to raster layers
#'     \item \code{esm}: Earth System Model identifier
#'   }
#' @param grid A SpatRaster object defining the prediction grid to project onto
#' @param path_prefix Optional. A string to prefix to the paths in \code{data$path}
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{esm}: Earth System Model identifier
#'     \item \code{cell}: Cell identifiers
#'     \item \code{x}: X coordinates
#'     \item \code{y}: Y coordinates
#'     \item \code{date}: Dates
#'     \item Additional columns for each variable in the input raster
#'   }
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Loads raster data and sets time and layer names
#'   \item Removes problematic edges from the raster
#'   \item Projects the raster onto the prediction grid using bilinear interpolation
#'   \item Applies a correction (multiplication by 1) to ensure accurate values after projection
#'   \item Converts the raster to a data frame and reshapes it for prediction
#' }
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
