#' Create a Raster from Model Predictions
#'
#' This function converts model prediction data to a SpatRaster object and
#' optionally saves it as a GeoTIFF file, with appropriate naming based on model
#' parameters.
#'
#' @param data Data frame. Model prediction data.
#' @param model_info Data frame. Information about the model.
#' @param grid SpatRaster. The spatial grid for predictions.
#' @param crop Spatial object. Optional area to crop the output to.
#' @param dir_out Character string. Directory to save the output file.
#'
#' @return Character string. Path to the saved file.
create_raster <- function(data, model_info, grid, crop = NULL, dir_out = NULL) {
  info <- dplyr::filter(model_info, model_id == unique(data$model_id))

  r_temp <- grid
  terra::values(r_temp) <- NA
  r <- purrr::map(1:12, \(x) {
    temp <- dplyr::filter(data, month == !!x)
    r_i <- r_temp
    terra::set.values(r_i, cells = temp$cell, values = temp$.mean_pred)
    names(r_i) <- month.name[x]
    r_i
  }) |>
    terra::rast()
  terra::varnames(r) <- info$code
  if (!is.null(crop)) {
    r <- terra::crop(r, y = crop, mask = TRUE)
  }

  if (is.null(dir_out)) {
    r
  } else {
    file_name <- paste(
      "model-predictions",
      info$code,
      info$covariate_prefix,
      info$basis,
      info$mgcv_gamma,
      info$spatial_effect,
      unique(data$esm),
      "monthly-climatology",
      unique(data$period),
      sep = "-"
    ) |>
      stringr::str_replace_all(pattern = '_', replacement = '-')
    file_path <- fs::path(dir_out, info$code, file_name) |>
      fs::path_ext_set(ext = ".tiff")

    fs::path_dir(file_path) |>
      fs::dir_create()
    terra::writeRaster(r, filename = file_path, overwrite = TRUE)
    as.character(file_path)
  }
}
