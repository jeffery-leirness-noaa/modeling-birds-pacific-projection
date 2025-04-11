save_raster <- function(data, model_info, grid, crop = NULL, dir_out) {
  info <- dplyr::filter(model_info, model_id == unique(data$model_id))

  r_temp <- grid_10km
  values(r_temp) <- NA
  r <- purrr::map(1:12, \(x) {
    temp <- dplyr::filter(data, month == x)
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

  file_name <- paste("model-predictions", info$code, info$covariate_prefix,
                     info$basis, info$mgcv_gamma, info$spatial_effect,
                     unique(data$esm), "monthly-climatology",
                     unique(data$period), sep = "-") |>
    stringr::str_replace_all(pattern = '_', replacement = '-')
  file_path <- fs::path(info$code, file_name) |>
    fs::path_ext_set(ext = ".tiff")

  terra::writeRaster(r, filename = fs::path(dir_out, file_path))
  fs::path(dir_out, file_path) |>
    as.character()
}
