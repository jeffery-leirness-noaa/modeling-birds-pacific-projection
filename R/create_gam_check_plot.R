create_gam_check_plot <- function(model, model_info, dir_out) {
  info <- dplyr::filter(model_info, model_id == unique(model$model_id))

  file_name <- paste("gam-check-plot", info$code, info$covariate_prefix,
                     info$basis, info$mgcv_gamma, info$spatial_effect,
                     sep = "-") |>
    stringr::str_replace_all(pattern = '_', replacement = '-')
  file_path <- fs::path(dir_out, info$code, file_name) |>
    fs::path_ext_set(ext = ".png")

  fs::path_dir(file_path) |>
    fs::dir_create()
  png(file_path, width = 14, height = 7, units = "in", res = 300)
  workflows::extract_fit_engine(model$.fit[[1]]) |>
    mgcv::gam.check()
  dev.off()
  as.character(file_path)
}
