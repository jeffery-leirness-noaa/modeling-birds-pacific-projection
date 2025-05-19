#' Plot Marginal Effects for a GAM Model
#'
#' This function creates plots showing the marginal effects for terms in a
#' Generalized Additive Model (GAM).
#'
#' @param model A fitted GAM model object.
#' @param se Logical. Whether to show standard errors. Default is FALSE.
#'
#' @return A plot showing the marginal effects of model terms.
plot_marginal_effects <- function(model, se = FALSE) {
  mgcv::plot.gam(model, rug = TRUE, se = se, pages = 1, scale = 0, shade = TRUE)
}

create_marginal_effects_plot <- function(model, model_info, se = FALSE, dir_out) {
  info <- dplyr::filter(model_info, model_id == unique(model$model_id))

  file_name <- paste("marginal-effects-plot", info$code, info$covariate_prefix,
                     info$basis, info$mgcv_gamma, info$spatial_effect,
                     sep = "-") |>
    stringr::str_replace_all(pattern = '_', replacement = '-')
  if (se) {
    file_name <- stringr::str_replace(
      file_name,
      pattern = "marginal-effects-plot",
      replacement = "marginal-effects-plot-se"
    )
  }
  file_path <- fs::path(dir_out, info$code, file_name) |>
    fs::path_ext_set(ext = ".png")

  fs::path_dir(file_path) |>
    fs::dir_create()
  png(file_path, width = 14, height = 7, units = "in", res = 300)
  workflows::extract_fit_engine(model$.fit[[1]]) |>
    plot_marginal_effects(se = se)
  dev.off()
  as.character(file_path)
}
