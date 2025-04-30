#' Create a Data Frame of Models to Run
#'
#' This function creates a data frame that defines all model combinations to run,
#' based on species information. It expands the input data frame with combinations
#' of covariate types, basis functions, penalty adjustment parameters, and spatial effects.
#'
#' @param data Data frame. Contains species information with columns sortorder, code, and size_class.
#'
#' @return A data frame with model specifications including formulae.
create_models_to_run_df <- function(data) {
  dplyr::select(data, sortorder, code, size_class) |>
    tidyr::expand_grid(covariate_prefix = "reanalysis",
                       basis = "tp",
                       mgcv_gamma = 1:4,
                       spatial_effect = c(FALSE, TRUE)) |>
    dplyr::rowwise() |>
    dplyr::mutate(model_formula = create_model_formula_mgcv(code, type = covariate_prefix, bs = basis, spatial_effect = spatial_effect)) |>
    dplyr::ungroup()
}
