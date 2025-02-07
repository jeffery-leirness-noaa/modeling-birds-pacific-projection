create_models_to_run_df <- function(data) {
  dplyr::select(data, sortorder, code, size_class) |>
    tidyr::expand_grid(covariate_prefix = "reanalysis",
                       basis = "tp",
                       mgcv_gamma = 1:4,
                       spatial_random_effect = c(FALSE, TRUE)) |>
    dplyr::rowwise() |>
    dplyr::mutate(model_formula = create_model_formula_mgcv(code, type = covariate_prefix, bs = basis, spatial_random_effect = spatial_random_effect)) |>
    dplyr::ungroup()
}
