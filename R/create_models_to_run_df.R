create_models_to_run_df <- function(df) {
  dplyr::select(df, sortorder, code, size_class) |>
    tidyr::expand_grid(covariate_prefix = c("hindcast", "reanalysis"),
                       basis = "tp",
                       mgcv_gamma = c(1, 2, 3),
                       spatial_random_effect = c(FALSE, TRUE)) |>
    dplyr::rowwise() |>
    dplyr::mutate(model_formula = create_model_formula_mgcv(code, type = covariate_prefix, bs = basis, spatial_random_effect = spatial_random_effect)) |>
    dplyr::ungroup() |>
    dplyr::filter(spatial_random_effect == FALSE,
                  stringr::str_starts(code, pattern = "grp_", negate = TRUE)) |>
    dplyr::slice(1:3)
}
