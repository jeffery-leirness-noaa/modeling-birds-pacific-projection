create_models_to_run_df <- function(df) {
  # vars <- purrr::map(purrr::set_names(c("hindcast_", "reanalysis_")),
  #                    .f = \(x) get(config$target_data_analysis) |>
  #                      tibble::as_tibble() |>
  #                      dplyr::select(tidyselect::starts_with(x)) |>
  #                      names())
  dplyr::select(df, sortorder, code, size_class) |>
    tidyr::expand_grid(covariate_prefix = c("hindcast", "reanalysis"),
                       mgcv_gamma = c(1, 2),
                       spatial_random_effect = c(FALSE, TRUE)) |>
    dplyr::rowwise() |>
    dplyr::mutate(model_formula = create_model_formula_mgcv(code, type = covariate_prefix)) |>
    dplyr::ungroup() |>
    dplyr::filter(spatial_random_effect == FALSE,
                  stringr::str_starts(code, pattern = "grp_", negate = TRUE)) |>
    dplyr::slice(1:3)
}
