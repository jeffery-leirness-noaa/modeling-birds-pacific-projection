#' Create a Target for Summarizing Model Predictions into Monthly Climatologies
#'
#' This function creates a {targets} pipeline target that combines model predictions
#' and calculates monthly climatologies.
#'
#' @param .targets List of targets. The model prediction targets to combine.
#' @param v_esm Character string. The Earth System Model used for predictions.
#' @param v_period Character string. The time period of the climatology.
#'
#' @return A targets pipeline target object.
create_target_model_predictions_climatology  <- function(.targets, v_esm, v_period) {
  targets::tar_target_raw(
    glue::glue("model_predictions_climatology_{v_esm}_{v_period}"),
    command = dplyr::bind_rows(!!!.targets) |>
      dplyr::group_by(model_id, esm, cell, x, y, month) |>
      dplyr::summarise(.mean_pred = stats::weighted.mean(.mean_pred, w = .ndays)) |>
      dplyr::ungroup() |>
      dplyr::mutate(period = !!v_period, .after = esm) |>
      rlang::expr(),
    pattern = map(!!!.targets) |>
      rlang::expr(),
    iteration = "list"
  )
}
