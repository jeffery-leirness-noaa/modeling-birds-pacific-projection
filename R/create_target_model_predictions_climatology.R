#' Create a Target for Summarizing Model Predictions into Monthly Climatologies
#'
#' This function creates a {targets} pipeline target that combines model predictions
#' and calculates monthly climatologies.
#'
#' @param .targets List of targets. The model prediction targets to combine.
#' @param esm Character string. The Earth System Model used for predictions.
#' @param period Character string. The time period of the climatology.
#'
#' @return A targets pipeline target object.
create_target_model_predictions_climatology  <- function(.targets, esm, period) {
  targets::tar_target_raw(
    glue::glue("model_predictions_climatology_{esm}_{period}"),
    command = dplyr::bind_rows(!!!.targets) |>
      dplyr::group_by(model_id, esm, cell, x, y, month) |>
      dplyr::summarise(.mean_pred = stats::weighted.mean(.mean_pred, w = .ndays)) |>
      dplyr::ungroup() |>
      dplyr::mutate(period = !!period, .after = esm) |>
      rlang::expr(),
    pattern = map(!!!.targets) |>
      rlang::expr(),
    iteration = "list"
  )
}
