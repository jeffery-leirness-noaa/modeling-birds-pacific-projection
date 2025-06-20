#' Make Predictions Using a Fitted Model
#'
#' This function makes predictions for a new dataset using a fitted model.
#' It prepares the data and applies the model, with options to exclude certain
#' model terms from the predictions.
#'
#' @param object A fitted model object.
#' @param data Data frame. The prediction dataset.
#' @param label Character string. The type of data (e.g., "reanalysis").
#' @param add List. Additional data to include (e.g., bathymetry).
#' @param mask Spatial mask to apply to the predictions.
#'
#' @return A data frame with predictions.
#'
#' @examples
#' # Make predictions for 2010 using a fitted model
#' predictions <- make_predictions(
#'   model_fit,
#'   data = data_prediction_gfdl_2010,
#'   label = "reanalysis",
#'   add = list(depth = data_bathy_10km, slope = data_slope_10km),
#'   mask = study_polygon
#' )
# TO DO: change function default to `mask = NULL`
# TO DO: add option for object to be a workflow or parsnip object. If parsnip
# object, must also supply pre-processing recipe that will need to be applied
# (i.e., `recipes::bake()`) prior to calling `predict()`. Doing this could
# drastically reduce storage costs because fitted parsnip models could be
# "butchered" using `butcher::butcher()` to reduce object size.
make_predictions <- function(object, data, label, add, mask) {
  new_data <- prepare_data_prediction(data, label = label, add = add,
                                      mask = mask)
  pred <- predict(
    object,
    new_data = new_data,
    type = "raw",
    opts = list(type = "response",
                exclude = c("s(survey_id)", "s(date_decimal)"))
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(.pred = value)
  dplyr::select(new_data, esm, cell, x, y, date) |>
    dplyr::bind_cols(pred)
}
