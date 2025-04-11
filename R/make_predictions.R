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
