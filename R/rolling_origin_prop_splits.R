rolling_origin_prop_splits <- function(data, prop) {
  splits <- data |>
    dplyr::mutate(group = cut(1:nrow(data) / nrow(data),
                              breaks = c(0, rev(seq(-1, 0, by = prop) * -1)))) |>
    tidyr::nest(.by = group) |>
    rsample::rolling_origin(initial = 1)

  splits_analysis <- purrr::map(splits$splits,
                                \(x) rsample::analysis(x) |>
                                  tidyr::unnest(cols = data))

  splits_assessment <- splits$splits[[nrow(splits)]] |>
    rsample::assessment() |>
    tidyr::unnest(cols = data)

  purrr::map(splits_analysis,
             \(x) rsample::make_splits(x, assessment = splits_assessment)) |>
    rsample::manual_rset(ids = splits$id)
}
