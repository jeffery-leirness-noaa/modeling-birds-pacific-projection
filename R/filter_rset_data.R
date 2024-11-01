filter_rset_data <- function(.rset, ..., .split = c("both", "analysis", "assessment")) {
  splits_analysis <- purrr::map(.rset$splits,
                                \(x) rsample::analysis(x))
  splits_assessment <- purrr::map(.rset$splits,
                                  \(x) rsample::assessment(x))
  if (.split %in% c("analysis", "both")) {
    splits_analysis <- purrr::map(splits_analysis,
                                  \(x) dplyr::filter(x, ...))
  }
  if (.split %in% c("assessment", "both")) {
    splits_assessment <- purrr::map(splits_assessment,
                                    \(x) dplyr::filter(x, ...))
  }
  purrr::map2(splits_analysis,
              splits_assessment,
              \(x, y) rsample::make_splits(x, assessment = y)) |>
    rsample::manual_rset(ids = .rset$id)
}
