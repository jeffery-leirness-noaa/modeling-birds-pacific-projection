#' Filter Data in an RSet Object
#'
#' This function filters data in an {rsample} resampling object (RSet) based on
#' specified conditions. It can filter either the analysis set, assessment set, or both.
#'
#' @param .rset An rsample resampling object.
#' @param ... Conditions to filter by, passed to dplyr::filter().
#' @param .split Character string. Which split to filter: "both", "analysis", or "assessment".
#'              Default is "both".
#'
#' @return A filtered {rsample} resampling object.
#'
#' @examples
#' # Filter data in both analysis and assessment sets where year > 2000
#' filtered_rset <- filter_rset_data(data_analysis_resamples_spatial, year > 2000)
#'
#' # Filter only the analysis set
#' filtered_rset <- filter_rset_data(data_analysis_resamples_spatial, 
#'                                  year > 2000, 
#'                                  .split = "analysis")
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
