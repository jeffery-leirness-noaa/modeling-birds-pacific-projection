sample_data <- function(data, ..., n = NULL, prop = NULL) {
  data_grouped <- dplyr::group_by(data, ...)
  if (!is.null(n)) {
    dplyr::slice_sample(data_grouped, n = n)
  } else if (!is.null(prop)) {
    dplyr::slice_sample(data_grouped, prop = prop)
  }
}
