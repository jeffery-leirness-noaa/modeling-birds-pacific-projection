#' Root mean square logarithmic error (implemented as a custom performance metric in the [yardstick](https://yardstick.tidymodels.org/) package)
#'
#' Calculate the root mean square logarithmic error.
#'
#' @param data A `data.frame` containing the columns specified by the truth and estimate arguments.
#' @param ... Not currently used.
#' @param truth The column identifier for the true results (that is `numeric`). This should be an unquoted column name although this argument is passed by expression and supports quasiquotation (you can unquote column names). For `_vec()`⁠ functions, a `numeric` vector. This value is passed the `x` argument of `stats::cor()`.
#' @param estimate The column identifier for the predicted results (that is also `numeric`). As with `truth` this can be specified different ways but the primary method is to use an unquoted variable name. For `_vec()`⁠ functions, a numeric `vector`. This value is passed the `y` argument of `stats::cor()`.
#' @param na_rm A `logical` value indicating whether `NA` values should be stripped before the computation proceeds.
#' @param case_weights The optional column identifier for case weights. This should be an unquoted column name that evaluates to a numeric column in `data`. For `_vec()` functions, a `numeric` vector.
#'
#' @return
#' @examples
rmsle <- function(data, ...) {
  UseMethod("rmsle")
}
rmsle <- yardstick::new_numeric_metric(rmsle, direction = "minimize")

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  yardstick::numeric_metric_summarizer(
    name = "rmsle",
    fn = rmsle_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights)
  )
}

rmsle_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  rmsle_impl(truth, estimate, case_weights = case_weights)
}

rmsle_impl <- function(truth, estimate, case_weights = NULL) {
  sqrt(mean((log(estimate + 1) - log(truth + 1))^2))
}
