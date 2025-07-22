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

rmsle.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
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


#' Negative binomial log-loss calculation
#'
#' Calculate the negative log-likelihood for a negative binomial model fit.
#' This function extracts the fitted model, theta parameter, observed values,
#' and predicted values to compute the log-loss metric.
#'
#' @param object A model fit object containing the fitted model results,
#'   typically from a tidymodels workflow or tune results. The object should
#'   contain `.fit` column with model fits that include extracts and predictions.
#'
#' @return A numeric value representing the negative log-likelihood (log-loss)
#'   for the negative binomial model. Lower values indicate better model fit.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts the fitted GAM model engine
#'   \item Retrieves the theta (dispersion) parameter from the negative binomial family
#'   \item Identifies the outcome variable name
#'   \item Extracts observed values from the assessment set
#'   \item Extracts predicted values
#'   \item Calculates log-likelihoods using the negative binomial distribution
#'   \item Returns the negative sum of log-likelihoods
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming you have a fitted model object with negative binomial family
#' log_loss_value <- calculate_nbinom_log_loss(fitted_model_object)
#' }
calculate_nbinom_log_loss <- function(object) {
  # Extract the fitted GAM model engine from the tune results
  model_gam <- tune::collect_extracts(object) |>
    dplyr::pull(.extracts) |>
    purrr::pluck(1) |>
    purrr::pluck(2) |>
    parsnip::extract_fit_engine()

  # Get the theta (dispersion) parameter from the negative binomial family
  theta <- model_gam$family$getTheta(TRUE)

  # Identify the outcome variable name from the model recipe
  y_var <- tune::collect_extracts(object) |>
    dplyr::pull(.extracts) |>
    purrr::pluck(1) |>
    purrr::pluck(1) |>
    summary() |>
    dplyr::filter(role == "outcome") |>
    dplyr::pull(variable)

  # Extract observed values from the assessment (test) set
  y_obs <- dplyr::pull(object, splits) |>
    purrr::pluck(1) |>
    rsample::assessment() |>
    dplyr::pull(!!y_var)

  # Extract predicted values from tune results
  mu_pred <- tune::collect_predictions(object) |>
    dplyr::pull(.pred)

  # Calculate log-likelihoods for each observation using negative binomial distribution
  log_likelihoods <- stats::dnbinom(
    x = y_obs,
    mu = mu_pred,
    size = theta,
    log = TRUE
  )

  # Return negative sum of log-likelihoods (log-loss)
  -sum(log_likelihoods)
}

# predicted density in cells with no observed presence
# caution: this is experimental
# conception: Arliss Winship
calculate_pdnp <- function(r, data, column, monthly = TRUE, power = 2) {
  if (monthly) {
    cells_present <- purrr::map(1:12, .f = \(x) {
      terra::extract(
        r,
        y = dplyr::filter(
          data,
          .data[[column]] > 0,
          lubridate::month(date) == !!x
        ) |>
          terra::vect(),
        cells = TRUE
      ) |>
        dplyr::pull(cell) |>
        unique()
    })
  } else {
    cells_present <- terra::extract(
      r,
      y = dplyr::filter(data, .data[[column]] > 0) |>
        terra::vect(),
      cells = TRUE
    ) |>
      dplyr::pull(cell) |>
      unique()
  }

  df <- terra::as.data.frame(r, cells = TRUE) |>
    dplyr::mutate(dplyr::across(!cell, ~ .x^power)) |>
    tibble::as_tibble()

  df_sum <- dplyr::select(df, !cell) |>
    sum(na.rm = TRUE)

  purrr::map(1:12, .f = \(x) {
    dplyr::filter(df, !(cell %in% cells_present[[x]])) |>
      dplyr::pull(month.name[x]) |>
      sum(na.rm = TRUE) /
      df_sum
  }) |>
    purrr::list_c() |>
    sum()
}
