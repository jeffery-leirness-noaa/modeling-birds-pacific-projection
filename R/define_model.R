#' Define a Model Recipe for Species Distribution Models
#'
#' This function creates a model preprocessing recipe for species distribution modeling.
#' It handles variable transformations, normalization, and preprocessing steps.
#'
#' @param model_formula Formula. The model formula.
#' @param data Data frame. The data to be used for modeling.
#' @param species_size_class Character string. Size class of the species ("sm" or "lg").
#'
#' @return A recipe object.
#'
#' @examples
#' # Define a recipe for Black-footed Albatross
#' recipe <- define_model_recipe(
#'   model_formula = bfal ~ offset(survey_area_km2) + platform + s(date_doy, bs = "cc"),
#'   data = data_analysis,
#'   species_size_class = "lg"
#' )
define_model_recipe <- function(model_formula, data, species_size_class) {

  survey_area_var <- stringr::str_c("survey_area_km2_", species_size_class)

  lhs <- formula.tools::lhs(model_formula) |>
    as.character()
  op <- formula.tools::op(model_formula) |>
    as.character()
  rhs <- formula.tools::rhs(model_formula) |>
    all.vars() |>
    stringr::str_replace_all(c("survey_area_km2" = survey_area_var,
                               "date_doy" = "date",
                               "date_decimal" = "date")) |>
    unique() |>
    stringr::str_flatten(collapse = " + ")
  preproc_formula <- stringr::str_c(lhs, op, rhs, sep = " ") |>
    as.formula()

  recipes::recipe(preproc_formula, data = tibble::as_tibble(data)) |>
    recipes::step_date(date, features = c("doy", "decimal")) |>
    recipes::step_rm(date) |>
    recipes::step_rename(survey_area_km2 = !!survey_area_var) |>
    recipes::step_log(survey_area_km2) |>
    recipes::step_center(survey_area_km2) |>
    recipes::step_normalize(
      recipes::all_numeric_predictors(),
      -c(survey_area_km2, date_doy)
    ) |>
    recipes::step_naomit(recipes::all_outcomes(), recipes::all_predictors())

}

#' Define a GAM Model Specification
#'
#' This function creates a model specification for a Generalized Additive Model (GAM)
#' using the mgcv engine.
#'
#' @param mgcv_select Logical. Whether to use automatic feature selection. Default is FALSE.
#' @param mgcv_gamma Numeric or NULL. Penalty for the degrees of freedom in the model.
#'                  Default is NULL.
#'
#' @return A model specification object.
define_model_spec <- function(mgcv_select = FALSE, mgcv_gamma = NULL) {
  gam_model <- parsnip::gen_additive_mod(
    select_features = !!mgcv_select,
    adjust_deg_free = !!mgcv_gamma
  ) |>
    parsnip::set_engine(
      "mgcv",
      family = mgcv::nb(),
      knots = list(date_doy = c(1, 366))
    ) |>
    parsnip::set_mode("regression")
}

#' Define a Model Workflow
#'
#' This function creates a tidymodels workflow that combines a preprocessing recipe
#' and a model specification.
#'
#' @param model_formula Formula. The model formula.
#' @param data Data frame. The data to be used for modeling.
#' @param species_size_class Character string. Size class of the species ("sm" or "lg").
#' @param mgcv_select Logical. Whether to use automatic feature selection. Default is FALSE.
#' @param mgcv_gamma Numeric or NULL. Penalty for the degrees of freedom in the model.
#'                  Default is NULL.
#'
#' @return A workflow object.
#'
#' @examples
#' # Define a workflow for a model
#' workflow <- define_model_workflow(
#'   model_formula = rhau ~ offset(survey_area_km2) + platform + s(date_doy, bs = "cc"),
#'   data = data_analysis,
#'   species_size_class = "lg",
#'   mgcv_select = TRUE,
#'   mgcv_gamma = 1.4
#' )
define_model_workflow <- function(model_formula, data, species_size_class,
                                  mgcv_select = FALSE, mgcv_gamma = NULL) {
  model_recipe <- define_model_recipe(model_formula = model_formula,
                                      data = data,
                                      species_size_class = species_size_class)
  model_spec <- define_model_spec(mgcv_select = mgcv_select,
                                  mgcv_gamma = mgcv_gamma)
  workflows::workflow() |>
    workflows::add_recipe(recipe = model_recipe) |>
    workflows::add_model(model_spec, formula = model_formula)
}
