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

plot_marginal_effects <- function(model, se = FALSE) {
  # marginal effects plots (i.e., term plots) with standard errors
  mgcv::plot.gam(model, rug = TRUE, se = se, pages = 1, scale = 0, shade = TRUE)
}
