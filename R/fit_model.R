fit_model <- function(model_formula, data, species_size_class, mgcv_select = FALSE, mgcv_gamma = NULL) {

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

  gam_recipe <- recipes::recipe(preproc_formula, data = tibble::as_tibble(data)) |>
    recipes::step_date(date, features = c("doy", "decimal")) |>
    recipes::step_rm(date) |>
    recipes::step_rename(survey_area_km2 = !!survey_area_var) |>
    recipes::step_log(survey_area_km2) |>
    recipes::step_center(survey_area_km2) |>
    recipes::step_normalize(recipes::all_numeric_predictors(), -survey_area_km2)

  # recipes::prep(gam_recipe) |>
  #   recipes::bake(new_data = NULL)

  gam_model <- parsnip::gen_additive_mod(select_features = mgcv_select,
                                         adjust_deg_free = mgcv_gamma) |>
    parsnip::set_engine("mgcv", family = mgcv::nb()) |>
    parsnip::set_mode("regression")

  gam_workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe = gam_recipe) |>
    workflows::add_model(gam_model, formula = model_formula)

  parsnip::fit(gam_workflow, data = data)

}


plot_marginal_effects <- function(model, se = FALSE) {
  # marginal effects plots (i.e., term plots) with standard errors
  mgcv::plot.gam(model, rug = TRUE, se = se, pages = 1, scale = 0, shade = TRUE)
}
