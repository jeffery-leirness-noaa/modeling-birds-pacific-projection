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

define_model_spec <- function(mgcv_select = FALSE,
                              mgcv_gamma = NULL,
                              nb_mat = NULL,
                              spatial_method = c('none', 'mrf', 'gp')) {

  spatial_method <- match.arg(spatial_method)

  engine_args <- list(
    family = mgcv::nb(),
    knots = list(date_doy = c(1, 366))
  )

  # # add spatial smoother specifications
  # if (spatial_method == "mrf" && !is.null(nb_mat)) {
  #   engine_args$bs <- "mrf"
  #   engine_args$xt <- list(penalty = nb_mat)
  # } else if (spatial_method == "gp") {
  #   engine_args$bs <- "gp"
  #   engine_args$m <- c(2, 3/2)  # Matérn smoothness parameter
  # }

  # # Create smooth constructor for MRF
  # if(!is.null(nb_mat)) {
  #   engine_args <- list(
  #     family = mgcv::nb(),
  #     knots = list(date_doy = c(1, 366)),
  #     bs = "mrf",
  #     xt = list(
  #       penalty = nb_mat
  #     )
  #   )
  # } else {
  #   engine_args <- list(
  #     family = mgcv::nb(),
  #     knots = list(date_doy = c(1, 366))
  #   )
  # }

  gam_model <- parsnip::gen_additive_mod(
    select_features = !!mgcv_select,
    adjust_deg_free = !!mgcv_gamma
  ) |>
    parsnip::set_engine("mgcv", !!!engine_args) |>
    parsnip::set_mode("regression")

}

define_model_workflow <- function(model_formula, data,
                                  species_size_class,
                                  mgcv_select = FALSE,
                                  mgcv_gamma = NULL,
                                  nb_mat = NULL, #add option for mrf neighborhood matrix
                                  spatial_method = c('none', 'mrf', 'gp')
                                  ) {

  spatial_method <- match.arg(spatial_method)

  model_recipe <- define_model_recipe(model_formula = model_formula,
                                      data = data,
                                      species_size_class = species_size_class)
  model_spec <- define_model_spec(mgcv_select = mgcv_select,
                                  mgcv_gamma = mgcv_gamma,
                                  nb_mat = nb_mat, #add neighborhood matrix
                                  spatial_method = spatial_method
                                  )
  workflows::workflow() |>
    workflows::add_recipe(recipe = model_recipe) |>
    workflows::add_model(model_spec, formula = model_formula)
}

plot_marginal_effects <- function(model, se = FALSE) {
  # marginal effects plots (i.e., term plots) with standard errors
  mgcv::plot.gam(model, rug = TRUE, se = se, pages = 1, scale = 0, shade = TRUE)
}
