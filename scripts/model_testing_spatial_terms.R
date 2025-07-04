# update/install necessary packages to versions specified in the renv lockfile
# note: this only needs to be done once to update the environment and should not need to be rerun on subsequent uses of the code (i.e., after an intial run, it can be commented out)
renv::restore(prompt = FALSE)

# specify example species for testing purposes
species_code_testing <- "rhau"

# specify model formula for testing purposes
# note: feel free to make this as simple/complex as you choose
# this is where you will need to add the spatial term specification
model_formula_testing <- glue::glue("{species_code_testing} ~ offset(survey_area_km2) + platform + s(date_doy, bs = \"cc\") + s(date_decimal, bs = \"tp\") + s(depth, bs = \"tp\")")

# source relevant R functions
targets::tar_source()

# load relevant data from azure storage
# note: the tar_load_azure_store() function will utilize your azure credentials and may require additional setup if you are getting errors
tar_load_azure_store("data_species_info")
tar_load_azure_store("data_analysis")
tar_load_azure_store("model_metrics")

# create "species_to_model" data frame
# note: this will only contain a single row based on the species code specified above
species_to_model <- create_species_to_model_df(
  data_analysis,
  species_info_df = data_species_info,
  threshold = 50
) |>
  dplyr::filter(code == species_code_testing)

# create "models_to_run" data frame
# note: this will only contain a single row and the model formula will be updated based on the model formula specified above
models_to_run <- create_models_to_run_df(species_to_model) |>
  dplyr::slice(1) |>
  dplyr::mutate(model_formula = model_formula_testing)

# define model workflow for tidymodels framework
# note: additional arguments to the define_model_workflow() function may need to be developed if additional arguments to the mgcv::gam() function are necessary based on the spatial term specification
model_workflow <- define_model_workflow(
  as.formula(models_to_run$model_formula),
  data = data_analysis,
  species_size_class = models_to_run$size_class,
  mgcv_select = TRUE,
  mgcv_gamma = models_to_run$mgcv_gamma
)

# fit the model via the tidymodels framework
model_fit <- generics::fit(model_workflow, data = data_analysis)

# get tidy data frame of model summary output
broom::tidy(model_fit)

# assess model fit
# note: see https://yardstick.tidymodels.org/articles/metric-types.html for details on specific performance metrics
generics::augment(model_fit, new_data = data_analysis) |>
  dplyr::mutate(.obs = .data[[species_code_testing]]) |>
  model_metrics(truth = .obs, estimate = .pred)

# extract the fitted {mgcv} model object if necessary for further inspection
model_fit_mgcv <- workflows::extract_fit_engine(model_fit)

# explore additional model checks
summary(model_fit_mgcv)
mgcv::gam.check(model_fit_mgcv)
mgcv::plot.gam(model_fit_mgcv, pages = 1, scale = 0)
