dat <- data_bird_10km

vars <- c("hindcast_bbv_200",
          "hindcast_curl",
          "hindcast_ild_05",
          "hindcast_ssh",
          "hindcast_sst",
          "hindcast_su",
          "hindcast_sustr",
          "hindcast_sv",
          "hindcast_svstr",
          "hindcast_eke",
          "hindcast_chl_surf",
          "hindcast_zoo_100m_int",
          "reanalysis_bbv_200",
          "reanalysis_curl",
          "reanalysis_ild_05",
          "reanalysis_ssh",
          "reanalysis_sst",
          "reanalysis_su",
          "reanalysis_sustr",
          "reanalysis_sv",
          "reanalysis_svstr")
dat[, vars] <- NA
dat <- dplyr::mutate(dat,
              dplyr::across(dplyr::all_of(vars), ~ rnorm(nrow(data_bird_10km), mean = 0, sd = 5))) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
  sf::st_transform(crs = sf::st_crs(terra::unwrap(grid_10km)))

species_to_model <- create_species_to_model_df(dat,
                                               species_info_df = data_species_info,
                                               threshold = 50)
models_to_run <- create_models_to_run_df(species_to_model)

block_folds <- spatialsample::spatial_block_cv(dat)
spatialsample::autoplot(block_folds)

i <- 1
mod_workflow <- formula(models_to_run$model_formula[i]) |>
  fit_model(data = dat,
            species_size_class = "lg",
            mgcv_select = TRUE,
            mgcv_gamma = models_to_run$mgcv_gamma[i],
            fit = FALSE)

ptm <- proc.time()
ck <- formula(models_to_run$model_formula[i]) |>
  fit_model(data = dat,
            species_size_class = "lg",
            mgcv_select = TRUE,
            mgcv_gamma = models_to_run$mgcv_gamma[i],
            fit = TRUE)
proc.time() - ptm

ptm <- proc.time()
ck <- tune::fit_resamples(mod_workflow, resamples = block_folds)
proc.time() - ptm
tune::collect_metrics(ck)
