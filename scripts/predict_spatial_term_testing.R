# specify desired options in a list
option_list <- list(
  optparse::make_option("--dir_in",
                        type = "character",
                        dest = "dir_in",
                        default = "data",
                        help = "Input directory to use [default %default]"),
  optparse::make_option("--dir_processing",
                        type = "character",
                        dest = "dir_processing",
                        default = "processing",
                        help = "Processing directory to use [default %default]"),
  optparse::make_option("--species",
                        type = "character",
                        dest = "species",
                        default = "cagu",
                        help = "Species code to use [default %default]"),
  optparse::make_option("--spatial_type",
                        type = "character",
                        dest = "spatial_type",
                        default = "none",
                        help = "Type of spatial term to use [default %default]")
)

# get command line options
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# load workflows package
library(workflows)

# source R scripts in the R/ folder
targets::tar_source()

# load model fit object
model_fit <- fs::path(
  opt$dir_processing,
  "modeling-birds-spatial-term-tests",
  glue::glue("{opt$species}_{opt$spatial_type}_model_object.rds")
) |>
  readRDS()

# load prediction grid
grid_10km <- fs::path(opt$dir_processing, "_targets_main", "objects",
                      "grid_10km") |>
  terra::rast()

# load study area polygon
study_polygon <- fs::path(opt$dir_processing, "_targets_main", "objects",
                          "study_polygon") |>
  qs2::qs_read()

# load bathymetry raster
data_bathy_10km <- fs::path(opt$dir_processing, "_targets_main", "objects",
                            "data_bathy_10km") |>
  terra::rast()

# load slope raster
data_slope_10km <- fs::path(opt$dir_processing, "_targets_main", "objects",
                            "data_slope_10km") |>
  terra::rast()

# specify parallel processing
future::plan(future::multicore, workers = 1)

# make predictions for each year
model_predictions <- furrr::future_map(
  1980:2017,
  \(x) make_predictions(
    model_fit,
    data = fs::path(opt$dir_processing, "_targets_main", "objects",
                    glue::glue("data_prediction_gfdl_{x}")) |>
      qs2::qs_read(),
    label = "reanalysis",
    add = list(depth = data_bathy_10km,
               slope = data_slope_10km),
    mask = study_polygon
  ) |>
    dplyr::mutate(year = lubridate::year(date),
                  month = lubridate::month(date)) |>
    dplyr::group_by(esm, cell, x, y, year, month) |>
    dplyr::summarise(.ndays = dplyr::n(),
                     .mean_pred = mean(.pred)) |>
    dplyr::ungroup()
)

# save model predictions object
qs2::qs_save(
  model_predictions,
  file = fs::path(
    opt$dir_processing,
    "modeling-birds-spatial-term-tests",
    glue::glue("{opt$species}_{opt$spatial_type}_model_prediction_object.qs")
  )
)

# summarize predictions as monthly climatologies
model_predictions_climatology <- dplyr::bind_rows(model_predictions) |>
  dplyr::group_by(esm, cell, x, y, month) |>
  dplyr::summarise(.mean_pred = stats::weighted.mean(.mean_pred, w = .ndays)) |>
  dplyr::ungroup()

# convert climatology predictions to SpatRaster object
r_temp <- grid_10km
terra::values(r_temp) <- NA
r <- purrr::map(1:12, \(x) {
  temp <- dplyr::filter(model_predictions_climatology, month == !!x)
  r_i <- r_temp
  terra::set.values(r_i, cells = temp$cell, values = temp$.mean_pred)
  names(r_i) <- month.name[x]
  r_i
}) |>
  terra::rast() |>
  terra::crop(y = study_polygon, mask = TRUE)
terra::varnames(r) <- opt$species

# save SpatRaster as GeoTIFF
file_name <- glue::glue("{opt$species}_{opt$spatial_type}_model_predictions")
file_path <- fs::path("modeling-birds-spatial-term-tests", file_name) |>
  fs::path_ext_set(ext = ".tiff")
terra::writeRaster(r, filename = fs::path(opt$dir_processing, file_path))
