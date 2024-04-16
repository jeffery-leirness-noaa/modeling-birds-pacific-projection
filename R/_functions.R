create_intervals_monthly <- function(file, round_dt = FALSE) {
  tm <- stars::read_ncdf(file) |>
    time()
  if (round_dt) {
    tm <- (tm - 1) |>
      lubridate::round_date(unit = "day")
  }
  tibble::tibble(start = tm |>
                   lubridate::as_date() |>
                   lubridate::rollbackward(roll_to_first = TRUE) |>
                   unique(),
                 end = lubridate::rollforward(start),
                 label = format(start, "%Y-%m"))
}

create_intervals_daily <- function(file, round_dt = FALSE) {
  tm <- stars::read_ncdf(file) |>
    time()
  if (round_dt) {
    tm <- (tm - 1) |>
      lubridate::round_date(unit = "day")
  }
  tibble::tibble(start = tm |>
                   lubridate::as_date() |>
                   unique(),
                 end = start,
                 label = start)
}

process_covariate_file <- function(file, start, end, label, round_dt = FALSE) {
  library(lubridate)
  x <- stars::read_ncdf(file)
  tm <- time(x)
  if (round_dt) {
    tm <- (tm - 1) |>
      lubridate::round_date(unit = "day")
  }
  idx <- lubridate::date(tm) %within% lubridate::interval(start, end) |>
    which()
  r <- x[, , , idx] |>  # dplyr::slice(time, idx) can be used if x is not a stars proxy object
    stars::st_as_stars()

  # fix lat/lon values that were incorrectly stored in netcdf file --------
  nc <- ncdf4::nc_open(file)
  lon <- ncdf4::ncvar_get(nc, "lon_rho")
  lat <- ncdf4::ncvar_get(nc, "lat_rho")
  ncdf4::nc_close(nc)
  r <- stars::st_as_stars(list(sst = r[[1]]),
                          dimensions = stars::st_dimensions(x = round(lon[, 1], 2),
                                                            y = round(lat[1, ], 2),
                                                            time = tm[idx])) |>
    terra::rast()
  # -----------------------------------------------------------------------

  if (terra::nlyr(r) > 1) {
    r <- terra::mean(r)
  } else {
    terra::time(r) <- NULL
  }
  terra::crs(r) <- "epsg:4326"
  names(r) <- label
  terra::wrap(r)
}

create_covariate_output <- function(file, start, end, fname, label, round_dt = FALSE) {
  r <- process_covariate_file(file, start = start, end = end, label = label, round_dt = round_dt) |>
    terra::unwrap()
  terra::writeRaster(r, filename = file.path("output", paste0(fname, "-", label, ".tif")), overwrite = TRUE)
  file.path("output", paste0(fname, "-", label, ".tif"))
}

process_covariate_data <- function(file, fname) {
  int_monthly <- create_intervals_monthly(file)
  purrr::map(int_monthly, \(x) create_covariate_output(file, start = x$start, end = x$end, fname = fname, label = x$label))
}


prepare_data <- function(path) {
  data.table::fread(path) |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    dplyr::select(!(chla:index_pdo_lag12)) |>
    dplyr::mutate(date = lubridate::as_date(paste(year, month, day, sep = "-"))) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
}
