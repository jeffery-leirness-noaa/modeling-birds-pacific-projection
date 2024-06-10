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
  if (length(idx) > 0) {

    r <- x[, , , idx] |>  # dplyr::slice(time, idx) can be used if x is not a stars proxy object
      stars::st_as_stars()

    # fix lat/lon values that were incorrectly stored in netcdf file --------
    nc <- ncdf4::nc_open(file)
    lon <- ncdf4::ncvar_get(nc, "lon_rho")
    lat <- ncdf4::ncvar_get(nc, "lat_rho")
    ncdf4::nc_close(nc)
    r <- stars::st_as_stars(list(var = r[[1]]),
                            dimensions = stars::st_dimensions(x = round(lon[, 1], 2),
                                                              y = round(lat[1, ], 2),
                                                              time = tm[idx])) |>
      terra::rast()
    # r <- stars::st_as_stars(x) |>
    #   aggregate(by = "months", FUN = mean)
    # stars::st_extract(r, )
    # -----------------------------------------------------------------------

    if (terra::nlyr(r) > 1) {
      r <- terra::mean(r)
    } else {
      terra::time(r) <- NULL
    }
    terra::crs(r) <- "WGS84"
    names(r) <- label
    terra::wrap(r)
  } else {
    NA
  }
}

extract_covariate_data <- function(x, at, time_column, aggregate_by = NULL, name, round_dt = FALSE) {

  r <- stars::read_ncdf(x, proxy = FALSE)
  tm <- time(r)

  # fix time values that were incorrectly stored in netcdf file -----------
  if (round_dt) {
    tm <- (tm - 1) |>
      lubridate::round_date(unit = "day")
  }

  # fix lat/lon values that were incorrectly stored in netcdf file --------
  err <- c("data/wcra31/wcra31_curl_daily_1980_2010.nc",
           "data/wcra31/wcra31_sustr_daily_1980_2010.nc",
           "data/wcra31/wcra31_su_daily_1980_2010.nc",
           "data/wcra31/wcra31_svstr_daily_1980_2010.nc",
           "data/wcra31/wcra31_sv_daily_1980_2010.nc",
           "data/wcnrt/wcnrt_curl_daily_20110102_20240117.nc",
           "data/wcnrt/wcnrt_sustr_daily_20110102_20240117.nc",
           "data/wcnrt/wcnrt_su_daily_20110102_20240117.nc",
           "data/wcnrt/wcnrt_svstr_daily_20110102_20240117.nc",
           "data/wcnrt/wcnrt_sv_daily_20110102_20240117.nc")
  if (x %in% err) {
    nc <- ncdf4::nc_open("data/wcnrt/wcnrt_sst_daily_20110102_20240117.nc")
    lon <- ncdf4::ncvar_get(nc, "lon_rho")
    lat <- ncdf4::ncvar_get(nc, "lat_rho")
    ncdf4::nc_close(nc)
  } else {
    nc <- ncdf4::nc_open(x)
    lon <- ncdf4::ncvar_get(nc, "lon_rho")
    lat <- ncdf4::ncvar_get(nc, "lat_rho")
    ncdf4::nc_close(nc)
  }

  r <- stars::st_as_stars(setNames(list(r[[1]]), name),
                          dimensions = stars::st_dimensions(x = lon[, 1],
                                                            y = lat[1, ],
                                                            time = tm))
  sf::st_crs(r) <- "WGS84"

  if (!is.null(aggregate_by)) {
    r <- aggregate(r, by = aggregate_by, FUN = mean)
    if (aggregate_by == "months") {
      at <- at |> dplyr::mutate("{time_column}" := lubridate::floor_date(.data[[time_column]], "month"))
    }
  }

  stars::st_extract(r, at = at, time_column = time_column)
}

create_covariate_output <- function(file, start, end, fname, label, round_dt = FALSE) {
  r <- process_covariate_file(file, start = start, end = end, label = label, round_dt = round_dt)
  if (isa(r, "PackedSpatRaster")) {
    r <- terra::unwrap(r)
    terra::writeRaster(r, filename = file.path("output", paste0(fname, "-", label, ".tif")), overwrite = TRUE)
    file.path("output", paste0(fname, "-", label, ".tif"))
  }
}

process_covariate_data <- function(file, fname) {
  int_monthly <- create_intervals_monthly(file)
  purrr::map(int_monthly, \(x) create_covariate_output(file, start = x$start, end = x$end, fname = fname, label = x$label))
}


prepare_data <- function(path) {
  data.table::fread(path) |>
    tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
    # dplyr::mutate(date = lubridate::as_date(paste(year, month, day, sep = "-")),
    #               .before = dplyr::everything()) |>
    # dplyr::select(!c(year, month, day, season, chla:index_pdo_lag12)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
}


sample_data <- function(.data, ..., n = NULL, prop = NULL) {
  if (!is.null(n)) {
    # readr::read_csv(path, show_col_types = FALSE) |>
    .data |>
      dplyr::group_by(...) |>
      dplyr::slice_sample(n = n)
  } else if (!is.null(prop)) {
    # readr::read_csv(path, show_col_types = FALSE) |>
    .data |>
      dplyr::group_by(...) |>
      dplyr::slice_sample(prop = prop)
  }
}
