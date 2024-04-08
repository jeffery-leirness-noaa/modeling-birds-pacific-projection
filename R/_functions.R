
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

create_covariate_output <- function(file, start, end, fname, label, round_dt = FALSE) {
  library(lubridate)
  x <- stars::read_ncdf(file)
  tm <- time(r)
  if (round_dt) {
    tm <- (tm - 1) |>
      lubridate::round_date(unit = "day")
  }
  idx <- lubridate::date(tm) %within% lubridate::interval(start, end) |>
    which()
  r <- x[, , , idx] |>  # dplyr::slice(time, idx) can be used if x is not a stars proxy object
    stars::st_as_stars() |>
    terra::rast() |>
    terra::mean()
  names(r) <- label
  terra::writeRaster(r, filename = file.path("output", paste0(fname, "-", label, ".tif")), overwrite = TRUE)
  file.path("output", paste0(fname, "-", label, ".tif"))
}

process_covariate_data <- function(file, fname) {
  int_monthly <- create_intervals_monthly(file)
  purrr::map(int_monthly, \(x) create_covariate_output(file, start = x$start, end = x$end, fname = fname, label = x$label))
}
