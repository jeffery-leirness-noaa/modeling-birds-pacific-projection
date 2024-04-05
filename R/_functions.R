
create_intervals_monthly <- function(x) {
  tibble::tibble(start = time(x) |>
                   lubridate::as_date() |>
                   lubridate::rollbackward(roll_to_first = TRUE) |>
                   unique(),
                 end = lubridate::rollforward(start),
                 label = format(start, "%Y-%m"))
}

create_intervals_daily <- function(x) {
  tibble::tibble(start = time(x) |>
                   lubridate::as_date() |>
                   unique(),
                 end = start,
                 label = start)
}

create_covariate_output <- function(x, start, end, fname, label) {
  library(lubridate)
  idx <- time(x) |>
    lubridate::date() %within% lubridate::interval(start, end) |>
    which()
  r <- x[, , , idx] |>  # dplyr::slice(time, idx) can be used if x is not a stars proxy object
    stars::st_as_stars() |>
    terra::rast() |>
    terra::mean()
  names(r) <- label
  terra::writeRaster(r, filename = file.path("output", paste0(fname, "-", label, ".tif")), overwrite = TRUE)
  file.path("output", paste0(fname, "-", label, ".tif"))
}
