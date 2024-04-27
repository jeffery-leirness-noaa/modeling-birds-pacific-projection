# unzip("wcra31_daily_1980-2010.zip", exdir = "wcra31_daily_1980-2010")
# unzip("WCNRT.zip", exdir = "WCNRT")

fl <- "data/wcra31/wcra31_sst_daily_1980_2010.nc"
nm <- fs::path_file(fl) |>
  stringr::str_split(pattern = "_") |>
  purrr::map_vec(.f = \(x) paste(x[1:2], collapse = "_"))

source("R/_functions.R")

dat <- prepare_data("data/segmented-data.csv") |>
  dplyr::mutate(yrmon = lubridate::floor_date(date, "month")) |>
  dplyr::group_by(yrmon) |>
  dplyr::mutate("{nm}" := extract_covariate_data(geometry |> terra::vect(),
                                                 file = fl,
                                                 start = unique(yrmon),
                                                 end = lubridate::rollforward(unique(yrmon))))
dat |>
  sf::st_drop_geometry() |>
  dplyr::summarise(sst = mean(wcra31_sst, na.rm = TRUE)) |>
  print(n = Inf)


fl <- "data/wcnrt/wcnrt_sst_daily_20110102_20240117.nc"
nm <- fs::path_file(fl) |>
  stringr::str_split(pattern = "_") |>
  purrr::map_vec(.f = \(x) paste(x[1:2], collapse = "_"))
dat <- dat |>
  dplyr::mutate("{nm}" := extract_covariate_data(geometry |> terra::vect(),
                                                 file = fl,
                                                 start = unique(yrmon),
                                                 end = lubridate::rollforward(unique(yrmon))))
dat |>
  sf::st_drop_geometry() |>
  dplyr::summarise(wcra31_sst = mean(wcra31_sst, na.rm = TRUE), wcnrt_sst = mean(wcnrt_sst, na.rm = TRUE)) |>
  print(n = Inf)





r <- process_covariate_file(fl, start = "1990-01-01", end = "1990-01-01", label = "1990-01-01", round_dt = TRUE) |>
  terra::unwrap()
temp <- dat_80 |> dplyr::filter(is.na(sst))
# mapview::mapview(raster::raster(r), na.color = "transparent", maxpixels = terra::ncell(bathy)) +
mapview::mapview(r, na.color = "transparent") +
  mapview::mapview(temp, alpha.regions = 0.5, label = "yrmon", layer.name = "sst")

tmap::tm_shape()

tmap::tmap_mode("view")
temp |>
  dplyr::filter(date == "1999-08-16") |>
  tmap::tm_shape() + tmap::tm_dots() +
  tmap::tm_shape(r) + tmap::tm_raster("1990-01-01", style = "cont", palette = "viridis")


temp |>
  dplyr::filter(date == "1999-08-16") |>
  sf::st_geometry() |>
  plot()
terra::plot(r, add = TRUE)


start <- sort(unique(dat$yrmon))[1]
end <- lubridate::rollforward(start)
r <- process_covariate_file(fl, start = start, end = end, label = start, round_dt = TRUE) |>
  terra::unwrap()

dat$sst <- NA
dat$sst[dat$yrmon == start] <- terra::extract(r, dplyr::filter(dat, yrmon == start), ID = FALSE)[, 1]

terra::plot(r)
terra::plot(v, add = TRUE)



# this seems to work
r <- raster::brick(fl)
raster::plot(r[[1]])

r <- ncdf4::nc_open(fl)
sst <- ncdf4::ncvar_get(r, "sst")
lon <- ncdf4::ncvar_get(r, "lon_rho")
lat <- ncdf4::ncvar_get(r, "lat_rho")
time <- ncdf4::ncvar_get(r, "time")
tunits <- ncdf4::ncatt_get(r, "time", "units")
time <- CFtime::CFtime(tunits$value, offsets = time) |>
  CFtime::CFtimestamp() |>
  lubridate::as_datetime()


r <- tidync::tidync(fl)
ck <- r |> tidync::activate("D0,D1") |> tidync::hyper_tbl_cube()

lon <- tidync::tidync(fl) |>
  tidync::activate("lon_rho") |>
  tidync::hyper_array()
lon <- lon$lon_rho

sst <- tidync::tidync(fl) |>
  tidync::activate("sst") |>
  tidync::hyper_filter(time = index < 2) |>
  tidync::hyper_tibble()


# use tidync to get var and lat/lon
# use ncdf4 to get time
lon <- tidync::tidync(fl) |>
  tidync::activate("lon_rho") |>
  tidync::hyper_array()
lat <- tidync::tidync(fl) |>
  tidync::activate("lat_rho") |>
  tidync::hyper_array()

nc <- ncdf4::nc_open(fl)
lon <- ncdf4::ncvar_get(nc, "lon_rho")
lat <- ncdf4::ncvar_get(nc, "lat_rho")
time <- ncdf4::ncvar_get(nc, "time")
tunits <- ncdf4::ncatt_get(nc, "time", "units")
time <- CFtime::CFtime(tunits$value, offsets = time) |>
  CFtime::CFtimestamp() |>
  lubridate::as_datetime()
ncdf4::nc_close(nc)


start <- lubridate::as_date("2012-01-01")
end <- lubridate::as_date("2012-01-31")
idx <- which(time >= start & time <= end)
sst <- tidync::tidync(fl) |>
  tidync::activate("sst") |>
  tidync::hyper_filter(time = index %in% idx) |>
  tidync::hyper_tibble()



# perhaps the most promising
r <- stars::read_ncdf(fl)


nc <- ncdf4::nc_open(fl)
lon <- ncdf4::ncvar_get(nc, "lon_rho")
lat <- ncdf4::ncvar_get(nc, "lat_rho")
time <- ncdf4::ncvar_get(nc, "time")
tunits <- ncdf4::ncatt_get(nc, "time", "units")
time <- CFtime::CFtime(tunits$value, offsets = time) |>
  CFtime::CFtimestamp() |>
  lubridate::as_datetime()
ncdf4::nc_close(nc)

start <- lubridate::as_date("2012-01-01")
end <- lubridate::as_date("2012-01-31")
idx <- which(time >= start & time <= end)
r <- stars::read_ncdf(fl)
sst <- r[, , , idx] |>
  st_as_stars()

r <- st_as_stars(list(sst = sst$sst), dimensions = st_dimensions(x = round(lon[, 1], 2), y = round(lat[1, ], 2),
                                                                 time = time[idx])) |>
  terra::rast()



attr(r, "dimensions")[["xi_rho"]]$delta <- 0.1
attr(r, "dimensions")[["xi_rho"]]$offset <- min(lon)

attr(r, "dimensions")[["eta_rho"]]$delta <- 0.1
attr(r, "dimensions")[["eta_rho"]]$offset <- min(lat)

r <- r[, , , 1] |>
  stars::st_as_stars() |>
  terra::rast()


r <- stars::st_set_dimensions(r, which = "xi_rho", values = lon[, 1], names = "lon")
r <- stars::st_set_dimensions(r, which = "eta_rho", values = lat[1, ], names = "lat")

idx <- time(r) |>
  lubridate::date() == "1999-01-25"
r[, , , which(idx)] |>
  plot()

intervals <- tidyr::expand_grid(year = 1980:2024, month = 1:12) |>
  dplyr::mutate(start = lubridate::as_date(paste(year, month, "01", sep = "-")),
                end = lubridate::rollforward(start)) |>
  dplyr::select(start, end)

library(lubridate)
i <- 1
r <- stars::read_ncdf(fl)
idx <- time(r) |>
  lubridate::date() %within% lubridate::interval(intervals$start[i], intervals$end[i])
r[, , , which(idx)] |>
  stars::st_as_stars() |>
  terra::rast() |>
  terra::mean() |>
  terra::plot()
d <- r[, , , which(idx)] |>
  stars::st_as_stars()
ggplot2::ggplot() +
  stars::geom_stars(data = d) +
  ggplot2::coord_equal() +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void() +
  viridis::scale_fill_viridis() +
  ggplot2::scale_x_discrete(expand = c(0, 0)) +
  ggplot2::scale_y_discrete(expand = c(0, 0))


# r <- stars::read_ncdf(fl, var = "sst", ncsub = cbind(start = c(1, 1, 1), count = c(NA, NA, 2)))
# terra::rast(r) |>
#   terra::plot()


# create month-year summary rasters
foo <- function(x, start, end, label) {
  if (match("time", names(stars::st_dimensions(x))) == 3) {
    library(lubridate)
    idx <- time(x) |>
      lubridate::date() %within% lubridate::interval(start, end) |>
      which()
    x <- x[, , , idx] |>  # dplyr::slice(time, idx) can be used if x is not a stars proxy object
      stars::st_as_stars() |>
      terra::rast() |>
      terra::mean()
    names(x) <- label
    x
  }
}
r <- stars::read_ncdf(fl)
intervals <- tibble::tibble(start = time(r) |>
                              lubridate::as_date() |>
                              lubridate::rollbackward(roll_to_first = TRUE) |>
                              unique(),
                            end = lubridate::rollforward(start),
                            label = format(start, "%Y-%m")) |>
  dplyr::rowwise() |>
  dplyr::group_split()

d <- purrr::map(intervals[1:5], \(x) foo(r, start = x$start, end = x$end, label = x$label))

terra::rast(d) |>
  terra::plot()

purrr::walk(d, \(x) terra::writeRaster(x, filename = file.path("output/wcra31", paste0("wcra31-sst-monthly-", names(x), ".tif"))))



# this fails
r <- terra::rast(fl)



