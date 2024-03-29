unzip("wcra31_daily_1980-2010.zip", exdir = "wcra31_daily_1980-2010")
unzip("WCNRT.zip", exdir = "WCNRT")

fn <- "wcra31_daily_1980-2010/wcra31_sst_daily_1980_2010.nc"

# this seems to work
r <- raster::brick(fn)
raster::plot(r[[1]])

r <- ncdf4::nc_open(fn)
r <- ncdf4::ncvar_get(r, "sst")


# perhaps the most promising
r <- stars::read_ncdf(fn)
idx <- time(r) |> 
  lubridate::date() == "1999-01-25"
r[, , , which(idx)] |> 
  plot()

intervals <- tidyr::expand_grid(year = 1980:2024, month = 1:12) |> 
  dplyr::mutate(start = lubridate::as_date(paste(year, month, "01", sep = "-")), 
                end = lubridate::rollforward(start)) |> 
  dplyr::select(start, end)


r <- stars::read_ncdf(fn)
idx <- time(r) |> 
  lubridate::date() %within% lubridate::interval("1999-01-01", "1999-01-31")
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


r <- stars::read_ncdf(fn, var = "sst", ncsub = cbind(start = c(1, 1, 1), count = c(NA, NA, 2)))
terra::rast(r) |> 
  terra::plot()




# this fails
r <- terra::rast(fn)



