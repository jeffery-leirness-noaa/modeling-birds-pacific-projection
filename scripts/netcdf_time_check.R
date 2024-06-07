
# specify netcdf file
file <- "data/wcra31/wcra31_sst_daily_1980_2010.nc"

# get time component of netcdf file
(nc <- ncdf4::nc_open(file))
tm <- ncdf4::ncvar_get(nc, "time")
ncdf4::nc_close(nc)

# summary of time component
range(tm)
diff(tm) |>
  unique()

# convert time to datetime: method 1
(as.POSIXct("1900-01-01", tz = "UTC") + tm) |>
  range()

# convert time to datetime: method 2
(lubridate::as_datetime("1900-01-01") + lubridate::seconds(tm)) |>
  range()

# convert time to date
as.Date("1900-01-01") + (2525428800 / 86400)

# use {stars} package to summarize time component of netcdf file
stars::read_ncdf(file) |>
  time() |>
  range()
