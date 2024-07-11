
ptm <- Sys.time()
fl <- "data/wcra31_sst_daily_1980_2010.nc"
r_nc <- ncdf4::nc_open(fl)
varname <- r_nc$var$sst$name
longname <- r_nc$var$sst$longname
unit <- r_nc$var$sst$units
r <- stars::read_ncdf(fl)

idx <- time(r) |>
  lubridate::date()

for (i in 1:length(idx)) {
  x <- r[, , , i] |>  # dplyr::slice(time, idx) can be used if x is not a stars proxy object
    stars::st_as_stars() |>
    terra::rast()
  names(x) <- idx[i]
  terra::writeRaster(x, filename = file.path("output", paste0("wcra31-sst-daily-", idx[i], ".nc")), overwrite = TRUE)
  terra::writeCDF(x, filename = file.path("output", paste0("wcra31-sst-daily-", idx[i], ".nc")), overwrite = TRUE, varname = varname, longname = longname, unit = unit)
}
Sys.time() - ptm
# Time difference of 41.36021 mins

r <- terra::rast("output/wcra31-sst-daily-1999-10-08.tif")
