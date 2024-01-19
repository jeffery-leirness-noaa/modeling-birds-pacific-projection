# ---------------------------------------------------------------------------- #
# download-polygons.R
# Date Created: 2024-01-19
# Author: Jeff Leirness
#
# Description: Shapefiles downloaded on January 19, 2024.
# ---------------------------------------------------------------------------- #


# download NOAA NGDC GSHHG shorelines polygon shapefiles ------------------
temp <- tempfile()
url <- "https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-2.3.7.tar.gz"
download.file(url, temp)
tempfiles <- unzip(temp, list = TRUE)
untar(temp, exdir = "data-raw/GSHHG")


# download natural earth data states & provinces polygon shapefile --------
temp <- tempfile()
url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip"
download.file(url, temp)
tempfiles <- unzip(temp, list = TRUE)
unzip(temp, exdir = tempdir())

# subset to usa, canada, and mexico
states <- sf::st_read(tempdir(), layer = "ne_10m_admin_1_states_provinces") |>
  tibble::as_tibble() |>
  dplyr::filter(adm0_a3 %in% c("CAN", "MEX", "USA"))

# save polygon as GPKG
sf::write_sf(states, "data-raw/states-provinces.gpkg")
