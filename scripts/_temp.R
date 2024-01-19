# load study area polygon
sa <- "data/study-area.gpkg" |>
  sf::read_sf()

# load states & provinces polygon
states <- "data-raw/states-provinces.gpkg" |>
  sf::read_sf() |>
  sf::st_transform(crs = sf::st_crs(sa))

# load bird data
dat <- "data/segmented-data.csv" |>
  data.table::fread() |>
  tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
  sf::st_transform(crs = sf::st_crs(sa))

# plots by year of bird data
bbox <- sf::st_bbox(dat)
ggplot2::ggplot() +
  ggplot2::geom_sf(data = states) +
  ggplot2::geom_sf(data = dat) +
  ggplot2::coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
  ggplot2::facet_wrap(~ year)

# summary of total area surveyed each year
dat |>
  dplyr::group_by(year) |>
  dplyr::summarise(total_area = sum(seg_length_km * seg_width_km_lg)) |>
  print(n = Inf)

# mapview interactive plot
m <- dat |>
  # dplyr::filter(year < 1982) |>
  mapview::mapview(zcol = "year", burst = TRUE, cex = 3, alpha = 0, label = "survey_id", hide = TRUE)

mapview::mapshot2(m, url = "data/map-data-by-year.html")
browseURL("data/map-data-by-year.html")


# load NPPSD data
nppsd <- "data-raw/NPPSDv4.1/Locations_NPPSDv4.1.csv" |>
  data.table::fread() |>
  tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") |>
  sf::st_transform(crs = sf::st_crs(sa))

ck <- sf::st_intersects(sa, nppsd)[[1]]

nppsd <- nppsd |>
  dplyr::slice(ck)
janitor::tabyl(nppsd, "survey_id")


library(sf)

sa |>
  sf::st_geometry() |>
  plot()
nppsd |>
  dplyr::filter(survey_id |> stringr::str_starts(pattern = "TR3")) |>
  sf::st_geometry() |>
  plot(add = TRUE)


sa |>
  sf::st_geometry() |>
  plot()
dat |>
  sf::st_geometry() |>
  plot(add = TRUE)
