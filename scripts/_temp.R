library(sf)

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



# base map
m <- ggplot() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "gray20"),
        panel.grid = element_line(color = "gray10", size = 0.3)) +
  labs(x = NULL,
       y = NULL,
       title = spcodes$common_name[spcodes$spp_cd == sp])
gdc <- guide_colorbar(title.position = "top",
                      title.hjust = 0.5,
                      barwidth = unit(70 / length(labels), units = "mm"),
                      barheight = unit(2, units = "mm"))
odm <- m +
  geom_raster(data = r.df[r.df$month %in% month.name[i], ],
              aes(x = x, y = y, fill = observed)) +
  geom_sf(data = land.pcs, color = "gray10", fill = "gray10", size = 0.3) +
  geom_sf(data = states.pcs, color = "gray20", fill = "gray20", size = 0.3) +
  geom_sf(data = canus.pcs, color = "gray20", fill = "gray20", size = 0.3) +
  geom_sf(data = lakes.pcs, color = "gray20", fill = "gray20", size = 0.3) +
  coord_sf(xlim = c(ext(r)[1], ext(r)[2]), ylim = c(ext(r)[3], ext(r)[4])) +
  facet_wrap(~ month) +
  scale_fill_viridis(name = NULL,
                     # name = "Observed density",
                     option = "turbo",
                     na.value = "transparent",
                     limits = c(0, max(r.df[r.df$month %in% month.name[i], "observed"])),
                     guide = gdc)
ggsave(here(dir.out, fnm), plot = odm, width = fwidth, height = fheight, units = "in")



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
