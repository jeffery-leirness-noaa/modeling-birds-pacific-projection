# specify analysis phase
phase <- 2

# specify directories
dir.root <- "D:/ccma/BIOGEO/Projects/Pacific_Seabird_Modeling"
dir.ap <- file.path(dir.root, "Analysis", paste("Phase", phase, sep = ""))

library(sf)

# load study area polygon
sa <- sf::read_sf(file.path(dir.ap, "Data/studyArea.shp"))

# load bird data
dat <- file.path(dir.ap, "Data", paste("segmentedData_analysisPhase", phase, ".csv", sep = "")) |> 
  readr::read_csv() |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |> 
  sf::st_transform(crs = sf::st_crs(sa))

# load NPPSD data
nppsd <- "C:/Users/jeffery.leirness/Downloads/NPPSDv4.1/Locations_NPPSDv4.1.csv" |> 
  readr::read_csv() |> 
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
