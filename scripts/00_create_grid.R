# specify custom projected coordinate system
pcs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +k=0.9996 +x_0=0 +y_0=0 +gamma=75 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# load remote sensing boundary
extent <- c(-133, -116, 28, 50) |> 
  terra::ext() |> 
  terra::project(from = "epsg:4326", to = terra::crs(pcs))

# create 10 km grid based on remote sensing boundary extent
r <- terra::rast(crs = terra::crs(pcs), extent = extent, resolution = 10000)
terra::values(r) <- 1:terra::ncell(r)
terra::writeRaster(r, filename = "data/grid-10km.tif")
