# specify desired options in a list
option_list <- list(
  optparse::make_option("--dir_in",
                        type = "character",
                        dest = "dir_in",
                        default = "data",
                        help = "Input directory to use [default %default]"),
  optparse::make_option("--dir_processing",
                        type = "character",
                        dest = "dir_processing",
                        default = "processing",
                        help = "Processing directory to use [default %default]"),
  optparse::make_option("--esm",
                        type = "character",
                        dest = "esm",
                        default = "gfdl",
                        help = "Earth system model data to use [default %default]")
)

# get command line options
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# load prediction grid
grid_10km <- terra::rast(fs::path(opt$dir_in, "grid-10km.tiff"))

# create paths to netCDF files for each environmental variable
# files <- fs::path("data/environmental-data", opt$esm, paste0(vars, "_daily_pcs.nc"))
files <- fs::path(opt$dir_in, "environmental-data", opt$esm) |>
  fs::dir_ls(glob = "*_daily.nc")

# load all netCDF files
r <- list()
for (i in seq_along(files)) {
  r[[i]] <- terra::rast(files[i])
  names(r[[i]]) <- rep(terra::varnames(r[[i]]), terra::nlyr(r[[i]]))
}
r <- terra::rast(r)

# get unique years of data
yrs <- terra::time(r) |>
  lubridate::year() |>
  unique() |>
  sort()
yrs <- 2005

for (year in yrs) {

  # subset the raster data to the specified year
  idx <- lubridate::year(terra::time(r)) == year
  r_i <- terra::subset(r, idx)

  # project the raster onto the prediction grid
  r_i <- terra::project(r_i, grid_10km, method = "bilinear")

  # convert raster to data frame and reshape for analysis
  df_i <- terra::as.data.frame(r_i, xy = TRUE, cells = TRUE, time = TRUE, wide = FALSE) |>
    tibble::as_tibble() |>
    dplyr::rename(date = time) |>
    tidyr::pivot_wider(names_from = layer, values_from = values)

  png(fs::path(opt$dir_processing, glue::glue("{opt$esm}_daily_pcs_{year}.png")))
  terra::plot(r_i[[1]])
  dev.off()

  # g <- dplyr::filter(df_i, lubridate::yday(date) == 1) |>
  #   ggplot2::ggplot(mapping = ggplot2::aes(x, y, fill = sst)) +
  #   ggplot2::geom_raster() +
  #   ggplot2::coord_equal()
  # ggplot2::ggsave(fs::path(opt$dir_processing, glue::glue("{opt$esm}_daily_pcs_{year}.png")))

  test <- dplyr::filter(df_i, lubridate::yday(date) == 1)
  qs2::qs_save(test, fs::path(opt$dir_processing, "test"))
  test <- qs2::qs_read(fs::path(opt$dir_processing, "test"))
  r_temp <- grid_10km
  r_df <- terra::as.data.frame(r_temp, cells = TRUE) |>
    tibble::as_tibble()

  r_df <- dplyr::inner_join(r_df, test, by = "cell")
  terra::values(r_temp) <- NA
  r_temp[r_df$cell] <- r_df$sst

  png(fs::path(opt$dir_processing, glue::glue("{opt$esm}_daily_pcs_{year}_test_sst.png")))
  terra::plot(r_temp)
  dev.off()

  # g <- terra::as.data.frame(r_temp, xy = TRUE, cells = TRUE) |>
  #   tibble::as_tibble() |>
  #   ggplot2::ggplot(mapping = ggplot2::aes(x, y, fill = sst)) +
  #   ggplot2::geom_raster() +
  #   ggplot2::coord_equal()
  # ggplot2::ggsave(fs::path(opt$dir_processing, glue::glue("{opt$esm}_daily_pcs_{year}_test.png")))

  # # save as .qs file
  # file_i <- fs::path(opt$dir_processing, opt$esm, glue::glue("{opt$esm}_daily_pcs_{year}.qs"))
  # qs2::qs_save(df_i, file = file_i)

}
