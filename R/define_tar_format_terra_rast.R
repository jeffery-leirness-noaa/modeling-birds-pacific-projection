#' Define a Custom {targets} Format for {terra} SpatRaster Objects
#' 
#' This function creates a custom format for the {targets} package to handle
#' {terra} SpatRaster objects. It defines how to read, write, marshal, unmarshal,
#' convert, and copy SpatRaster objects within the {targets} pipeline.
#' 
#' The function is primarily used to ensure that SpatRaster objects are properly
#' handled when being processed by the {targets} package, especially when storing
#' and retrieving them from the {targets} cache. It supports saving rasters in
#' different file formats, though it has special handling for GeoTIFF format.
#' 
#' @param filetype Character string. The file format to use when writing rasters.
#'   Currently has special handling for "GTiff" (GeoTIFF).
#' 
#' @return A {targets} format object for handling {terra} SpatRaster objects.
#' 
#' @details This custom format implements all the necessary functions for the
#'   {targets} package to handle SpatRaster objects:
#'   \itemize{
#'     \item read: Function to read a SpatRaster from disk
#'     \item write: Function to write a SpatRaster to disk
#'     \item marshal: Function to convert a SpatRaster to a simple R object
#'     \item unmarshal: Function to convert the simple R object back to a SpatRaster
#'     \item convert: Function to ensure proper internal state of the SpatRaster
#'     \item copy: Function to create a deep copy of the SpatRaster
#'   }
#'   
#' @examples
#' # Define a format for GeoTIFF files
#' terra_tiff_format <- define_tar_format_terra_rast("GTiff")
#' 
#' # Use the format in a targets pipeline
#' # tar_target(
#' #   name = elevation_raster,
#' #   command = terra::rast("elevation.tif"),
#' #   format = define_tar_format_terra_rast("GTiff")
#' # )
#' 
define_tar_format_terra_rast <- function(filetype) {
  read_terra <- function(path) terra::rast(path)
  if (filetype == "GTiff") {
    write_terra <- function(object, path) terra::writeRaster(
      object,
      path,
      filetype = filetype,
      overwrite = TRUE
    )
  }
  targets::tar_format(
    read = read_terra,
    write = write_terra,
    marshal = function(object) terra::wrap(object),
    unmarshal = function(object) terra::unwrap(object),
    convert = function(object) {
      terra::set.values(object)
      object
    },
    copy = function(object) terra::deepcopy(object),
    substitute = list(filetype = filetype)
  )
}
