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
