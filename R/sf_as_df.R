#' Convert {sf} Object to Data Frame with Coordinates
#'
#' This function extracts coordinates from an {sf} object and combines them with
#' the attribute data to create a regular data frame.
#'
#' @param data {sf} object. The spatial data to convert.
#' @param names Character vector of length 2. Names for the coordinate columns.
#'             Default is c("X", "Y").
#'
#' @return A data frame with coordinates and attributes.
sf_as_df <- function(data, names = NULL) {
  if (is.null(names)) {
    names <- c("X", "Y")
  }
  coords <- sf::st_coordinates(data) |>
    tibble::as_tibble()
  names(coords) <- names
  dplyr::bind_cols(coords, sf::st_drop_geometry(data))
}
