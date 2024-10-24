sf_as_df <- function(data, names = NULL) {
  if (is.null(names)) {
    names <- c("X", "Y")
  }
  coords <- sf::st_coordinates(data) |>
    tibble::as_tibble()
  names(coords) <- names
  dplyr::bind_cols(coords, sf::st_drop_geometry(data))
}
