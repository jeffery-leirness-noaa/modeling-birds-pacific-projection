#' Remove Edges from a Raster
#'
#' This function sets the values at the edges of a raster to NA.
#' This is useful for removing edge effects in raster processing.
#'
#' @param x SpatRaster. The input raster.
#' @param edges Numeric vector of length 2. Number of cells to remove from the edges [rows, columns].
#'
#' @return SpatRaster with edges removed.
remove_raster_edges <- function(x, edges) {
  mat <- terra::as.matrix(x, wide = TRUE)
  mat[c(1:edges[1], (nrow(mat) - (edges[1] - 1)):nrow(mat)), ] <- NA
  mat[, 1:edges[2]] <- NA
  terra::values(x) <- mat
  x
}
