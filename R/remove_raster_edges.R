remove_raster_edges <- function(x, edges) {
  mat <- terra::as.matrix(x, wide = TRUE)
  mat[c(1:edges[1], (nrow(mat) - (edges[1] - 1)):nrow(mat)), ] <- NA
  mat[, 1:edges[2]] <- NA
  terra::values(x) <- mat
  x
}
