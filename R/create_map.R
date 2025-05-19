#' Creat Map Panel (by Month) of Model Predictions
#'
#' This function creates a map panel of model prediction data by month and saves
#' it as a PNG file.
#'
#' @param data Data frame. Model prediction data.
#' @param model_info Data frame. Information about the model.
#' @param species_info Data frame. Information about the species.
#' @param land add
#' @param borders add
#' @param dir_out Character string. Directory to save the output file.
#'
#' @return Character string. Path to the saved file.
create_map <- function(data, model_info, species_info, study_polygon, land,
                       borders, dir_out) {
  model_info <- dplyr::filter(model_info, model_id == unique(data$model_id))
  species_info <- dplyr::filter(
    species_info,
    code == toupper(unique(model_info$code))
  )
  data$month <- factor(month.name[data$month], levels = month.name)

  file_name <- paste("maps-model-predictions", model_info$code,
                     model_info$covariate_prefix, model_info$basis,
                     model_info$mgcv_gamma, model_info$spatial_effect,
                     unique(data$esm), "monthly-climatology",
                     unique(data$period), sep = "-") |>
    stringr::str_replace_all(pattern = '_', replacement = '-')
  file_path <- fs::path(dir_out, model_info$code, file_name) |>
    fs::path_ext_set(ext = ".png")

  m <- ggplot2::ggplot() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.background = ggplot2::element_rect(fill = "gray20"),
      panel.grid = ggplot2::element_line(color = "gray10", size = 0.3)
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = species_info$common_nm) +
    ggplot2::geom_raster(
      data = data,
      mapping = ggplot2::aes(x = x, y = y, fill = .mean_pred)
    ) +
    ggplot2::geom_sf(
      data = land,
      color = "gray10",
      fill = "gray10",
      size = 0.3
    ) +
    ggplot2::geom_sf(
      data = borders[[1]],
      color = "gray20",
      fill = "gray20",
      size = 0.3
    ) +
    ggplot2::geom_sf(
      data = borders[[2]],
      color = "gray20",
      fill = "gray20",
      size = 0.3
    ) +
    # ggplot2::coord_sf(xlim = c(ext(r)[1], ext(r)[2]), ylim = c(ext(r)[3], ext(r)[4])) +
    ggplot2::coord_sf(xlim = range(data$x), ylim = range(data$y)) +
    ggplot2::facet_wrap(~ month) +
    viridis::scale_fill_viridis(
      name = NULL,
      # name = "Mean predicted density",
      option = "turbo",
      na.value = "transparent",
      limits = c(0, max(data$.mean_pred)),
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(70 / length(labels), units = "mm"),
        barheight = grid::unit(2, units = "mm")
      )
    )
  ggplot2::ggsave(
    file_path,
    plot = m,
    width = 6.5,
    height = 9.0,
    units = "in"
  )
  magick::image_read(file_path) |>
    magick::image_trim() |>
    magick::image_write(file_path)
  as.character(file_path)
}
