correlation <- stats::cor(data_analysis$hindcast_zoo_50m_int,
                          data_analysis$hindcast_zoo_100m_int,
                          method = "pearson",
                          use = "pairwise.complete.obs")
ggplot2::ggplot(data_analysis,
                mapping = ggplot2::aes(hindcast_zoo_50m_int,
                                       y = hindcast_zoo_100m_int)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(col = "red") +
  ggplot2::annotate(geom = "text",
                    label = expression(stringr::str_c(rho, "=", correlation)),
                    x = -Inf, y = Inf, hjust = 0, vjust = 1)

data_analysis |>
  tibble::as_tibble() |>
  dplyr::select(tidyselect::starts_with("hindcast_zoo")) |>
  tidyr::drop_na() |>
  GGally::ggpairs()
