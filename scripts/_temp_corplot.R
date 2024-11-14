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


# hindcast variables
cor_df_pearson <- data_analysis |>
  tibble::as_tibble() |>
  dplyr::select(tidyselect::starts_with("hindcast_"), "depth", "slope") |>
  corrr::correlate(method = "pearson")
corrr::autoplot(cor_df_pearson)
corrr::stretch(cor_df_pearson, na.rm = TRUE, remove.dups = TRUE) |>
  dplyr::filter(abs(r) > 0.6)

cor_df_spearman <- data_analysis |>
  tibble::as_tibble() |>
  dplyr::select(tidyselect::starts_with("hindcast_"), "depth", "slope") |>
  corrr::correlate(method = "spearman")
corrr::autoplot(cor_df_spearman)
corrr::stretch(cor_df_spearman, na.rm = TRUE, remove.dups = TRUE) |>
  dplyr::filter(abs(r) > 0.6)


# reanalysis variables
cor_df_pearson <- data_analysis |>
  tibble::as_tibble() |>
  dplyr::select(tidyselect::starts_with("reanalysis_"), "depth", "slope") |>
  corrr::correlate(method = "pearson")
corrr::autoplot(cor_df_pearson)
corrr::stretch(cor_df_pearson, na.rm = TRUE, remove.dups = TRUE) |>
  dplyr::filter(abs(r) > 0.6)

cor_df_spearman <- data_analysis |>
  tibble::as_tibble() |>
  dplyr::select(tidyselect::starts_with("reanalysis_"), "depth", "slope") |>
  corrr::correlate(method = "spearman")
corrr::autoplot(cor_df_spearman)
corrr::stretch(cor_df_spearman, na.rm = TRUE, remove.dups = TRUE) |>
  dplyr::filter(abs(r) > 0.6)

