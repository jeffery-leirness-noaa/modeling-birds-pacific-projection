#' Create a Data Frame of Species to Model
#'
#' This function identifies which species have sufficient data for modeling based on a
#' threshold number of cells with sightings, and returns information about those species.
#'
#' @param data Data frame. Bird observation data with species columns.
#' @param species_info_df Data frame. Contains metadata about species, including their codes.
#' @param threshold Numeric. Minimum number of cells with sightings required to include a species.
#'
#' @return A data frame of species meeting the sighting threshold, with associated metadata.
create_species_to_model_df <- function(data, species_info_df, threshold) {
  dplyr::summarise(data,
                   dplyr::across(anmu:wgwh, ~ sum(.x > 0,
                                                  na.rm = TRUE))) |>
    tibble::as_tibble() |>
    dplyr::select(!geometry) |>
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "code",
                        names_transform = list(code = stringr::str_to_upper),
                        values_to = "cells_with_sightings") |>
    dplyr::filter(cells_with_sightings >= threshold) |>
    dplyr::inner_join(y = species_info_df, by = "code") |>
    tidyr::drop_na(sortorder) |>
    dplyr::mutate(code = stringr::str_to_lower(code),
                  size_class = stringr::str_to_lower(size_class)) |>
    dplyr::select(!marine_bird) |>
    dplyr::arrange(sortorder)
}
