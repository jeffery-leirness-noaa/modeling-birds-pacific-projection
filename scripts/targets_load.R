targets::tar_make()

targets::tar_load_globals()
tmp <- c("grid_10km", "data_bird_raw", "data_bird_10km", "data_species_info")
for (i in seq(along = tmp)) {
  assign(tmp[i], targets::tar_read_raw(tmp[i]))
}


data_bird_10km |>
  dplyr::mutate(grp = ifelse(date >= "2011-01-01", "2011_2017", "1980_2010")) |>
  janitor::tabyl(grp)

thresh <- 50
dat_temp <- data_bird_10km |>
  dplyr::mutate(grp = ifelse(date >= "2011-01-01", "2011_2017", "1980_2010")) |>
  dplyr::group_by(grp) |>
  dplyr::summarise(dplyr::across(anmu:wgwh, ~ sum(.x > 0,
                                                  na.rm = TRUE))) |>
  tidyr::pivot_longer(cols = anmu:wgwh,
                      names_to = "code",
                      names_transform = list(code = stringr::str_to_upper),
                      values_to = "cells_with_sightings") |>
  tidyr::pivot_wider(names_from = grp,
                     values_from = cells_with_sightings,
                     names_prefix = "cells_with_sightings_") |>
  dplyr::mutate(cells_with_sightings_total = cells_with_sightings_1980_2010 + cells_with_sightings_2011_2017) |>
  dplyr::filter(cells_with_sightings_total >= thresh) |>
  dplyr::inner_join(y = data_species_info, by = "code") |>
  tidyr::drop_na(sortorder) |>
  dplyr::mutate(code = stringr::str_to_lower(code),
                size_class = stringr::str_to_lower(size_class)) |>
  dplyr::select(!marine_bird) |>
  dplyr::arrange(sortorder)

dat_temp |>
  dplyr::filter(cells_with_sightings_1980_2010 < thresh) |>
  print(n = Inf)

dat_temp |>
  dplyr::mutate(dplyr::across(c(cells_with_sightings_1980_2010, cells_with_sightings_2011_2017), ~ .x / cells_with_sightings_total)) |>
  dplyr::select(code:cells_with_sightings_total, common_nm) |>
  print(n = Inf)

dat1 <- data_bird_10km |>
  dplyr::filter(date < lubridate::as_date("2011-01-01")) |>
  dplyr::pull(cell)
dat2 <- data_bird_10km |>
  dplyr::filter(date >= lubridate::as_date("2011-01-01")) |>
  dplyr::pull(cell)

r <- terra::unwrap(grid_10km)
val <- terra::values(r) |>
  tibble::as_tibble()
val$lyr.2 <- ifelse(val$lyr.1 %in% dat1, 1, 0)
val$lyr.3 <- ifelse((val$lyr.1 %in% dat2) & !(val$lyr.1 %in% dat1), 2, val$lyr.2)
terra::values(r) <- val$lyr.3
terra::plot(r)
