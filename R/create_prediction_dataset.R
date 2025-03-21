create_prediction_dataset <- function(path, year) {
  values_climate <- tibble::tibble(
    variable = c("bbv_200",
                 "curl",
                 "ild_05",
                 "sst",
                 "su",
                 "sustr",
                 "sv",
                 "svstr"),
    file = stringr::str_c(variable, "_daily_pcs.nc")
  )
  r <- purrr::map(
    values_climate$file,
    \(x) {
      temp <- create_targets_data_command(fs::path(path, x),
                                          local = targets_cas_local,
                                          container_name = "processing") |>
        eval()
      names(temp) <- rep(terra::varnames(temp), terra::nlyr(temp))
      terra::subset(temp, subset = lubridate::year(terra::time(temp)) == year)
    }
  ) |> terra::rast()
  terra::as.data.frame(r, xy = TRUE, cells = TRUE, time = TRUE, wide = FALSE) |>
    tibble::as_tibble() |>
    dplyr::rename(date = time) |>
    tidyr::pivot_wider(names_from = layer, values_from = values)
}
