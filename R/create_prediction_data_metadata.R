create_prediction_data_metadata <- function(.path, path_remove = NULL) {
  df <- tibble::tibble(
    path = fs::dir_ls(.path, recurse = TRUE, type = "file", glob = "*.nc"),
    esm = fs::path_dir(path) |>
      fs::path_dir() |>
      fs::path_file(),
    variable = fs::path_dir(path) |>
      fs::path_file(),
    year = fs::path_file(path) |>
      fs::path_ext_remove() |>
      stringr::str_split_i(pattern = "_", i = 1) |>
      as.numeric(),
    day = fs::path_file(path) |>
      fs::path_ext_remove() |>
      stringr::str_split_i(pattern = "_", i = 2) |>
      as.numeric(),
    date = lubridate::as_date(day, origin = lubridate::as_date(stringr::str_c(year, "-01-01")) - 1)
  ) |>
    dplyr::select(esm, variable, year, date, path)
  if (!is.null(path_remove)) {
    df <- dplyr::mutate(
      df,
      path = stringr::str_replace(path, pattern = paste0(path_remove, "/"),
                                  replacement = "")
    )
  }
  df
}
