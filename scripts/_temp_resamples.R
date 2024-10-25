
rsample::training(data_analysis_split) |>
  dplyr::arrange(date, cell, survey_id) |>
  rsample::sliding_period(index = date, period = "year", every = 5) |>
  dplyr::pull(splits) |>
  purrr::map(\(x) rsample::analysis(x) |>
               tibble::as_tibble() |>
               dplyr::summarise(year_min = min(lubridate::year(date)),
                                year_max = max(lubridate::year(date)))) |>
  purrr::list_rbind() |>
  print(n = Inf)

rsample::training(data_analysis_split) |>
  dplyr::arrange(date, cell, survey_id) |>
  rsample::sliding_period(index = date, period = "year", every = 5) |>
  dplyr::pull(splits) |>
  purrr::map(\(x) rsample::assessment(x) |>
               tibble::as_tibble() |>
               dplyr::summarise(year_min = min(lubridate::year(date)),
                                year_max = max(lubridate::year(date)))) |>
  purrr::list_rbind() |>
  print(n = Inf)

rsample::training(data_analysis_split) |>
  dplyr::arrange(date, cell, survey_id) |>
  dplyr::mutate(year = lubridate::year(date)) |>
  rsample::group_vfold_cv(group = year, v = 5) |>
  dplyr::pull(splits) |>
  purrr::map(\(x) rsample::analysis(x) |>
               janitor::tabyl(year)) |>
  purrr::list_rbind() |>
  print(n = Inf)

rsample::training(data_analysis_split) |>
  dplyr::arrange(date, cell, survey_id) |>
  dplyr::mutate(year = lubridate::year(date)) |>
  tidyr::nest(.by = year) |>
  rsample::rolling_origin()

data_analysis |>
  dplyr::mutate(pre2011 = dplyr::if_else(date < "2011-01-01", TRUE, FALSE)) |>
  janitor::tabyl(pre2011)

data_analysis |>
  dplyr::mutate(pre2011 = dplyr::if_else(date < "2011-01-01", TRUE, FALSE)) |>
  tidyr::nest(.by = pre2011)

breaks <- c(0, rev(seq(-1, 0, by = 0.15) * -1))
data_analysis$group <- cut(1:nrow(data_analysis) / nrow(data_analysis), breaks = breaks)

ck <- data_analysis |>
  dplyr::mutate(group = cut(1:nrow(data_analysis) / nrow(data_analysis),
                            breaks = c(0, rev(seq(-1, 0, by = 0.15) * -1)))) |>
  tidyr::nest(.by = group) |>
  rsample::rolling_origin(initial = 1)

rsample::analysis(ck$splits[[2]]) |>
  tidyr::unnest(cols = data)

purrr::map_vec(ck$splits,
               \(x) rsample::analysis(x) |>
                 tidyr::unnest(cols = data) |>
                 nrow())

# use ck as analysis sets and use the compliment for assessment
# see rsample::manual_rset() and possibly rsample::compliment()
