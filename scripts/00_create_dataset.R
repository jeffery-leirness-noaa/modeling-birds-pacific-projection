# specify path to data used in OCS Study BOEM 2021-014
dir_in <- "D:/ccma/BIOGEO/Projects/Pacific_Seabird_Modeling/Analysis/Phase2/Data"

# load analysis datasets from OCS Study BOEM 2021-014
dat <- dir_in |>
  fs::path("segmentedData_analysisPhase2.csv") |>
  readr::read_csv()
dat_mamu <- dir_in |>
  fs::path("segmentedData_analysisPhase2_add4mamu.csv") |>
  readr::read_csv() |>
  dplyr::mutate(dplyr::across(c(anmu:ltja, mash:wgwh), ~ dplyr::na_if(., y = 0)))

# ensure species counts from dat_mamu are NA for all species except mamu
dat_mamu |>
  dplyr::summarise_at(dplyr::vars(anmu:wgwh), ~ sum(!is.na(.x))) |>
  dplyr::select(dplyr::where(~ .x > 0))

dplyr::bind_rows(dat, dat_mamu) |>
  tibble::as_tibble(.name_repair = janitor::make_clean_names) |>
  dplyr::mutate(date = lubridate::as_date(paste(year, month, day, sep = "-")),
                .before = dplyr::everything()) |>
  dplyr::select(!c(year, month, day, season, chla:index_pdo_lag12)) |>
  dplyr::arrange(date, survey_id, transect_id, segment_id) |>
  readr::write_rds(file = "data/segmented-data.rds")
