# source necessary R scripts
source("R/_functions.R")

# create dev version of dataset
set.seed(20240424)
"data/segmented-data.csv" |>
  sample_data(platform, prop = 0.1) |>
  readr::write_csv("data/segmented-data-dev.csv")

# create test version of dataset
set.seed(20240424)
"data/segmented-data.csv" |>
  sample_data(platform, prop = 0.4) |>
  readr::write_csv("data/segmented-data-test.csv")
