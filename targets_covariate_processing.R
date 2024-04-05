# load targets package
library(targets)

# set target options
tar_option_set(
  format = "qs"
)

# source necessary R scripts
tar_source("R/_functions.R")

list(
  tar_target(wcra31_sst_daily_file, command = "data/wcra31_sst_daily_1980_2010.nc", format = "file"),
  tar_target(wcra31_sst_daily, command = stars::read_ncdf(wcra31_sst_daily_file)),
  tar_target(int_monthly, command = create_intervals_monthly(wcra31_sst_daily)),
  # tar_target(int_test, command = head(int_monthly)),
  tar_target(covariates_wcra31_sst,
             command = create_covariate_output(wcra31_sst_daily,
                                               start = int_monthly$start,
                                               end = int_monthly$end,
                                               fname = "wcra31-sst-monthly",
                                               label = int_monthly$label),
             pattern = map(intervals),
             format = "file",
             iteration = "vector"
  )
)
