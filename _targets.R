# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

create_intervals_test <- function(r) {
  # r <- stars::read_ncdf(file)
  tibble::tibble(start = time(r) |>
                   lubridate::as_date() |>
                   lubridate::rollbackward(roll_to_first = TRUE) |>
                   unique(),
                 end = lubridate::rollforward(start),
                 label = format(start, "%Y-%m"))
}
create_output_test <- function(r, start, end, label) {
  library(lubridate)
  # r <- stars::read_ncdf(file)
  idx <- time(r) |>
    lubridate::date() %within% lubridate::interval(start, end) |>
    which()
  x <- r[, , , idx] |>  # dplyr::slice(time, idx) can be used if x is not a stars proxy object
    stars::st_as_stars() |>
    terra::rast() |>
    terra::mean()
  terra::set.names(x, label)
  # names(x) <- label
  terra::wrap(x)
}

list(
  tar_target(wcra31_sst_daily_file, command = "data/wcra31_sst_daily_1980_2010.nc", format = "file"),
  tar_target(wcra31_sst_daily, command = stars::read_ncdf(wcra31_sst_daily_file)),
  tar_target(intervals, command = create_intervals_test(wcra31_sst_daily)),
  tar_target(int_test, command = head(intervals)),
  tar_target(test,
             command = create_output_test(wcra31_sst_daily, start = int_test$start, end = int_test$end, label = int_test$label),
             pattern = map(int_test),
             iteration = "list"
  )
)
