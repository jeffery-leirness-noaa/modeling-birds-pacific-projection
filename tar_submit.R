source("R/submit_job.R")
job <- submit_job_rfile(
  rfile = "tar_make_cluster.R",
  inputs = list(dir_in = Sys.getenv("AML_DATASTORE_RAW")),
  outputs = list(
    dir_targets_cas = paste0(
      Sys.getenv("AML_DATASTORE_PROCESSING"),
      targets::tar_path_store()
    ),
    dir_targets_store = paste0(
      Sys.getenv("AML_DATASTORE_WORKINGDIR"),
      targets::tar_path_store()
    ),
    dir_processing = Sys.getenv("AML_DATASTORE_PROCESSING")
  ),
  environment = Sys.getenv("AML_ENVIRONMENT"),
  compute = "nccos-vm-cluster-d32av4",
  experiment_name = "run-targets-tar-make",
  display_name = "run-targets-tar-make",
  description = "Run targets::tar_make() on compute cluster."
)
job$studio_url
job$status
job$update
