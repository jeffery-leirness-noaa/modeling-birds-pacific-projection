source("R/submit_job.R")
config <- config::get(file = "config.yaml")
submit_job_rfile(
  rfile = "tar_make_cluster.R",
  inputs = list(dir_in = Sys.getenv("AML_DATASTORE_RAW")),
  outputs = list(
    dir_targets_cas = paste0(
      Sys.getenv("AML_DATASTORE_PROCESSING"),
      targets::tar_path_store()
    ),
    dir_targets_store = paste0(
      Sys.getenv("AML_DATASTORE_WORKINGDIR"),
      targets::tar_path_store())
  ),
  environment = Sys.getenv("AML_ENVIRONMENT"),
  compute = "nccos-vm-cluster-ds2",
  experiment_name = "run-targets-tar-make",
  display_name = "run-targets-tar-make-data-prep",
  description = "Run targets::tar_make() on compute cluster."
)
