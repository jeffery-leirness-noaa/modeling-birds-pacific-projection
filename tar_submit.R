source("R/submit_job.R")
config <- config::get(file = "config.yaml")
submit_job_rfile(rfile = "tar_make_cluster.R",
                 inputs = list(dir_in = Sys.getenv("AML_DATASTORE_RAW")),
                 outputs = list(dir_targets_cas = paste0(Sys.getenv("AML_DATASTORE_PROCESSING"), targets::tar_path_store()),
                                dir_targets_store = paste0(Sys.getenv("AML_DATASTORE_WORKINGDIR"), targets::tar_path_store())),
                 environment = config$aml_env,
                 compute = "nccos-vm-cluster-ds2",
                 experiment_name = "test-targets-tar-make",
                 display_name = "test-targets-tar-make",
                 description = "Test running targets::tar_make() on compute cluster.")
