# configure separate targets projects
targets::tar_config_set(script = "targets_covariate_processing.R",
                        store = "covariate_processing",
                        project = "covariate_processing")
targets::tar_config_set(script = "targets_modeling.R",
                        store = "modeling",
                        project = "modeling")
