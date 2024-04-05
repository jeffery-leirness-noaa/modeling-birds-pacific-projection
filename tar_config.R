# configure separate targets projects
targets::tar_config_set(script = "targets_covariate_processing.R",
                        store = "_targets_covariate_processing",
                        project = "covariate_processing")
targets::tar_config_set(script = "targets_modeling.R",
                        store = "_targets_modeling",
                        project = "modeling")
