
Sys.setenv(TAR_PROJECT = "covariate_processing")

ptm <- Sys.time()
targets::tar_make()
Sys.time() - ptm
# Time difference of 15.40642 mins

targets::tar_manifest()
targets::tar_visnetwork(targets_only = TRUE)

targets::tar_destroy()
