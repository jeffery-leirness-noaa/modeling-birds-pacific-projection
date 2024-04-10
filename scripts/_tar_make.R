
Sys.setenv(TAR_PROJECT = "covariate_processing")

ptm <- Sys.time()
targets::tar_make()
Sys.time() - ptm
# Time difference of

man <- targets::tar_manifest()
man
targets::tar_visnetwork(targets_only = TRUE)

targets::tar_destroy()

ptm <- Sys.time()
targets::tar_make()
Sys.time() - ptm
#
