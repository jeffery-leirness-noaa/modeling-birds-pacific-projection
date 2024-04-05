
Sys.setenv(TAR_PROJECT = "covariate_processing")

ptm <- Sys.time()
targets::tar_make()
Sys.time() - ptm
# Time difference of 3.870845 mins
# Time difference of 5.045395 mins for reading in netcdf file each time
# Time difference of 2.936815 mins for reading in netcdf file each time, but with 10 workers

man <- targets::tar_manifest()
man
targets::tar_visnetwork(targets_only = TRUE)

targets::tar_destroy()

ptm <- Sys.time()
targets::tar_make()
Sys.time() - ptm
#
