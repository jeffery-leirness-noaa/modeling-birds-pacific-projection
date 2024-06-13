# specify desired options in a list
option_list <- list(
  optparse::make_option("--dir_in",
                        type = "character",
                        dest = "dir_in",
                        default = "data",
                        help = "Input directory to use [default %default]"),
  optparse::make_option("--dir_out",
                        type = "character",
                        dest = "dir_out",
                        default = "output",
                        help = "Output directory to use [default %default]")
)

# get command line options
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# save options to temporary file that targets file can access
targets::tar_helper("_targets_helper.R", code = {
  opt <- !!opt
  opt <- setNames(opt, !!names(opt))
})

# # copy files from blob to compute
# fs::dir_copy(fs::path(opt$dir_out, targets::tar_config_get("store")),
#              new_path = targets::tar_config_get("store"),
#              overwrite = TRUE)
#
# # Sys.setenv(TAR_PROJECT = "covariate_processing")
#
# # ptm <- Sys.time()
# targets::tar_make()
# # Sys.time() - ptm
# # Time difference of 15.40642 mins
#
# # targets::tar_manifest()
# # targets::tar_visnetwork(targets_only = TRUE)
# #
# # targets::tar_destroy()
#
# # copy files from compute to blob
# fs::dir_copy(targets::tar_config_get("store"),
#              new_path = fs::path(opt$dir_out, targets::tar_config_get("store")),
#              overwrite = TRUE)
