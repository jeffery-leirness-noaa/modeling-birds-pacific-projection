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

# copy files from blob to compute
if (fs::dir_exists(fs::path(opt$dir_out, targets::tar_config_get("store")))) {
  fs::dir_copy(fs::path(opt$dir_out, targets::tar_config_get("store")),
               new_path = targets::tar_config_get("store"),
               overwrite = TRUE)
}
# if TAR_PROJECT is "modeling", also copy "covariate_processing" targets store
if (Sys.getenv("TAR_PROJECT") == "modeling") {
  if (fs::dir_exists(fs::path(opt$dir_out, targets::tar_config_get("store", project = "covariate_processing")))) {
    fs::dir_copy(fs::path(opt$dir_out, targets::tar_config_get("store", project = "covariate_processing")),
                 new_path = targets::tar_config_get("store", project = "covariate_processing"),
                 overwrite = TRUE)
  }
}

# run targets
targets::tar_make()

# copy files from compute to blob
if (fs::dir_exists(targets::tar_config_get("store"))) {
  fs::dir_copy(targets::tar_config_get("store"),
               new_path = fs::path(opt$dir_out, targets::tar_config_get("store")),
               overwrite = TRUE)
}
