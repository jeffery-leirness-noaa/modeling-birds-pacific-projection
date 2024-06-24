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

# specify path to targets datastore
store_path <- fs::path(opt$dir_out, targets::tar_config_get("store"))
targets::tar_config_set(store = store_path)
# if TAR_PROJECT is "modeling", also specify the path to "covariate_processing" targets store
if (Sys.getenv("TAR_PROJECT") == "modeling") {
  store_path_covariate_processing <- fs::path(opt$dir_out, targets::tar_config_get("store", project = "covariate_processing"))
  targets::tar_config_set(store = store_path_covariate_processing, project = "covariate_processing")
}

# run targets
targets::tar_make()
