# specify desired options in a list
option_list <- list(
  optparse::make_option("--dir_in",
                        type = "character",
                        dest = "dir_in",
                        default = "data",
                        help = "Input directory to use [default %default]"),
  optparse::make_option("--dir_output",
                        type = "character",
                        dest = "dir_output",
                        default = "output",
                        help = "Output directory to use [default %default]"),
  optparse::make_option("--dir_processing",
                        type = "character",
                        dest = "dir_processing",
                        default = "processing",
                        help = "Processing directory to use [default %default]"),
  optparse::make_option("--dir_targets_cas",
                        type = "character",
                        dest = "dir_targets_cas",
                        default = "dir_targets_cas",
                        help = "Directory to use as the {targets} CAS [default %default]"),
  optparse::make_option("--dir_targets_store",
                        type = "character",
                        dest = "dir_targets_store",
                        default = targets::tar_path_store(),
                        help = "Directory to use as the {targets} data store [default %default]")
)

# get command line options
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# save options to temporary file that {targets} file can access
targets::tar_helper("_targets_helper.R", code = {
  opt <- !!opt
  opt <- setNames(opt, !!names(opt))
  targets_cas_local <- TRUE
})

# set file path of the {targets} data store
targets::tar_config_set(store = opt$dir_targets_store)

# run targets
targets::tar_make()
