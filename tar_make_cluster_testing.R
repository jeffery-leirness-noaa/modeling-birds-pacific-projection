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
  optparse::make_option("--dir_workspace",
                        type = "character",
                        dest = "dir_workspace",
                        default = "dir_workspace",
                        help = "Workspace working directory to use [default %default]"),
  optparse::make_option("--dir_targets_store",
                        type = "character",
                        dest = "dir_targets_store",
                        default = targets::tar_path_store(),
                        help = "Directory to use as the {targets} data store [default %default]")
)

# get command line options
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# specify directory for targets data store
opt$dir_targets_store <- fs::path(
  opt$dir_processing,
  paste0("_targets_", gert::git_info()$shorthand, "_testing_20250529")
)

# save options to temporary file that {targets} file can access
targets::tar_helper("_targets_helper.R", code = {
  opt <- !!opt
  opt <- setNames(opt, !!names(opt))
})

# set file path of the {targets} data store
targets::tar_config_set(store = opt$dir_targets_store)

# run targets
# targets::tar_prune_list()
# targets::tar_prune()
targets::tar_make(script = "_targets_testing.R")
