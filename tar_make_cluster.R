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
  targets_cas_local <- TRUE
})

getwd()
opt
targets::tar_path_store()

# run targets
targets::tar_make()
