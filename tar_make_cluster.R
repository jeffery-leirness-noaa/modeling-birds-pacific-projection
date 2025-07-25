# specify desired options in a list
option_list <- list(
  optparse::make_option(
    "--dir_in",
    type = "character",
    dest = "dir_in",
    default = "data",
    help = "Input directory to use [default %default]"
  ),
  optparse::make_option(
    "--dir_output",
    type = "character",
    dest = "dir_output",
    default = "output",
    help = "Output directory to use [default %default]"
  ),
  optparse::make_option(
    "--dir_processing",
    type = "character",
    dest = "dir_processing",
    default = "processing",
    help = "Processing directory to use [default %default]"
  ),
  optparse::make_option(
    "--dir_workspace",
    type = "character",
    dest = "dir_workspace",
    default = "dir_workspace",
    help = "Workspace working directory to use [default %default]"
  ),
  optparse::make_option(
    "--dir_targets_store",
    type = "character",
    dest = "dir_targets_store",
    default = targets::tar_path_store(),
    help = "Directory to use as the {targets} data store [default %default]"
  )
)

# get command line options
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# specify directory for targets data store
opt$dir_targets_store <- fs::path(
  opt$dir_processing,
  paste0("_targets_", gert::git_info()$shorthand)
)

# save options to temporary file that {targets} file can access
targets::tar_helper("_targets_helper.R", code = {
  opt <- !!opt
  opt <- setNames(opt, !!names(opt))
})

# set file path of the {targets} data store
targets::tar_config_set(store = opt$dir_targets_store)

# list targets that are no longer part of the pipeline
# cat("Completed targets that are no longer part of the pipeline:\n")
# targets::tar_prune_list()

# prune targets that are no longer part of the pipeline
# targets::tar_prune()

# list targets that are outdated
# cat("\nOutdated targets:\n")
# targets::tar_outdated()

# save data frame of the cue-by-cue status of each target (this may take awhile to run)
# time_stamp <- lubridate::stamp("2025-01-01-000000", quiet = TRUE)
# targets::tar_sitrep() |>
#   qs2::qs_save(
#     fs::path(
#       opt$dir_processing,
#       paste0(
#         "_targets_",
#         gert::git_info()$shorthand,
#         "_sitrep_",
#         time_stamp(lubridate::now()),
#         ".qs"
#       )
#     )
#   )

# run targets
targets::tar_make()
