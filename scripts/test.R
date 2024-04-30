# get configuration values
config <- config::get(file = "config.yaml")

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
# dir_out <- opt$dir_out

# load mtcars dataset
data(mtcars)

# print
# print(mtcars)
# print(getwd())
# print(fs::dir_ls())
# print(fs::dir_ls("data"))
print(paste("opt$dir_in:", opt$dir_in))
print(paste("opt$dir_out:", opt$dir_out))
print(paste("R_CONFIG_ACTIVE:", Sys.getenv("R_CONFIG_ACTIVE")))
print(paste("TAR_PROJECT:", Sys.getenv("TAR_PROJECT")))
print(paste("conf$dataset:", config$dataset))

# save as RDS file
# saveRDS(mtcars, file.path(dir_out, "mtcars.rds"))
# saveRDS(mtcars, "data/test_mtcars.rds")
