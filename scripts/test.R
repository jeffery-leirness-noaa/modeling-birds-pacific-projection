# specify desired options in a list
option_list <- list(
  optparse::make_option("--dir_out",
                        type = "character",
                        dest = "dir_out",
                        default = "output",
                        help = "Output directory to use [default %default]")
)

# get command line options
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
dir_out <- opt$dir_out

# load mtcars dataset
data(mtcars)

# print
print(mtcars)

# save as RDS file
saveRDS(mtcars, file.path(dir_out, "mtcars.rds"))
