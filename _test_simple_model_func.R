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
                        help = "Output directory to use [default %default]"),
  optparse::make_option("--sp",
                        type = "character",
                        dest = "sp",
                        default = "bfal",
                        help = "Species code to use [default %default]")
)

# get command line options
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# get configuration values
config <- config::get(file = "config.yaml")

# specify path to "covariate_processing" targets datastore
store_path_covariate_processing <- fs::path(opt$dir_out,
                                            targets::tar_config_get("store", project = "covariate_processing") |>
                                              fs::path_file())
targets::tar_config_set(store = store_path_covariate_processing, project = "covariate_processing")

# specify data
data <- targets::tar_read_raw(config$target, store = targets::tar_config_get("store", project = "covariate_processing"))

# run model
simple_model_func <- function(data, sp, dayofyear_k = -1, mgcv_gamma = 1, basis = "tp") {
  form <- count ~ platform +
    # s(julianday, bs = basis) +
    # s(dayofyear, bs = "cc", k = dayofyear_k) +
    s(depth, bs = basis)
  mgcv::gam(form,
            data = dplyr::rename(data, "count" = tolower(sp)),
            family = mgcv::nb(),
            gamma = mgcv_gamma)
}
simple_model_func(data = data, sp = opt$sp) |>
  readr::write_rds(file = fs::path(opt$dir_out, paste0(opt$sp, ".rds")))
