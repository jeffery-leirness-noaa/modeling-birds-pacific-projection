targets::tar_make()

targets::tar_load_globals()
tmp <- c("grid_10km", "data_bird_raw", "data_bird_10km")
for (i in seq(along = tmp)) {
  assign(tmp[i], targets::tar_read_raw(tmp[i]))
}
