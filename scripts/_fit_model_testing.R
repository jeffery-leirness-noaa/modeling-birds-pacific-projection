
# get configuration values
config <- config::get(file = "config.yaml")

dat <- targets::tar_read_raw(config$target, store = targets::tar_config_get("store", project = "covariate_processing"))

sp <- "bfal"

dat$x4 <- runif(nrow(dat), 0, 1)
dat$x5 <- runif(nrow(dat), 0, 1)
dat$x6 <- runif(nrow(dat), 0, 1)

form <- count ~ platform + s(depth, bs = "tp") + s(x4, bs = "tp")
m1 <- mgcv::gam(form,
                data = dplyr::rename(dat, "count" = tolower(sp)),
                family = mgcv::nb())

form <- count ~ platform + s(depth, bs = "tp") + s(x4, bs = "tp")
m2 <- mgcv::gam(form,
                data = dplyr::rename(dat, "count" = tolower(sp)),
                family = mgcv::nb(),
                select = TRUE)

form <- count ~ platform + s(depth, bs = "ts") + s(x4, bs = "ts")
m3 <- mgcv::gam(form,
                data = dplyr::rename(dat, "count" = tolower(sp)),
                family = mgcv::nb())

form <- count ~ platform + s(depth, bs = "ts") + s(x4, bs = "ts")
m4 <- mgcv::gam(form,
                data = dplyr::rename(dat, "count" = tolower(sp)),
                family = mgcv::nb(),
                select = TRUE)



form <- count ~ platform + s(depth, bs = "tp") + s(x4, bs = "tp") + s(x5, bs = "tp") + s(x6, bs = "tp")
m1b <- parsnip::gen_additive_mod() |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, "count" = tolower(sp)))

form <- count ~ platform + s(depth, bs = "tp") + s(x4, bs = "tp") + s(x5, bs = "tp") + s(x6, bs = "tp")
m2b <- parsnip::gen_additive_mod(select_features = TRUE) |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, "count" = tolower(sp)))

form <- count ~ platform + s(depth, bs = "ts") + s(x4, bs = "ts") + s(x5, bs = "ts") + s(x6, bs = "ts")
m3b <- parsnip::gen_additive_mod() |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, "count" = tolower(sp)))

form <- count ~ platform + s(depth, bs = "ts") + s(x4, bs = "ts") + s(x5, bs = "ts") + s(x6, bs = "ts")
m4b <- parsnip::gen_additive_mod() |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, "count" = tolower(sp)))


broom::tidy(m1b)
broom::tidy(m2b)
broom::tidy(m3b)
broom::tidy(m4b)


data(gam.data, package = "gam")
Gam.object <- gam::gam(y ~ x + z, data = gam.data)
step.object <- gam::step.Gam(Gam.object, scope = list("x" = ~ 1 + x + s(x, 4) + s(x, 6) + s(x, 12), "z" = ~ 1 + z + s(z, 4)))

data(gam.data, package = "gam")
Gam.object <- mgcv::gam(y ~ x + z, data = gam.data)
step.object <- MASS::stepAIC(Gam.object, scope = list("x" = ~ 1 + x + s(x, 4) + s(x, 6) + s(x, 12), "z" = ~ 1 + z + s(z, 4)))


form <- count ~ platform +
  s(depth, bs = "ts", k = 5) +
  s(dcanyon, bs = "ts", k = 5) +
  s(slp10km, bs = "ts", k = 5) +
  s(bbv, bs = "ts", k = 5) +
  s(curl, bs = "ts", k = 5) +
  s(ild, bs = "ts", k = 5) +
  s(ssh, bs = "ts", k = 5) +
  s(sst, bs = "ts", k = 5)
mgcv_gamm <- 1
mod <- parsnip::gen_additive_mod(adjust_deg_free = mgcv_gamma) |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, "count" = tolower(sp)))
summary(mod$fit)
mgcv::plot.gam(mod$fit, pages = 1, scale = 0)

mgcv_gamma <- 1.4
mod <- parsnip::gen_additive_mod(adjust_deg_free = mgcv_gamma) |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, "count" = tolower(sp)))
summary(mod$fit)
mgcv::plot.gam(mod$fit, pages = 1, scale = 0)
