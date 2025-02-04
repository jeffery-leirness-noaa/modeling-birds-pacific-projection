
library(sf)

# get configuration values
config <- config::get(file = "config.yaml")

dat <- targets::tar_read_raw(config$target, store = targets::tar_config_get("store", project = "covariate_processing"))

sp <- "bfal"

dat$x4 <- runif(nrow(dat), 0, 1)
dat$x5 <- runif(nrow(dat), 0, 1)
dat$x6 <- runif(nrow(dat), 0, 1)

form <- count ~ platform + s(depth, bs = "tp") + s(x4, bs = "tp")
m1 <- mgcv::gam(form,
                data = dplyr::rename(dat, count = tolower(sp)),
                family = mgcv::nb())

form <- count ~ platform + s(depth, bs = "tp") + s(x4, bs = "tp")
m2 <- mgcv::gam(form,
                data = dplyr::rename(dat, count = tolower(sp)),
                family = mgcv::nb(),
                select = TRUE)

form <- count ~ platform + s(depth, bs = "ts") + s(x4, bs = "ts")
m3 <- mgcv::gam(form,
                data = dplyr::rename(dat, count = tolower(sp)),
                family = mgcv::nb())

form <- count ~ platform + s(depth, bs = "ts") + s(x4, bs = "ts")
m4 <- mgcv::gam(form,
                data = dplyr::rename(dat, count = tolower(sp)),
                family = mgcv::nb(),
                select = TRUE)



form <- count ~ platform + s(depth, bs = "tp") + s(x4, bs = "tp") + s(x5, bs = "tp") + s(x6, bs = "tp")
m1b <- parsnip::gen_additive_mod() |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, count = tolower(sp)))

form <- count ~ platform + s(depth, bs = "tp") + s(x4, bs = "tp") + s(x5, bs = "tp") + s(x6, bs = "tp")
m2b <- parsnip::gen_additive_mod(select_features = TRUE) |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, count = tolower(sp)))


form <- paste(sp, '~ platform + s(depth, bs = "tp") + s(x4, bs = "tp") + s(x5, bs = "tp") + s(x6, bs = "tp")') |>
  as.formula()
m2b <- parsnip::gen_additive_mod(select_features = TRUE) |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dat)


form <- paste(sp, '~ platform + s(depth, bs = "tp") + s(x4, bs = "tp") + s(x5, bs = "tp") + s(x6, bs = "tp")') |>
  as.formula()
gam_model <- parsnip::gen_additive_mod(select_features = TRUE) |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression")

gam_workflow <- workflows::workflow() |>
  workflows::add_variables(outcome = bfal,
                           predictors = c(platform, depth, x4, x5, x6)) |>
  workflows::add_model(gam_model, formula = form)

gam_fit <- parsnip::fit(gam_workflow, data = dat |> tibble::as_tibble())


# things to test:
# 1. will predict() work on saved (.rds) mgcv object that is loaded without the data?
# 2. will predict() work on saved (.rds) parsnip object that is loaded without the data?
# 3. will predict() work on saved (.rds) workflows object that is loaded without the data?
library(sf)
config <- config::get(file = "config.yaml")
dat <- targets::tar_read_raw(config$target, store = targets::tar_config_get("store", project = "covariate_processing"))
sp <- "bfal"
dat$x4 <- runif(nrow(dat), 0, 1)
dat$x5 <- runif(nrow(dat), 0, 1)
dat$x6 <- runif(nrow(dat), 0, 1)
dat$sid <- stringr::str_split_i(dat$survey_id, pattern = "_", i = 1) |>
  as.factor()
form <- paste(sp, '~ platform + s(sid, bs = "re") + s(depth, bs = "ts") + s(x4, bs = "ts") + s(x5, bs = "ts") + s(x6, bs = "ts")') |>
  as.formula()
m1_mgcv <- mgcv::gam(form, data = dat, family = mgcv::nb())
gam_model <- parsnip::gen_additive_mod() |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression")
m1_parsnip <- parsnip::fit(gam_model, formula = form, data = dat)
gam_workflow <- workflows::workflow() |>
  workflows::add_variables(outcome = bfal,
                           predictors = c(platform, sid, depth, x4, x5, x6)) |>
  workflows::add_model(gam_model, formula = form)
m1_workflows <- parsnip::fit(gam_workflow, data = dat |> tibble::as_tibble())

saveRDS(m1_mgcv, "data/test-model-mgcv.rds")
saveRDS(m1_parsnip, "data/test-model-parsnip.rds")
saveRDS(m1_workflows, "data/test-model-workflows.rds")

# restart R session, then do:
m1_mgcv <- readRDS("data/test-model-mgcv.rds")
m1_parsnip <- readRDS("data/test-model-parsnip.rds")
m1_workflows <- readRDS("data/test-model-workflows.rds")
data_test <- tibble::tibble(platform = "boat",
                            sid = "CAC",
                            depth = -1000,
                            x4 = runif(20, 0, 1),
                            x5 = runif(20, 0, 1),
                            x6 = runif(20, 0, 1))
predict(m1_mgcv, newdata = data_test, type = "response", exclude = "s(sid)")
predict(m1_parsnip, new_data = data_test, type = "raw", opts = list(type = "response", exclude = "s(sid)"))
predict(m1_workflows, new_data = data_test, type = "raw", opts = list(type = "response", exclude = "s(sid)"))


form <- count ~ platform + s(depth, bs = "ts") + s(x4, bs = "ts") + s(x5, bs = "ts") + s(x6, bs = "ts")
m3b <- parsnip::gen_additive_mod() |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, count = tolower(sp)))

form <- count ~ platform + s(depth, bs = "ts") + s(x4, bs = "ts") + s(x5, bs = "ts") + s(x6, bs = "ts")
m4b <- parsnip::gen_additive_mod() |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, count = tolower(sp)))


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
  parsnip::fit(form, data = dplyr::rename(dat, count = tolower(sp)))
summary(mod$fit)
mgcv::plot.gam(mod$fit, pages = 1, scale = 0)

mgcv_gamma <- 1.4
mod <- parsnip::gen_additive_mod(adjust_deg_free = mgcv_gamma) |>
  parsnip::set_engine("mgcv", family = mgcv::nb()) |>
  parsnip::set_mode("regression") |>
  parsnip::fit(form, data = dplyr::rename(dat, count = tolower(sp)))
summary(mod$fit)
mgcv::plot.gam(mod$fit, pages = 1, scale = 0)


targets::tar_load_globals()
tar_load_azure_store("data_analysis")
data_analysis$date_doy <- lubridate::yday(data_analysis$date)

m1 <- mgcv::gam(susc ~ s(date_doy, bs = "cc"),
                data = data_analysis,
                family = mgcv::nb())
mgcv::plot.gam(m1, rug = TRUE, se = TRUE, pages = 1, scale = 0, shade = TRUE)

m2 <- mgcv::gam(susc ~ s(date_doy, bs = "cc"),
                data = data_analysis,
                family = mgcv::nb(),
                knots = list(date_doy = c(1, 366)))
mgcv::plot.gam(m2, rug = TRUE, se = TRUE, pages = 1, scale = 0, shade = TRUE)

newdata <- tibble::tibble(date_doy = 1:366)
m1_pred <- tibble::tibble(model = "m1", .pred = mgcv::predict.gam(m1, newdata = newdata, type = "link")) |>
  dplyr::bind_cols(newdata)
m2_pred <- tibble::tibble(model = "m2", .pred = mgcv::predict.gam(m2, newdata = newdata, type = "link")) |>
  dplyr::bind_cols(newdata)
preds <- dplyr::bind_rows(m1_pred, m2_pred)

ggplot2::ggplot(data = preds, mapping = ggplot2::aes(date_doy, .pred, group = model, color = model)) +
  ggplot2::geom_line()
