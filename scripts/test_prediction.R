
ck <- data |> tibble::as_tibble()
ck$date_doy <- lubridate::yday(ck$date) |> 
  scale()
ck$date_decimal <- lubridate::decimal_date(ck$date) |> 
  scale()
ck$survey_area_km2 <- scale(log(ck$survey_area_km2_lg), center = TRUE, scale = FALSE)

vars <- c("hindcast_bbv_200",
          "hindcast_curl",
          "hindcast_ild_05",
          "hindcast_ssh",
          "hindcast_sst",
          "hindcast_su",
          "hindcast_sustr",
          "hindcast_sv",
          "hindcast_svstr",
          "hindcast_zoo_200m_int",
          "hindcast_zoo_100m_int",
          "hindcast_zoo_50m_int",
          "hindcast_chl_surf",
          "hindcast_eke")
for (i in seq(along = vars)) {
  ck[, vars[i]] <- scale(ck[, vars[i]])
}

model_formula <- susc ~ offset(survey_area_km2) + 
  platform + s(survey_id, bs = "re") + 
  s(date_doy, bs = "cc") + 
  s(date_decimal, bs = "tp") + 
  s(hindcast_bbv_200, bs = "tp") + 
  s(hindcast_curl, bs = "tp") + 
  s(hindcast_ild_05, bs = "tp") + 
  s(hindcast_ssh, bs = "tp") + 
  s(hindcast_sst, bs = "tp") + 
  s(hindcast_su, bs = "tp") + 
  s(hindcast_sustr, bs = "tp") + 
  s(hindcast_sv, bs = "tp") + 
  s(hindcast_svstr, bs = "tp") + 
  s(hindcast_zoo_200m_int, bs = "tp") + 
  s(hindcast_zoo_100m_int, bs = "tp") + 
  s(hindcast_zoo_50m_int, bs = "tp") + 
  s(hindcast_chl_surf, bs = "tp") + 
  s(hindcast_eke, bs = "tp")

mod <- mgcv::gam(model_formula, family = mgcv::nb(), data = ck, select = TRUE)

predict(mod, ck[1:10, ], type = "response", exclude = "s(survey_id)")

