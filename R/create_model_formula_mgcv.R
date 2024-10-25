create_model_formula_mgcv <- function(lhs,
                                      type,
                                      bs = "tp",
                                      spatial_random_effect = FALSE) {
  if (type == "hindcast") {
    vars <- c("hindcast_bbv_200",
              "hindcast_curl",
              "hindcast_ild_05",
              "hindcast_ssh",
              "hindcast_sst",
              "hindcast_su",
              "hindcast_sv",
              "hindcast_sustr",
              "hindcast_svstr",
              "hindcast_eke",
              "hindcast_chl_surf",
              "hindcast_zoo_100m_int")
  } else if (type == "reanalysis") {
    vars <- c("reanalysis_bbv_200",
              "reanalysis_curl",
              "reanalysis_ild_05",
              "reanalysis_ssh",
              "reanalysis_sst",
              "reanalysis_su",
              "reanalysis_sv",
              "reanalysis_sustr",
              "reanalysis_svstr")
  }
  form_spatial <- if (spatial_random_effect) "s(x, y)" else NULL
  form_base <- stringr::str_glue(
    lhs,
    stringr::str_c(" ~ offset(survey_area_km2)",
                   "platform",
                   "s(survey_id, bs = \"re\")",
                   "s(date_doy, bs = \"cc\")",
                   "s(date_decimal, bs = \"{bs}\")",
                   sep = " + ")
  )
  form_vars <- stringr::str_glue("s({vars}, bs = \"{bs}\")") |>
    stringr::str_flatten(collapse = " + ")
  stringr::str_c(form_base, form_spatial, form_vars, sep = " + ")
}
