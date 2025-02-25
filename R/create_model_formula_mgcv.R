create_model_formula_mgcv <- function(lhs,
                                      type,
                                      bs = "tp",
                                      spatial_method = c("none", "mrf", "gp")
                                      ) {

  spatial_method <- match.arg(spatial_method)

  if (type == "hindcast") {
    vars <- c("hindcast_bbv_200",
              "hindcast_curl",
              "hindcast_ild_05",
              "hindcast_sst",
              "hindcast_eke",
              "hindcast_chl_surf",
              "hindcast_zoo_100m_int")
  } else if (type == "reanalysis") {
    vars <- c("reanalysis_bbv_200",
              "reanalysis_curl",
              "reanalysis_ild_05",
              "reanalysis_sst")
  }
  # form_spatial <- if (spatial_method != "none") {
  #   if (spatial_method == 'mrf') {
  #     "s(x, y, bs = 'mrf'))"
  #   } else if (spatial_method == 'gp') {
  #     "s(x, y, bs = 'gp', m = c(2, 3/2))"
  #   }} else NULL
  form_spatial <- if (spatial_method != "none") {
    if (spatial_method == 'mrf') {
      "s(x, y))"
    } else if (spatial_method == 'gp') {
      "s(x, y, bs = 'gp', m = c(2, 3/2))"
    }} else NULL

  form_current <- stringr::str_glue("s({type}_su, {type}_sv)")
  form_wind <- stringr::str_glue("s({type}_sustr, {type}_svstr)")
  form_base <- stringr::str_glue(
    lhs,
    stringr::str_c(" ~ offset(survey_area_km2)",
                   "platform",
                   "s(survey_id, bs = \"re\")",
                   "s(date_doy, bs = \"cc\")",
                   "s(date_decimal, bs = \"{bs}\")",
                   "s(depth, bs = \"{bs}\")",
                   "s(slope, bs = \"{bs}\")",
                   sep = " + ")
  )
  form_vars <- stringr::str_glue("s({vars}, bs = \"{bs}\")") |>
    stringr::str_flatten(collapse = " + ")
  stringr::str_c(form_base, form_spatial, form_vars, form_current, form_wind,
                 sep = " + ")
}
