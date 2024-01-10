fit_model <- function(sp, dayofyear.k, mgcv.gamma, basis,
                      data.nm = "segmented_data_analysis_2023-02-15.rds") {

  # specify general file name
  file.nm <- paste0(tolower(sp), "_k", dayofyear.k, "_gamma", mgcv.gamma, "_", basis)

  # source model_formula.R script
  source("scripts/model_formula.R", local = TRUE)

  # modify maximum dimension of the basis ("k" argument) for dayofyear term
  if (dayofyear.k != -1) {
    old <- attr(terms(form.mu), "term.labels")[grep("dayofyear", attr(terms(form.mu), "term.labels"))]
    new <- gsub(")", paste0(", k = ", dayofyear.k, ")"), old)
    form.mu <- as.character(form.mu)
    form.mu[2] <- gsub(old, new, form.mu[2], fixed = TRUE)
    form.mu <- formula(paste(form.mu, collapse = ""))
  }

  # modify the basis function for non-cyclic smooths
  form.mu <- as.character(form.mu)
  form.mu[2] <- gsub("@basis", basis, form.mu[2], fixed = TRUE)
  form.mu <- formula(paste(form.mu, collapse = ""))

  # specify the base (i.e., minimal) model formula
  ex <- attr(terms(form.mu), "term.labels")[-grep("platform|sid|julianday|dayofyear", attr(terms(form.mu), "term.labels"))]
  form.base <- as.character(form.mu)
  for (i in seq(along = ex)) {
    form.base[length(form.base)] <- gsub(paste("+", ex[i]), "", form.base[length(form.base)], fixed = TRUE)
  }
  form.base <- formula(paste(form.base, collapse = ""))

  # source variable selection scripts
  source("scripts/stepGAIC.test.R")
  source("scripts/stepGAICAll.A.test.R")

  # load analysis dataset
  dat <- readRDS(file.path("data", data.nm))

  # specify the species to be modeled
  names(dat)[names(dat) == toupper(sp)] <- "count"

  # create log file to capture output
  sink(here(dir.out, paste0(file.nm, ".Rout")))

  # step-wise variable selection --------------------------------------------
  cat("Output: initialization\n")
  ptm_ini <- Sys.time()
  m0 <- mgcv::gam(update(form.base, count ~ .), data = dat, family = mgcv::nb(), gamma = mgcv.gamma)
  m0.1 <- parsnip::gen_additive_mod(adjust_deg_free = mgcv.gamma) |>
    parsnip::set_engine("mgcv", family = nb()) |>
    parsnip::set_mode("regression") |>
    parsnip::fit(update(form.base, count ~ .), data = dat)
  sessioninfo::session_info()

  tm_ini <- difftime(Sys.time(), ptm_ini)
  cat("\nOutput: variable selection\n")
  ptm_fit <- Sys.time()
  m0$par <- "mu"
  m1 <- stepGAIC.test(m0,
                      scope = list(lower = m0$formula, upper = form.mu),
                      parallel = "multicore", ncpus = getOption("mc.cores"))
  tm_fit <- difftime(Sys.time(), ptm_fit)
  tm_tot <- as.numeric(sum(c(tm_ini, tm_fit)), units = "hours")
  if (exists("m1")) saveRDS(m1, here(dir.out, paste0(file.nm, ".rds")))

  # stop sending output to file
  sink()

  # create log file to document model run -----------------------------------
  out_fit <- readLines(here(dir.out, paste0(file.nm, ".Rout")))
  sink(here(dir.out, paste0(file.nm, ".log")))
  cat("Model completed on", date())
  cat("\nTotal run time:", tm_tot, "hours")
  cat("\n  Initialization.......", tm_ini, units(tm_ini))
  cat("\n  Variable selection...", tm_fit, units(tm_fit))
  cat("\nNumber of cores allocated:", getOption("mc.cores"))
  cat("\n\n", rep("-", 80), "\n\n", sep = "")
  cat("Machine: ", Sys.getenv("COMPUTERNAME"), "\n", sep = "")
  print(sessionInfo())
  cat("\n", rep("-", 80), "\n", sep = "")
  cat("\nValue of `form`\n")
  print(form)
  cat("\nValue of `form.mu`\n")
  print(form.mu)
  cat("\nValue of `form.base`\n")
  print(form.base)
  cat("\nValue of `mgcv.gamma`\n")
  print(mgcv.gamma)
  cat("\nAnalysis dataset used: `", dat.nm, "`", sep = "")
  cat("\n\n", rep("-", 80), "\n\n", sep = "")
  writeLines(out_fit)
  sink()
  invisible(file.remove(here(dir.out, paste0(file.nm, ".Rout"))))

}


diagnostic_plot <- function(model) {
  # marginal effects plots (i.e., term plots) with standard errors
  plot(m1, rug = TRUE, pages = 1, scale = 0, shade = TRUE)

  # marginal effects plots (i.e., term plots) without standard errors
  plot(m1, rug = TRUE, se = FALSE, pages = 1, scale = 0)
}
