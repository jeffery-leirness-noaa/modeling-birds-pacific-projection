tar_load_azure_store <- function(names,
                                 branches = NULL,
                                 meta = targets::tar_meta(targets_only = TRUE,
                                                          store = store),
                                 strict = TRUE,
                                 silent = FALSE,
                                 envir = parent.frame(),
                                 store = targets::tar_config_get("store")) {
  targets::tar_load_globals()
  dir_temp <- tempdir()
  names <- tar_tidyselect_eval(rlang::enquo(names), meta$name)
  purrr::walk(
    names,
    \(x) {
      type <- dplyr::filter(meta, name == x) |>
        dplyr::pull(type)
      if (type == "pattern") {
        key <- dplyr::filter(meta, parent == x) |>
          dplyr::pull(data)
        temp <- purrr::map(key,
                           \(y) {
                             azure_download(y, path = fs::path(dir_temp, y))
                             qs::qread(fs::path(dir_temp, y))
                           })
        assign(x, temp, envir = .GlobalEnv)
      } else {
        key <- dplyr::filter(meta, name == x) |>
          dplyr::pull(data)
        azure_download(key, path = fs::path(dir_temp, key))
        assign(x, qs::qread(fs::path(dir_temp, key)), envir = .GlobalEnv)
      }
    }
  )
}

tar_tidyselect_eval <- function(names_quosure, choices) {
  if (is.null(rlang::quo_squash(names_quosure))) {
    return(NULL)
  }
  if (!length(choices)) {
    return(NULL)
  }
  names(choices) <- choices
  out <- tidyselect::eval_select(names_quosure, data = choices,
                                 strict = FALSE)
  out <- names(out)
  out
}
