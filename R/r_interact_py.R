#' @title Read and evaluate a python script
#' @noRd
#'
utils_source_python = \(oauth_func_path) {
  gdverse_pyinit()
  module_name = gsub("\\.py$", "", basename(oauth_func_path))
  module_path = dirname(oauth_func_path)
  reticulate::import_from_path(module_name,
                               path = module_path,
                               convert = TRUE)
}

#' Check Python & R sync
#' @references https://github.com/r-spatial/rgee/blob/master/R/utils-auth.R
#' @noRd
#'
gdverse_pyinit = function() {
  gdverse_python = Sys.getenv("GDVERSE_PYTHON", unset = NA)
  if (!is.na(gdverse_python)) {
    reticulate::use_python(gdverse_python)
    if (!all(purrr::map_lgl(
      c('numpy','pandas','ruptures','joblib'),
      reticulate::py_module_available)
      )) {
      stop('python dependencies for gdverse cannot be loaded properly.')
    }
  } else if (!all(purrr::map_lgl(
    c('numpy','pandas','ruptures','joblib'),
    reticulate::py_module_available)
    )) {
    stop('python dependencies for gdverse cannot be loaded properly.')
  } else {
    message("\n", "Please set `GDVERSE_PYTHON` environment variable to appropriate python path!")
    message("\n", "Run `vignette('rgdrid',package = 'gdverse')` to see more details")
  }
}
