#' @title Read and evaluate a python script
#' @noRd
utils_source_python = \(oauth_func_path) {
  # Ensure Python environment is initialized
  gdverse_pyinit()

  # Define the module name and path
  module_name = gsub("\\.py$", "", basename(oauth_func_path))
  module_path = dirname(oauth_func_path)

  # Import Python module
  reticulate::import_from_path(module_name, path = module_path, convert = TRUE)
}

#' Check Python & R sync
#' @references https://github.com/r-spatial/rgee/blob/master/R/utils-auth.R
#' @noRd
gdverse_pyinit = function(){
  # Check if GDVERSE_PYTHON environment variable is set
  gdverse_python = Sys.getenv("GDVERSE_PYTHON", unset = NA)

  # Only call use_python if not already initialized
  if (reticulate::py_available()) {
    check_python_dependencies()
  } else if (is.na(gdverse_python)) {
    message("\n", "Please set `GDVERSE_PYTHON` environment variable to appropriate python path!")
    message("\n", "Run `vignette('rgdrid',package = 'gdverse')` to see more details")
  } else {
    reticulate::use_python(gdverse_python)  # Initialize Python environment if different from current one
    check_python_dependencies()
  }
}

#' Check required Python dependencies
#' @noRd
check_python_dependencies = function(){
  required_modules = c('numpy', 'pandas', 'ruptures', 'joblib')
  missing_modules = purrr::keep(required_modules, ~ !reticulate::py_module_available(.))

  if (length(missing_modules) > 0) {
    stop('Python dependencies for gdverse cannot be loaded properly. Missing: ', paste(missing_modules, collapse = ', '))
  }
}
