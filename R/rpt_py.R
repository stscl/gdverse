#' @title set up `gdverse` python dependence
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Create an isolated Python virtual environment with all `gdverse` dependencies.
#'
#'
#' @param py_env The name, or full path, of the environment in which Python packages are to be installed.
#' @param py_version (optional) The requested Python version.Default is 3.9.
#' @param method (optional) Installation method. By default, "auto" automatically finds a method that will
#' work in the local environment.
#' @param ... Other parameers passed to `reticulate::py_install()`
#'
#' @importFrom reticulate py_install
#' @export
#'
install_rpt = \(py_env = "r-gdverse",
                py_version = 3.9,
                method = "auto", ...) {
  reticulate::py_install(packages = c("numpy","pandas","ruptures"),
                         envname = py_env,
                         method = method,
                         python_version = py_version,...)
}
