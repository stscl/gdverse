#' @title univariate discretization based on offline change point detection
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Determines discretization interval breaks using an optimization algorithm for variance-based
#' change point detection.
#'
#' @note
#' Please set up python dependence and configure `GDVERSE_PYTHON` environment variable if you want to run `robust_disc()`.
#' See `vignette('rgdrid',package = 'gdverse')` for more details.
#'
#' @param formula A formula of univariate discretization.
#' @param data A data.frame or tibble of observation data.
#' @param discnum A numeric vector of discretized classes of columns that need to be discretized.
#' @param minsize (optional) The min size of each discretization group. Default all use `1`.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, use `python` `joblib` package to
#' parallel computation.
#'
#' @return A tibble of discretized columns which need to be discretized.
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code needs to configure the Python environment to run:
#' data('ndvi')
#' robust_disc(NDVIchange ~ GDP,data = ndvi,discnum = 5)
#' robust_disc(NDVIchange ~ .,
#'             data = dplyr::select(ndvi,-c(Climatezone,Mining)),
#'             discnum = 10,cores = 6)
#' }
robust_disc = \(formula,data,discnum,minsize = 1,cores = 1) {
  formulavars = sdsfun::formula_varname(formula,data)
  response = data[, formulavars[[1]], drop = TRUE]
  explanatory = data[, formulavars[[2]]]
  y = formulavars[[1]]
  xvars = formulavars[[2]]
  if (length(minsize)==1) {minsize = rep(1,length(xvars))}
  if (length(discnum)==1) {discnum = rep(discnum,length(xvars))}
  gs = as.integer(discnum)
  minsizes = as.integer(minsize)
  cores = as.integer(cores)
  gdf = dplyr::select(data,dplyr::all_of(c(y,xvars)))

  script_path = system.file("python", "cpd_disc.py", package = "gdverse")
  cpd_disc_lib = utils_source_python(script_path)
  out_g = cpd_disc_lib$cpd_disc(gdf,y,xvars,gs,minsizes,cores) %>%
    #utils_py_to_r() %>%
    tibble::as_tibble()
  return(out_g)
}
