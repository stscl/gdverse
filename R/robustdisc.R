#' @title univariate discretization based on offline change point detection.
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Determines discretization interval breaks using an optimization algorithm for variance-based
#' change point detection.
#'
#' @param formula A formula of spatial stratified heterogeneity test.
#' @param data A data.frame or tibble of observation data.
#' @param discnum A numeric vector of discretized classes of columns that need to be discretized.
#' @param minsize (optional) The min size of each discretization group.Default all use `1`.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, use `python`'s `joblib` package to
#' parallel computation.
#'
#' @return A tibble of discretized classes of columns that need to be discretized.
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(tidyverse)
#' fvcpath = "https://github.com/SpatLyu/rdevdata/raw/main/FVC.tif"
#' fvc = terra::rast(paste0("/vsicurl/",fvcpath))
#' fvc = terra::aggregate(fvc,fact = 5)
#' fvc = as_tibble(terra::as.data.frame(fvc,na.rm = T))
#' new.fvc = robust_disc(fvc ~ .,data = select(fvc,-lulc),discnum = 10,cores = 6)
#' new.fvc
#' }
robust_disc = \(formula,data,discnum,minsize = NULL,cores = 1) {
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  response = data[, formula.vars[1], drop = TRUE]
  if (formula.vars[2] == "."){
    explanatory = data[,-which(colnames(data) == formula.vars[1])]
  } else {
    explanatory = subset(data, TRUE, match(formula.vars[-1], colnames(data)))
  }
  y = formula.vars[1]
  xvars = names(explanatory)
  if (is.null(minsize)) {minsize = rep(1,length(xvars))}
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
