#' @title robust geographical detector(RGD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for robust geographical detector(RGD) model.
#' @references
#' Zhang, Z., Song, Y.*, & Wu, P., 2022. Robust geographical detector. International Journal of Applied Earth Observation and Geoinformation. 109, 102782.
#' DOI: 10.1016/j.jag.2022.102782.
#' @note
#' Please set up python dependence and configure `GDVERSE_PYTHON` environment variable if you want to run `rgd()`.
#' See `vignette('RGDRID',package = 'gdverse')` for more details.
#'
#' @param formula A formula of RGD model.
#' @param data A data.frame, tibble or sf object of observation data.
#' @param discvar Name of continuous variable columns that need to be discretized. Noted that
#' when `formula` has `discvar`, `data` must have these columns.
#' @param discnum A numeric vector of discretized classes of columns that need to be discretized.
#' Default all `discvar` use `10`.
#' @param minsize (optional) The min size of each discretization group. Default all use `1`.
#' @param cores (optional) Positive integer(default is 1). If cores > 1, use `python` `joblib` package to
#' parallel computation.
#' @param type (optional) The type of geographical detector, which must be `factor`(default),
#' `interaction`, `risk`, `ecological`.You can run one or more types at one time.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#'
#' @return A list of the RGD model result.
#' \describe{
#' \item{\code{factor}}{the result of factor detector}
#' \item{\code{interaction}}{the result of interaction detector}
#' \item{\code{risk}}{the result of risk detector}
#' \item{\code{ecological}}{the result of ecological detector}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code needs to configure the Python environment to run:
#' data('ndvi')
#' g = rgd(NDVIchange ~ ., data = ndvi, discvar = names(ndvi)[-1:-3],
#'         cores = 6, type =c('factor','interaction'))
#' }
rgd = \(formula, data, discvar = NULL,
        discnum = NULL, minsize = NULL,
        cores = 1, type = "factor", alpha = 0.95){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (inherits(data,'sf')) {data = sf::st_drop_geometry(data)}
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
  yname = formula.vars[1]
  if (is.null(discvar)) {
    discvar = colnames(data)[-which(colnames(data) == yname)]
  }
  discdf =  dplyr::select(data,dplyr::all_of(c(yname,discvar)))
  if (is.null(discnum)) {discnum = rep(10,length(discvar))}
  g = robust_disc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                  discdf, discnum, minsize, cores = cores)
  discedvar = colnames(data[,-which(colnames(data) %in% discvar)])
  newdata = data %>%
    dplyr::select(dplyr::all_of(discedvar)) %>%
    dplyr::bind_cols(g)
  if (length(type) == 1){
    res = gd(paste0(yname,' ~ .'),data = newdata,
             type = type,alpha = alpha)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = gd(paste0(yname,' ~ .'),data = newdata,
                    type = type[i],alpha = alpha)[[1]]
    }
    names(res) = type
  }
  class(res) = "rgd_result"
  return(res)
}

#' @title print RGD result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for RGD model from `rgd()`.
#'
#' @param x Return by `rgd()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print rgd_result
#' @export
print.rgd_result = \(x, ...) {
  cat("                 RGD Model                  \n")
  nx = names(x)
  for (i in seq_along(x)){
    res = x[i]
    class(res) = paste0(nx[i],"_detector")
    print(res)
    cat("\n")
  }
}

#' @title plot RGD result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for RGD model result in `rgd()`.
#'
#' @param x Return by `rgd()`.
#' @param ... (optional) Other arguments passed to `patchwork::wrap_plots()`.
#'
#' @return A ggplot2 layer
#' @method plot rgd_result
#' @export
#'
plot.rgd_result = \(x, ...) {
  if (length(x) == 1){
    res = x[1]
    nx = names(x)
    class(res) = paste0(nx[1],"_detector")
    fig_p = plot(res)
  } else {
    fig_p = vector("list",length(x))
    nx = names(x)
    for (i in seq_along(x)){
      res = x[i]
      class(res) = paste0(nx[i],"_detector")
      fig_p[[i]] = plot(res)
    }
    fig_p = patchwork::wrap_plots(fig_p, ncol = 2, ...)
  }
  return(fig_p)
}
