#' @title robust interaction detector(RID) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for robust interaction detector(RID) model.
#' @references
#' Zhang, Z., Song, Y., Karunaratne, L., & Wu, P. (2024). Robust interaction detector:
#' A case of road life expectancy analysis. Spatial Statistics, 59(100814), 100814.
#' https://doi.org/10.1016/j.spasta.2024.100814
#' @note
#' Please set up python dependence and configure `GDVERSE_PYTHON` environment variable if you want to run `rid()`.
#' See `vignette('rgdrid',package = 'gdverse')` for more details.
#'
#' @param formula A formula of RGD model.
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param discvar Name of continuous variable columns that need to be discretized. Noted that
#' when `formula` has `discvar`, `data` must have these columns. By default, all independent
#' variables are used as `discvar`.
#' @param discnum A numeric vector of discretized classes of columns that need to be discretized.
#' Default all `discvar` use `3:8`.
#' @param minsize (optional) The min size of each discretization group. Default all use `1`.
#' @param strategy (optional) Optimal discretization strategy. When `strategy` is `1L`, choose the highest
#' q-statistics to determinate optimal spatial data discretization parameters. When `strategy` is `2L`,
#' The optimal discrete parameters of spatial data are selected by combining LOESS model.
#' @param increase_rate (optional) The critical increase rate of the number of discretization. Default is `5%`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#'
#' @return A list.
#' \describe{
#' \item{\code{interaction}}{the result of RID model}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code needs to configure the Python environment to run:
#' data('sim')
#' g = rid(y ~ .,
#'         data = dplyr::select(sim,-dplyr::any_of(c('lo','la'))),
#'         discnum = 4, cores = 1)
#' g
#' }
rid = \(formula, data, discvar = NULL, discnum = 3:8, minsize = 1,
        strategy = 2L, increase_rate = 0.05, cores = 1){
  rgd_res = gdverse::rgd(formula, data, discvar, discnum, minsize,
                         strategy, increase_rate, cores)
  res = gd(formula,data = rgd_res[[2]],type = "interaction")[[1]]
  res = list("interaction" = res)
  class(res) = "rid_result"
  return(res)
}

#' @title print RID result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for RID model from `rid()`.
#'
#' @param x Return by `rid()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @export
print.rid_result = \(x, ...) {
  cat("***       Robust Interaction Detector      ")
  print(knitr::kable(dplyr::select(x$interaction,1:3),
                     format = "markdown", digits = 12,
                     align = 'c', ...))
}

#' @title plot RID result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for RID model from `rid()`.
#'
#' @param x Return by `rid()`.
#' @param alpha (optional) Picture transparency. Default is `1`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @export
plot.rid_result = \(x, alpha = 1, ...) {
  class(x) = "interaction_detector"
  plot.interaction_detector(x,alpha,...)
}
