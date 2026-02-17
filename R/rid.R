#' @title robust interaction detector(RID) model
#' @references
#' Zhang, Z., Song, Y., Karunaratne, L., & Wu, P. (2024). Robust interaction detector:
#' A case of road life expectancy analysis. Spatial Statistics, 59(100814), 100814.
#' https://doi.org/10.1016/j.spasta.2024.100814
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
#' data('sim')
#' \donttest{
#' tryCatch({
#'   g = rid(y ~ .,
#'           data = dplyr::select(sim,-dplyr::any_of(c('lo','la'))),
#'           discnum = 3:6, cores = 1)
#'   g
#' }, error = \(e) message("Skipping Python-dependent example: ", e$message))
#' }
rid = \(formula, data, discvar = NULL, discnum = 3:8, minsize = 1,
        strategy = 2L, increase_rate = 0.05, cores = 1){
  yname = sdsfun::formula_varname(formula, data)[[1]]
  if (inherits(data,'sf')) {data = sf::st_drop_geometry(data)}
  rgd_res = gdverse::rgd(formula, data, discvar, discnum, minsize,
                         strategy, increase_rate, cores)
  res = gdverse::gd(formula,
           data = dplyr::bind_cols(dplyr::select(data,yname),rgd_res[[2]]),
           type = "interaction")[[1]]
  res = list("interaction" = res)
  class(res) = "rid_result"
  return(res)
}

#' @title print RID result
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
