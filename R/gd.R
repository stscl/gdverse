#' @title native geographical detector(GD) model
#' @references
#' Jin‐Feng Wang, Xin‐Hu Li, George Christakos, Yi‐Lan Liao, Tin Zhang, XueGu & Xiao‐Ying Zheng (2010)
#' Geographical Detectors‐Based Health Risk Assessment and its Application in the Neural Tube Defects Study
#' of the Heshun Region, China, International Journal of Geographical Information Science, 24:1, 107-127,
#' DOI: 10.1080/13658810802443457
#'
#' @param formula A formula of geographical detector model.
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param type (optional) The type of geographical detector, which must be one of `factor`(default),
#' `interaction`, `risk`, `ecological`. You can run one or more types at one time.
#' @param alpha (optional) Specifies the size of the alpha (confidence level). Default is `0.95`.
#'
#' @return A list.
#' \describe{
#' \item{\code{factor}}{the result of factor detector}
#' \item{\code{interaction}}{the result of interaction detector}
#' \item{\code{risk}}{the result of risk detector}
#' \item{\code{ecological}}{the result of ecological detector}
#' }
#' @export
#'
#' @examples
#' data("NTDs")
#' g = gd(incidence ~ watershed + elevation + soiltype,
#'        data = NTDs,type = c('factor','interaction'))
#' g
#'
gd = \(formula, data, type = "factor", alpha = 0.95){
  if (inherits(data,'sf')) {data = sf::st_drop_geometry(data)}
  data = tibble::as_tibble(data)
  if (length(type) == 1){
    res = gdverse::geodetector(formula,data = data,type = type,alpha = alpha)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = gdverse::geodetector(formula, data = data,
                                      type = type[i], alpha = alpha)[[1]]
    }
    names(res) = type
    class(res) = "gd_result"
  }
  return(res)
}

#' @title print GD result
#' @description
#' S3 method to format output for GD model from `gd()`.
#'
#' @param x Return by `gd()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @export
print.gd_result = \(x, ...) {
  nx = names(x)
  for (i in seq_along(x)){
    res = x[i]
    class(res) = paste0(nx[i],"_detector")
    print(res)
    cat("\n")
  }
}

#' @title plot GD result
#' @description
#' S3 method to plot output for GD model result in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param ... (optional) Other arguments.
#'
#' @return A ggplot2 layer
#' @export
#'
plot.gd_result = \(x, ...) {
  fig_p = vector("list",length(x))
  nx = names(x)
  for (i in seq_along(x)){
    res = x[i]
    class(res) = paste0(nx[i],"_detector")
    fig_p[[i]] = plot(res, ...)
  }
  fig_p = patchwork::wrap_plots(fig_p, ncol = 2)
  return(fig_p)
}
