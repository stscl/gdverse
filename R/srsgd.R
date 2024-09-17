#' @title spatial rough set-based geographical detector(SRSGD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for spatial rough set-based geographical detector model.
#' @references
#' Bai, H., Li, D., Ge, Y., Wang, J., & Cao, F. (2022). Spatial rough set-based
#' geographical detectors for nominal target variables. Information Sciences, 586, 525–539.
#' https://doi.org/10.1016/j.ins.2021.12.019
#' @note
#' The Spatial Rough Set-based Geographical Detector Model (SRSGD) conducts spatial
#' hierarchical heterogeneity analysis utilizing a geographical detector for data
#' where *the dependent variable* is *discrete*. Given the complementary relationship
#' between SRSGD and the native version of geographical detector, I strive to maintain
#' consistency with `gd()` function when establishing `srsgd()` function. This implies
#' that all input variable data in srsgd must *be discretized prior to use*.
#'
#' @param formula A formula of spatial rough set-based geographical detector model.
#' @param data A data.frame, tibble or sf object of observation data.
#' @param wt Spatial adjacency matrix. If `data` is a `sf` polygon object, the queen
#' adjacency matrix is used when no `wt` object is provided. In other cases, you must
#' provide a `wt` object.
#' @param type (optional) The type of geographical detector, which must be one of
#' `factor`(default), `interaction` and `ecological`.
#' @param alpha (optional) Specifies the size of the alpha (confidence level). Default is `0.95`.
#'
#' @return A list of tibble with the corresponding result under different detector types.
#' \describe{
#' \item{\code{factor}}{the result of spatial rough set-based factor detector}
#' \item{\code{interaction}}{the result of spatial rough set-based interaction detector}
#' \item{\code{ecological}}{the result of spatial rough set-based ecological detector}
#' }
#' @export
#'
#' @examples
#' data('srs_table')
#' data('srs_wt')
#' srsgd(d ~ a1 + a2 + a3, data = srs_table, wt = srs_wt,
#'       type = c('factor','interaction','ecological'))
#'
srsgd = \(formula,data,wt = NULL,type = "factor",alpha = 0.95){
  if (length(type) == 1){
    res = srs_geodetector(formula,data,wt,type,alpha)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = srs_geodetector(formula, data, wt,
                                 type = type[i],
                                 alpha = alpha)[[1]]
    }
    names(res) = type
    class(res) = "srsgd_result"
  }
  return(res)
}

#' @title print SRSGD result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for SRSGD model from `srsgd()`.
#'
#' @param x Return by `srsgd()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print srsgd_result
#' @export
print.srsgd_result = \(x, ...) {
  nx = names(x)
  for (i in seq_along(x)){
    res = x[i]
    class(res) = paste0("srs_",nx[i],"_detector")
    print(res)
    cat("\n")
  }
}

#' @title plot SRSGD result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for SRSGD model result in `srsgd()`.
#'
#' @param x Return by `srsgd()`.
#' @param ... (optional) Other arguments passed to `patchwork::wrap_plots()`.
#'
#' @return A ggplot2 layer
#' @method plot srsgd_result
#' @export
#'
plot.srsgd_result = \(x, ...) {
  fig_p = vector("list",length(x))
  nx = names(x)
  for (i in seq_along(x)){
    res = x[i]
    class(res) = paste0("srs_",nx[i],"_detector")
    fig_p[[i]] = plot(res)
  }
  fig_p = patchwork::wrap_plots(fig_p, ncol = 2, ...)
  return(fig_p)
}
