#' @title robust geographical detector(RGD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for robust geographical detector(RGD) model.
#' @references
#' Zhang, Z., Song, Y.*, & Wu, P., 2022. Robust geographical detector. International Journal of Applied Earth Observation and Geoinformation. 109, 102782.
#' DOI: 10.1016/j.jag.2022.102782.
#' @note
#' Please set up python dependence and configure `GDVERSE_PYTHON` environment variable if you want to run `rgd()`.
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
#' \item{\code{factor}}{robust power of determinant}
#' \item{\code{opt_disc}}{optimal robust discrete results}
#' \item{\code{allfactor}}{factor detection results corresponding to different number of robust discreteizations}
#' \item{\code{alldisc}}{all robust discrete results}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code needs to configure the Python environment to run:
#' data('sim')
#' g = rgd(y ~ .,
#'         data = dplyr::select(sim,-dplyr::any_of(c('lo','la'))),
#'         discnum = 3:6, cores = 1)
#' g
#' }
rgd = \(formula, data, discvar = NULL, discnum = 3:8, minsize = 1,
        strategy = 2L, increase_rate = 0.05, cores = 1){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (inherits(data,'sf')) {data = sf::st_drop_geometry(data)}
  if (length(discnum) == 1) {strategy = 1L}
  data = tibble::as_tibble(data)
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
  yname = formula.vars[1]
  if (is.null(discvar)) {
    discvar = colnames(data)[-which(colnames(data) == yname)]
  }
  discdf = dplyr::select(data,dplyr::all_of(c(yname,discvar)))
  discedvar = colnames(data[,-which(colnames(data) %in% c(discvar,yname))])
  if (is.null(discedvar)){
    xvarname = discvar
  } else {
    xvarname = c(discvar,discedvar)
  }

  resqv = vector("list", length(discnum))
  resdisc = vector("list", length(discnum))
  for (i in seq_along(discnum)) {
    g = gdverse::robust_disc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                             discdf, discnum[i], minsize, cores = cores)
    if (is.null(discedvar)){
      newdata = g
    } else {
      newdata = dplyr::bind_cols(g,dplyr::select(data,dplyr::all_of(discedvar)))
    }
    resdisc[[i]] = newdata
    resqv[[i]] = gdverse::gd(paste0(yname,' ~ .'),
                    data = dplyr::bind_cols(dplyr::select(data,yname),newdata),
                    type = "factor")[[1]]
  }
  qs = purrr::map2_dfr(resqv, discnum,
                       \(.x,.n) dplyr::mutate(.x,discnum = .n))
  disc = purrr::map2_dfr(resdisc, discnum,
                         \(.x,.n) dplyr::mutate(.x,discnum = .n))
  qs$variable = factor(qs$variable,levels = xvarname)
  qs = dplyr::rename(qs,qvalue = `Q-statistic`)

  if (strategy == 1L) {
    opt_discnum = dplyr::group_split(qs,variable) |>
      purrr::map_dbl(\(.df) .df$discnum[which.max(.df$qvalue)])
  } else {
    suppressWarnings({opt_discnum = dplyr::group_split(qs,variable) |>
      purrr::map_dbl(\(.df) sdsfun::loess_optnum(.df$qvalue, .df$discnum,
                                                 increase_rate = increase_rate)[1])})
  }
  res_discdf = purrr::map_dfc(seq_along(opt_discnum),
                              \(.n) {dn = which(disc$discnum == opt_discnum[.n])
                              return(disc[dn,.n])})
  res_qv = dplyr::group_split(qs,variable) %>%
    purrr::map2_dfr(opt_discnum,
                    \(.qv,.discn) dplyr::filter(.qv,discnum == .discn)) %>%
    dplyr::select(-discnum) %>%
    dplyr::mutate(variable = as.character(variable)) %>%
    dplyr::arrange(dplyr::desc(qvalue)) %>%
    dplyr::rename(`Q-statistic` = qvalue)
  res = list("factor" = res_qv, 'opt_disc' = res_discdf,
             "allfactor" = qs, "alldisc" = disc)
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
#' @export
print.rgd_result = \(x, ...) {
  cat("***      Robust Geographical Detector    ")
  print(knitr::kable(x[[1]],format = "markdown",digits = 12,align = 'c',...))
  cat("\n")
}

#' @title plot RGD result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for RGD model result in `rgd()`.
#'
#' @param x Return by `rgd()`.
#' @param slicenum (optional) The number of labels facing inward. Default is `2`.
#' @param alpha (optional) Confidence level. Default is `0.95`.
#' @param keep (optional) Whether to keep Q-value results for insignificant variables,
#' default is `TRUE`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @export
#'
plot.rgd_result = \(x, slicenum = 2, alpha = 0.95, keep = TRUE, ...) {
  qv = x[[1]]
  res = list("factor" = qv)
  class(res) = "factor_detector"
  fig_p = plot.factor_detector(res, slicenum, alpha, keep, ...)
  return(fig_p)
}
