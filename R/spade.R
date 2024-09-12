#' @title spatial association detector (SPADE) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for spatial association detector (SPADE) model.
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param formula A formula of spatial association detector (SPADE) model.
#' @param data A data.frame, tibble or sf of observation data.
#' @param wt (optional) The spatial weight matrix. When `data` is not an `sf` object, must provide `wt`.
#' @param discvar (optional) Name of continuous variable columns that need to be discretized. Noted that
#' when `formula` has `discvar`, `data` must have these columns. By default, all independent variables are
#' used as `discvar`.
#' @param discnum (optional) Number of multilevel discretization. Default will use `3:22`.
#' @param discmethod (optional) The discretization methods. Default all use `quantile`. Note
#' that when using different `discmethod` for `discvar`, please ensure that the lengths of
#' both are consistent. Noted that `robust` will use `robust_disc()`; `rpart` will use
#' `rpart_disc()`; Others use `st_unidisc()`.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, use parallel computation.
#' @param seed (optional) Random number seed, default is `123456789`.
#' @param permutations (optional) The number of permutations for the PSD computation. Default is `0`,
#' which means no pseudo-p values are calculated.
#' @param ... (optional) Other arguments passed to `st_unidisc()`,`robust_disc()` or `rpart_disc()`.
#'
#' @return A list of the SPADE model result.
#' \describe{
#' \item{\code{factor}}{the result of SPADE model}
#' }
#' @export
#'
#' @examples
#' data('sim')
#' sim1 = sf::st_as_sf(sim,coords = c('lo','la'))
#' g = spade(y ~ ., data = sim1)
#' g
#'
spade = \(formula, data, wt = NULL, discvar = NULL, discnum = 3:22,
          discmethod = 'quantile', cores = 1, seed = 123456789, permutations = 0, ...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (inherits(data,'sf')) {
    if (is.null(wt)){
      wt_spade = sdsfun::inverse_distance_swm(data)
    } else {
      wt_spade = wt
    }
    data = sf::st_drop_geometry(data)
  } else if (inherits(data,'data.frame')) {
    if (is.null(wt)){
      stop("When `data` is `data.frame` or `tibble`, please provide `wt` in spade input!")
    } else {
      wt_spade = wt
    }
  }
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) == yname)]
  if (is.null(discvar)) {
    xdiscname = xname
    xundiscname = NULL
  } else {
    xundiscname = xname[-which(xname %in% discvar)]
  }
  if (length(discmethod) == 1) {discmethod = rep('quantile',length(xdiscname))}
  qv_disc = vector("list",length = length(xdiscname))
  for (i in seq_along(xdiscname)){
    qv_disc[[i]] = psmd_pseudop(data[,yname,drop=TRUE],
                                data[,xdiscname[i],drop=TRUE],
                                wt_spade, discnum, discmethod[i],
                                cores, seed, permutations, ...)
  }
  if (!is.null(xundiscname)) {
    qv_undisc = vector("list",length = length(xundiscname))
    for (i in seq_along(xundiscname)){
      qv_undisc[[i]] = psd_pseudop(data[,yname,drop=TRUE],
                                   data[,xundiscname[i],drop=TRUE],
                                   wt_spade, cores, seed, permutations)
    }
    qv = purrr::list_cbind(c(qv_disc,qv_undisc))
    xname = c(xdiscname,xundiscname)
  } else {
    qv = qv_disc
  }
  res = purrr::list_rbind(qv) %>%
    dplyr::mutate(variable = xname) %>%
    dplyr::select(variable,dplyr::everything()) %>%
    dplyr::arrange(dplyr::desc(`Q-statistic`))
  res = list("factor" = res)
  class(res) = "spade_result"
  return(res)
}

#' @title print SPADE power of spatial and multilevel discretization determinant
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for SPADE power of spatial and multilevel discretization
#' determinant from `spade()`.
#'
#' @param x Return by `spade()`.
#' @param ... Other arguments.
#'
#' @return Formatted string output
#' @method print spade_result
#' @export
print.spade_result = \(x, ...) {
  cat("***         Spatial Association Detector         ")
  # pander::pander(x$factor)
  print(knitr::kable(x$factor,format = "markdown",digits = 12,align = 'c'))
}

#' @title plot SPADE power of spatial and multilevel discretization determinant
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for SPADE power of spatial and multilevel discretization
#' determinant from `spade()`.
#'
#' @param x Return by `spade()`.The number of labels facing inward.
#' @param slicenum (optional) The number of labels facing inward. Default is `2`.
#' @param alpha (optional) Confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer.
#' @method plot spade_result
#' @export
#'
plot.spade_result = \(x, slicenum = 2, alpha = 0.95, ...) {
  if ("No Pseudo-P Value" %in% x$factor$`P-value`) {
    g = x$factor %>%
      dplyr::select(variable, qv = `Q-statistic`,pv = `P-value`) %>%
      dplyr::filter(!is.na(qv)) %>%
      dplyr::mutate(significance = dplyr::if_else(pv <= 1-alpha,
                                                  'Significant',
                                                  'Not Significant',
                                                  NA))
    ylimits = round(max(g$qv) + 0.05,1)
    fig_factor = ggplot2::ggplot(g,
                                 ggplot2::aes(x = stats::reorder(variable,qv),
                                              y = qv)) +
      ggplot2::geom_bar(stat = "identity", fill = "#bebebe", alpha = 1) +
      ggplot2::geom_bar(data = dplyr::slice(g,1),
                        stat = "identity", fill = "#ff0000", alpha = 1) +
      ggplot2::geom_text(data = dplyr::slice(g, seq(1,slicenum)),
                         ggplot2::aes(label = round(qv,4)),
                         hjust = 1.25, color = "black") +
      ggplot2::geom_text(data = dplyr::slice(g, -seq(1,slicenum)),
                         ggplot2::aes(label = round(qv,4)),
                         hjust = -0.1, color = "black") +
      ggplot2::scale_y_continuous(limits = c(0,ylimits),
                                  breaks = seq(0,ylimits,by = 0.1),
                                  expand = c(0,0)) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "Q statistic") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 1,color = 'black'),
                     axis.text.y = ggplot2::element_text(color = 'black'),
                     legend.position = "inside",
                     legend.justification.inside = c('right','bottom'),
                     panel.grid = ggplot2::element_blank(), ...)
  } else {
    class(x) = "factor_detector"
    fig_factor = plot.factor_detector(x, slicenum, alpha, ...)
  }
  return(fig_factor)
}
