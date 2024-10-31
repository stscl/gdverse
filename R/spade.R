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
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param wt (optional) The spatial weight matrix. When `data` is not an `sf` object, must provide `wt`.
#' @param discvar (optional) Name of continuous variable columns that need to be discretized. Noted that
#' when `formula` has `discvar`, `data` must have these columns. By default, all independent variables are
#' used as `discvar`.
#' @param discnum (optional) Number of multilevel discretization. Default will use `3:22`.
#' @param discmethod (optional) The discretization methods. Default all use `quantile`. Note
#' that when using different `discmethod` for `discvar`, please ensure that the lengths of
#' both are consistent. Noted that `robust` will use `robust_disc()`; `rpart` will use
#' `rpart_disc()`; Others use `sdsfun::discretize_vector()`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#' @param seed (optional) Random number seed, default is `123456789`.
#' @param permutations (optional) The number of permutations for the PSD computation. Default is `0`,
#' which means no pseudo-p values are calculated.
#' @param ... (optional) Other arguments passed to `sdsfun::discretize_vector()`,`robust_disc()` or
#' `rpart_disc()`.
#'
#' @return A list.
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
      stop("When `data` is `data.frame` or `tibble`, please provide `wt`!")
    } else {
      wt_spade = wt
    }
  }
  data = tibble::as_tibble(data)
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) == yname)]
  if (is.null(discvar)) {
    xdiscname = xname
    xundiscname = NULL
  } else {
    xdiscname = discvar
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
#' @param keep (optional) Whether to keep Q-value results for insignificant variables,
#' default is `TRUE`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer.
#' @export
#'
plot.spade_result = \(x, slicenum = 2, alpha = 0.95, keep = TRUE, ...) {
  if ("No Pseudo-P Value" %in% x$factor$`P-value`) {
    g = x$factor %>%
      dplyr::select(variable, qv = `Q-statistic`,pv = `P-value`) %>%
      dplyr::filter(!is.na(qv)) %>%
      dplyr::mutate(variable = forcats::fct_reorder(variable, qv, .desc = TRUE)) %>%
      dplyr::mutate(variable_col = c("first",rep("others",times = nrow(.)-1))) %>%
      dplyr::mutate(qv_text = paste0(sprintf("%4.2f", qv * 100), "%"))
    fig_factor = ggplot2::ggplot(g,
                                 ggplot2::aes(x = qv, y = variable, fill = variable_col)) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::scale_fill_manual(breaks = c("first", "others"),
                                 values = c("#DE3533","#808080")) +
      ggplot2::geom_text(data = dplyr::slice(g, seq(1,slicenum)),
                         ggplot2::aes(label = qv_text),
                         hjust = 1.25, color = "black", fontface = "bold") +
      ggplot2::geom_text(data = dplyr::slice(g, -seq(1,slicenum)),
                         ggplot2::aes(label = qv_text),
                         hjust = -0.1, color = "black", fontface = "bold") +
      ggplot2::labs(x = "Q value", y = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                     legend.position = "off", ...)
  } else {
    class(x) = "factor_detector"
    fig_factor = plot.factor_detector(x, slicenum, alpha, keep, ...)
  }
  return(fig_factor)
}
