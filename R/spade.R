#' @title spatial association detector (SPADE) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for spatial association detector (SPADE) model.
#' @note
#' The columns in the `locations` part of data are only used to construct spatial weight matrix
#' when `wt` is `NULL`, and are not considered as explanatory variables. If you need to include
#' spatial locations as explanatory variables, build a spatial weight matrix ahead of time,
#' leaving the `locations` parameter to `NULL`.The most recommended method is to explicitly
#' specify all variables in `formula` instead of using `.`!
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param formula A formula of spatial association detector (SPADE) model.
#' @param data A data.frame or tibble of observation data.
#' @param wt (optional) The spatial weight matrix.When `wt` is not provided, must provide `locations`.
#' And `gdverse` will use `locations` columns to construct spatial weight use `inverse_distance_weight()`.
#' @param locations (optional) The geospatial locations coordinate columns name which in `data`.
#' Useful and must provided when `wt` is not provided. When `wt` is provided, `locations` is not need.
#' @param discnum (optional) Number of multilevel discretization.Default will use `3:22`.
#' @param discmethod (optional) The discretization methods. Default all use `quantile`.
#' When `discmethod` is `robust` use `robust_disc()`, others use `st_unidisc()`
#' @param cores (optional) A positive integer(default is 6). If cores > 1, use parallel computation.
#' @param seed (optional) Random number seed, default is `123456789`.
#' @param permutations (optional) The number of permutations for the PSD computation. Default is `0`,
#' which means no pseudo-p values are calculated.
#' @param ... (optional) Other arguments passed to `st_unidisc()` or `robust_disc()`.
#'
#' @return A list of the SPADE model result.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' usfi = read_sf(system.file('extdata/USFI_Xian.gpkg',package = 'gdverse')) |>
#'   dplyr::select(dplyr::all_of(c("NDVI","BH","SUHI")))
#' coord = usfi |>
#'   st_centroid() |>
#'   st_coordinates()
#' wt = inverse_distance_weight(coord[,1],coord[,2])
#' usfi = usfi |>
#'   dplyr::bind_cols(coord) |>
#'   st_drop_geometry()
#' spade('SUHI~.', data = usfi,locations = c('X','Y'),cores = 6)
#' spade('SUHI~.', data = usfi, wt = wt,locations = c('X','Y'),
#'       discmethod = c('sd','equal'),cores = 6)
#' }
spade = \(formula,data,wt = NULL,locations = NULL,discnum = NULL,discmethod = NULL,
          cores = 6, seed = 123456789, permutations = 0, ...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(c(formula.vars,locations)))
  }
  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) %in% c(formula.vars[1],locations))]
  if (is.null(discmethod)) {discmethod = rep('quantile',length(xname))}
  qv = vector("list",length = length(xname))
  for (i in seq_along(xname)){
    qv[[i]] = psmd_pseudop(
      formula = paste(yname,'~',xname[i]),
      data = dplyr::select(data,
                           dplyr::all_of(c(yname,xname[i],locations))),
      wt = wt, locations = locations, discnum = discnum, discmethod = discmethod[i],
      cores = cores, seed = seed, permutations = permutations, ...)
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
  cat("\n       Spatial Association Detector       \n",
      "\n                  SPADE Model               ")
  # pander::pander(x$factor)
  print(kableExtra::kable(x$factor,format = "markdown",digits = 16,align = 'c'))
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
    fig_factor = plot.factor_detector(x,...)
  }
  return(fig_factor)
}
