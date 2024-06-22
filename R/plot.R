#' @title plot factor detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for factor detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param alpha (optional) Confidence level of the interval,default is `0.95`.
#' @param ... Other arguments passed to `ggpubr::ggdotchart()`.
#'
#' @return A ggplot2 layer using `ggpubr::ggdotchart()`
#' @importFrom ggpubr ggdotchart
#' @importFrom ggplot2 theme
#' @export
#'
gd_plot.factor_detector = \(x, alpha = 0.05, ...) {
  g = x$factor %>%
    dplyr::select(variable, qv = `Q-statistic`,pv = `P-value`)%>%
    dplyr::mutate(significance = dplyr::if_else(pv<=0.05,
                                                'Significant',
                                                'Not Significant',
                                                NA))

  if (any(is.na(g$significance))){
    fig_factor = ggpubr::ggdotchart(data = g, x = "variable", y = 'qv',
                                    xlab = '', ylab = 'Q statistic',
                                    sorting = "descending", add = "segments",
                                    rotate = TRUE, dot.size = 10, color = '#00afbb',
                                    label = round(g[,2,drop = TRUE],3),shape = 19,
                                    font.label = list(color = "white", size = 8,vjust = 0.5),
                                    ggtheme = ggpubr::theme_pubr(), ...)
  } else {
    fig_factor = ggpubr::ggdotchart(data = g, x = "variable", y = 'qv',
                                    xlab = '', ylab = 'Q statistic',
                                    sorting = "descending", add = "segments",
                                    rotate = TRUE, dot.size = 10, color = "significance",
                                    group = "significance",palette = c('#56B4E9','#999999'),
                                    label = round(g[,2,drop = TRUE],3), shape = 19,
                                    font.label = list(color = "white", size = 8,vjust = 0.5),
                                    ggtheme = ggpubr::theme_pubr(), ...) +
      ggplot2::theme(
        legend.position = 'inside',
        legend.justification.inside = c('right','bottom')
      )
  }
  return(fig_factor)
}
