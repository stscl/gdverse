#' @title plot factor detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for factor detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param alpha (optional) Confidence level of the interval,default is `0.95`.
#' @param ... (optional) Other arguments passed to `ggpubr::ggdotchart()`.
#'
#' @return A ggplot2 layer using `ggpubr::ggdotchart()`
#' @importFrom ggpubr ggdotchart
#' @importFrom ggplot2 theme
#' @export
#'
plot.factor_detector = \(x, alpha = 0.05, ...) {
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

#' @title plot interaction detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for interaction detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param alpha (optional) Picture transparency. Default is `1`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @importFrom ggplot2 ggplot aes geom_point scale_size scale_color_manual theme_bw theme labs
#' @export
#'
plot.interaction_detector = \(x,alpha = 1,...){
  g = x$interaction %>%
    select(interactv = `Variable1 and Variable2 interact Q-statistics`,
           dplyr::everything())
  gv1 = dplyr::count(g,variable1)
  gv2 = dplyr::count(g,variable2)
  gv = gv1 %>%
    dplyr::left_join(gv2,by = 'n') %>%
    dplyr::arrange(dplyr::desc(n))
  g = g %>%
    dplyr::mutate(variable1 = factor(variable1,levels = gv$variable1),
                  variable2 = factor(variable2,levels = rev(gv$variable2)))
  fig_interaction = ggplot2::ggplot(g, ggplot2::aes(x = variable1, y = variable2,
                             size = interactv, color = Interaction)) +
    ggplot2::geom_point(alpha = alpha) +
    ggplot2::scale_size(range = c(1,10)) +
    ggplot2::scale_color_manual(values = c("Enhance, nonlinear" = "#EA4848",
                                           "Independent" = "#E08338",
                                           "Enhance, bi-" = "#F2C55E",
                                           "Weaken, uni-" = "#6EE9EF",
                                           "Weaken, nonlinear" = "#558DE8")) +
    ggplot2::theme_bw() +
    ggplot2::theme(...) +
    ggplot2::labs(x = "", y = "", size = "", color = "")
  return(fig_interaction)
}
