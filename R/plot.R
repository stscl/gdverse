#' @title plot factor detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for factor detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param alpha (optional) Confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer using `ggpubr::ggdotchart()`
#' @importFrom ggplot2 ggplot geom_bar geom_text scale_y_continuous coord_flip theme_minimal theme labs
#' @importFrom stats quantile
#' @export
#'
plot.factor_detector = \(x, alpha = 0.05, ...) {
  g = x$factor %>%
    dplyr::select(variable, qv = `Q-statistic`,pv = `P-value`)%>%
    dplyr::mutate(significance = dplyr::if_else(pv<=0.05,
                                                'Significant',
                                                'Not Significant',
                                                NA))
  qvl = stats::quantile(g$qv,0.75)
  fig_factor = ggplot2::ggplot(g, aes(x = stats::reorder(variable,qv), y = qv)) +
    ggplot2::geom_bar(stat = "identity",fill = "#bebebe") +
    ggplot2::geom_bar(data = dplyr::slice(g,1),stat = "identity",fill = "#ff0000") +
    ggplot2::geom_text(data = dplyr::filter(g,qv>=qvl),
                       aes(label = round(qv,4)), hjust = 1.25, color = "black") +
    ggplot2::geom_text(data = dplyr::filter(g,qv<qvl),
                       aes(label = round(qv,4)), hjust = -0.1, color = "black") +
    ggplot2::scale_y_continuous(limits = c(0,round(max(g$qv) + 0.04,1)),
                                breaks = seq(0,round(max(g$qv) + 0.04,1),by = 0.1),
                                expand = c(0,0)) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "Q statistic") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 1,color = 'black'),
                   axis.text.y = ggplot2::element_text(color = 'black'),...)
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
