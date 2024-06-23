#' @title plot factor detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for factor detector in `gd()`.
#'
#' @param x Return by `gd()`.The number of labels facing inward.
#' @param slicenum (optional) The number of labels facing inward. Default is `2`.
#' @param alpha (optional) Confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer.
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_y_continuous coord_flip theme_minimal scale_alpha_manual element_text element_blank theme labs
#' @importFrom dplyr if_else slice filter
#' @importFrom stats reorder
#' @export
#'
plot.factor_detector = \(x, slicenum = 2, alpha = 0.05, ...) {
  g = x$factor %>%
    dplyr::select(variable, qv = `Q-statistic`,pv = `P-value`) %>%
    dplyr::filter(!is.na(qv)) %>%
    dplyr::mutate(significance = dplyr::if_else(pv <= 0.05,
                                                'Significant',
                                                'Not Significant',
                                                NA))
  ylimits = round(max(g$qv) + 0.05,1)
  if ("No Pseudo-P Value" %in% g$pv) {
    fig_factor = ggplot2::ggplot(g,
                                 ggplot2::aes(x = stats::reorder(variable,qv), y = qv)) +
      ggplot2::geom_bar(stat = "identity", fill = "#bebebe") +
      ggplot2::geom_bar(data = dplyr::slice(g,1),
                        stat = "identity", fill = "#ff0000") +
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
                     panel.grid = ggplot2::element_blank(),
                     ...)
  } else {
    fig_factor = ggplot2::ggplot(g,
                                 ggplot2::aes(x = stats::reorder(variable,qv), y = qv)) +
      ggplot2::geom_bar(stat = "identity", fill = "#bebebe",
                        ggplot2::aes(alpha = significance)) +
      ggplot2::geom_bar(data = dplyr::slice(g,1),
                        ggplot2::aes(alpha = significance),
                        stat = "identity",fill = "#ff0000") +
      ggplot2::geom_text(data = dplyr::slice(g, seq(1,slicenum)),
                         ggplot2::aes(label = round(qv,4)),
                         hjust = 1.25, color = "black") +
      ggplot2::geom_text(data = dplyr::slice(g, -seq(1,slicenum)),
                         ggplot2::aes(label = round(qv,4)),
                         hjust = -0.1, color = "black") +
      ggplot2::scale_y_continuous(limits = c(0,ylimits),
                                  breaks = seq(0,ylimits,by = 0.1),
                                  expand = c(0,0)) +
      ggplot2::scale_alpha_manual(values = c("Not Significant" = 0.25),
                                  na.value = 0.85,name = '') +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "Q statistic") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,hjust = 1,color = 'black'),
                     axis.text.y = ggplot2::element_text(color = 'black'),
                     legend.position = "inside",
                     legend.justification.inside = c('right','bottom'),
                     panel.grid = ggplot2::element_blank(),
                     ...)
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
#' @importFrom ggplot2 ggplot aes geom_point scale_size scale_color_manual coord_equal theme_bw theme labs
#' @export
#'
plot.interaction_detector = \(x,alpha = 1,...){
  g = x$interaction %>%
    dplyr::select(interactv = `Variable1 and Variable2 interact Q-statistics`,
                  dplyr::everything())
  gv1 = dplyr::count(g,variable1)
  gv2 = dplyr::count(g,variable2)
  gv = gv1 %>%
    dplyr::left_join(gv2,by = 'n') %>%
    dplyr::arrange(dplyr::desc(n))
  g = g %>%
    dplyr::mutate(variable1 = factor(variable1,levels = gv$variable1),
                  variable2 = factor(variable2,levels = rev(gv$variable2)))
  fig_interaction = ggplot2::ggplot(g,
                                    ggplot2::aes(x = variable1, y = variable2,
                                    size = interactv, color = Interaction)) +
    ggplot2::geom_point(alpha = alpha) +
    ggplot2::scale_size(range = c(1,10)) +
    ggplot2::scale_color_manual(values = c("Enhance, nonlinear" = "#EA4848",
                                           "Independent" = "#E08338",
                                           "Enhance, bi-" = "#F2C55E",
                                           "Weaken, uni-" = "#6EE9EF",
                                           "Weaken, nonlinear" = "#558DE8")) +
    ggplot2::labs(x = "", y = "", size = "", color = "") +
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    ggplot2::theme(...)
  return(fig_interaction)
}

#' @title plot risk detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for risk detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 annotate element_blank element_text theme theme_minimal coord_fixed geom_tile
#' @importFrom forcats fct_recode
#' @importFrom purrr map
#' @export
#'
plot.risk_detector = \(x, ...) {
  plot_rduni = \(g,gname,...){
    fig_rd = ggplot2::ggplot(data = g,
                             ggplot2::aes(x = zone1st, y = zone2nd, fill = risk)) +
      ggplot2::geom_tile(color = "white", size = 0.75) +
      ggplot2::geom_text(ggplot2::aes(label = risk), color = "black") +
      ggplot2::scale_fill_manual(values = c("N" = "#7fdbff", "Y" = "#ffa500")) +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 60,hjust = 1,color = 'black'),
                     axis.text.y = ggplot2::element_text(color = 'black'),
                     legend.position = "none",
                     panel.grid = ggplot2::element_blank(),
                     ...) +
      ggplot2::annotate("text", x = Inf, y = -Inf, label = gname,
                        vjust = -1.75, hjust = 1.25, color = "#ff0000")
    return(fig_rd)
  }
  g = dplyr::select(x$risk,variable,zone1st,zone2nd,Risk) %>%
    dplyr::mutate(risk = forcats::fct_recode(Risk,"Y" = "Yes", "N" = "No"))
  zonevars = unique(g$variable)
  fig_rds = purrr::map(zonevars,\(x) plot_rduni(dplyr::filter(g,variable == x)
                                                ,x,...))
  fig_p = cowplot::plot_grid(plotlist = fig_rds,
                             ncol = ceiling(sqrt(length(zonevars))),
                             label_fontfamily = 'serif',
                             label_fontface = 'plain')
  return(fig_p)
}


#' @title plot ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for ecological detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @export
#'
plot.ecological_detector = \(x, ...) {
  g = dplyr::select(x$ecological,
                    dplyr::all_of(c('variable1','variable2','Ecological')))
  gv1 = dplyr::count(g,variable1)
  gv2 = dplyr::count(g,variable2)
  gv = gv1 %>%
    dplyr::left_join(gv2,by = 'n') %>%
    dplyr::arrange(dplyr::desc(n))
  g = g %>%
    dplyr::mutate(variable1 = factor(variable1,levels = gv$variable1),
                  variable2 = factor(variable2,levels = rev(gv$variable2)),
                  eco = forcats::fct_recode(Ecological,"Y" = "Yes", "N" = "No"))
  fig_ed = ggplot2::ggplot(data = g,
                           ggplot2::aes(x = variable1, y = variable2, fill = eco)) +
    ggplot2::geom_tile(color = "white", size = 0.75) +
    ggplot2::geom_text(aes(label = eco), color = "black") +
    ggplot2::scale_fill_manual(values = c("N" = "#7fdbff", "Y" = "#ffa500")) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = 'black'),
                   legend.position = "none",
                   panel.grid = ggplot2::element_blank(),
                   ...)
  return(fig_ed)
}
