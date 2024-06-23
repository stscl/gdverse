#' @title locally explained heterogeneity(LESH) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for locally explained heterogeneity model.
#'
#' @references
#' Li, Y., Luo, P., Song, Y., Zhang, L., Qu, Y., & Hou, Z. (2023). A locally explained heterogeneity model for
#' examining wetland disparity. International Journal of Digital Earth, 16(2), 4533–4552.
#' https://doi.org/10.1080/17538947.2023.2271883
#'
#' @param formula  A formula of LESH model.
#' @param data A data.frame or tibble of observation data.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster object.
#' @param ... (optional) Other arguments passed to `rpart_disc()`.
#'
#' @return A list of the LESH model result.
#' @importFrom dplyr starts_with left_join
#' @export
#'
#' @examples
#' \dontrun{
#' data('ndvi')
#' g = lesh(NDVIchange ~ ., data = ndvi, cores = 6)
#' g
#' }
lesh = \(formula,data,cores = 1,...){
  spd = spd_lesh(formula,data,cores,...)
  pd = gozh(formula,data,cores,type = 'interaction',...)[[1]]
  res = pd %>%
    dplyr::left_join(dplyr::select(spd,varibale,spd1 = spd_theta),
                     by = c("variable1" = "varibale")) %>%
    dplyr::left_join(dplyr::select(spd,varibale,spd2 = spd_theta),
                     by = c("variable2" = "varibale")) %>%
    dplyr::mutate(spd = (spd1 + spd2), spd1 = spd1 / spd, spd2 = spd2 / spd,
                  `Variable1 SPD` = `Variable1 and Variable2 interact Q-statistics`*spd1,
                  `Variable2 SPD` = `Variable1 and Variable2 interact Q-statistics`*spd2) %>%
    dplyr::select(-dplyr::starts_with('spd'))
  res = list("interaction" = res)
  class(res) = "interaction_lesh"
  return(res)
}

#' @title print LESH model interaction result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for LESH model interaction result in `lesh()`.
#'
#' @param x Return by `lesh()`.
#' @param ... Other arguments.
#'
#' @return Formatted string output
#' @importFrom pander pander
#' @importFrom dplyr mutate select
#' @export
print.interaction_lesh = \(x, ...) {
  cat("\n    Spatial Interaction Association Detect    \n",
      "\n                   LESH Model                 \n")
  x = x$interaction %>%
    dplyr::mutate(`Interactive variable` = paste0(variable1,
                                                  rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20))),
                                                  variable2)) %>%
    dplyr::select(`Interactive variable`,Interaction)
  pander::pander(x)
}

#' @title plot LESH model interaction result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for LESH model interaction result in `lesh()`.
#' @details
#' When both `scatter` and `pie` are set to `TRUE` in RStudio, enlarge the drawing frame
#' for normal display.
#'
#' @param x x Return by `lesh()`.
#' @param scatter (optional) Whether to draw the interaction direction diagram. Default is `TRUE`.
#' @param pie (optional) Whether to draw the interaction contributions. Default is `TRUE`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#' @param scatter_alpha (optional) Picture transparency. Default is `1`.
#'
#' @return A ggplot2 layer.
#' @importFrom ggplot2 scale_fill_manual element_text coord_equal
#' @importFrom PieGlyph geom_pie_glyph scale_radius_continuous
#' @importFrom cowplot plot_grid
#' @export
#'
#' @examples
#' \dontrun{
#' data('ndvi')
#' g = lesh(NDVIchange ~ ., data = ndvi, cores = 6)
#' plot(g)
#' }
#'
plot.interaction_lesh = \(x,scatter = TRUE,pie = TRUE,
                          ..., scatter_alpha = 1) {
  fig_scatter = NULL
  fig_pie = NULL
  if (scatter) {
    g_scatter = list("interaction" = dplyr::select(x$interaction,1:6))
    class(g_scatter) = 'interaction_detector'
    fig_scatter = plot.interaction_detector(g_scatter,scatter_alpha,...)
  }
  if (pie) {
    g_pie = x$interaction %>%
      dplyr::select(interactv = `Variable1 and Variable2 interact Q-statistics`,
                    spd1 = `Variable1 SPD`, spd2 = `Variable2 SPD`,
                    dplyr::everything())
    gv1 = dplyr::count(g_pie,variable1)
    gv2 = dplyr::count(g_pie,variable2)
    gv = gv1 %>%
      dplyr::left_join(gv2,by = 'n') %>%
      dplyr::arrange(dplyr::desc(n))
    g_pie = g_pie %>%
      dplyr::mutate(variable1 = factor(variable1,levels = gv$variable1),
                    variable2 = factor(variable2,levels = rev(gv$variable2)))
    fig_pie = ggplot2::ggplot(data = g_pie,
                              ggplot2::aes(x = variable1, y = variable2))+
      PieGlyph::geom_pie_glyph(ggplot2::aes(radius = interactv),
                               slices = c('spd1', 'spd2'),
                               show.legend = TRUE) +
      ggplot2::scale_fill_manual(name = "", values = c('#75c7af','#fb9872'),
                                 labels = c('Variable-Xaxis','Variable-Yaxis')) +
      PieGlyph::scale_radius_continuous(name = '', range = c(0.25, 0.75)) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::coord_equal() +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(color = '#75c7af'),
                     axis.text.y = ggplot2::element_text(color = '#fb9872'),
                     ...)
  }

  if (is.null(fig_scatter) | is.null(fig_pie)){
    if (is.null(fig_pie) & is.null(fig_scatter)) {
      stop("One or more of `pie` and `scatter` must be `TRUE`!")
    } else if (is.null(fig_pie)) {
      return(fig_scatter)
    } else {
      return(fig_pie)
    }
  } else {
    fig_p = cowplot::plot_grid(fig_scatter, fig_pie, nrow = 1,
                               label_fontfamily = 'serif',
                               labels = paste0('(',letters[1:2],')'),
                               label_fontface = 'plain',label_size = 10,
                               hjust = -1.5,align = 'hv')
    return(fig_p)
  }
}
