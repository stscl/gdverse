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
#' srsgd(d ~ a1 + a2 + a3, data = srs_table, wt = srs_wt)
#' srsgd(d ~ a1 + a2 + a3, data = srs_table,
#'       wt = srs_wt, type = 'interaction')
#' srsgd(d ~ a1 + a2 + a3, data = srs_table,
#'       wt = srs_wt, type = 'ecological')
#'
srsgd = \(formula,data,wt = NULL,type = "factor",alpha = 0.95){
  if (!(type %in% c("factor","interaction","ecological"))){
    stop("`type` must be one of `factor`,`interaction` and `ecological`!")
  }

  if (inherits(data,"sf")){
    if (is.null(wt)){
      wt = sdsfun::spdep_contiguity_swm(data, style='B',
                                        zero.policy = TRUE)
    }
    data = sf::st_drop_geometry(data)
  } else {
    if (is.null(wt)){
      stop("When data is not a `sf` object, you must provide `wt`!")
    }
  }

  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  response = data[, formula.vars[1], drop = TRUE]
  if (formula.vars[2] == "."){
    explanatory = data[,-which(colnames(data) == formula.vars[1])]
  } else {
    explanatory = subset(data, TRUE, match(formula.vars[-1], colnames(data)))
  }

  switch(type,
         "factor" = {
           res = purrr::map_dfr(names(explanatory),
                                \(i) srs_factor_detector(response,
                                                         data[,i,drop = TRUE],
                                                         wt)) %>%
             dplyr::mutate(variable = names(explanatory)) %>%
             dplyr::select(variable,dplyr::everything()) %>%
             dplyr::arrange(dplyr::desc(`PD`))
           res = list("factor" = res)
           class(res) = "srs_factor_detector"
         },
         "interaction" = {
           res = utils::combn(names(explanatory), 2, simplify = FALSE) %>%
             purrr::map_dfr(\(i) srs_interaction_detector(response,
                                                          data[,i[1],drop = TRUE],
                                                          data[,i[2],drop = TRUE],
                                                          wt) %>%
                              tibble::as_tibble() %>%
                              dplyr::mutate(variable1 = i[1],
                                            variable2 = i[2]) %>%
                              dplyr::select(variable1,variable2,Interaction,
                                            dplyr::everything()))
           res = list("interaction" = res)
           class(res) = "srs_interaction_detector"
         },
         "ecological" = {
           res = utils::combn(names(explanatory), 2, simplify = FALSE) %>%
             purrr::map_dfr(\(i) srs_ecological_detector(response,
                                                         data[,i[1],drop = TRUE],
                                                         data[,i[2],drop = TRUE],
                                                         wt, alpha) %>%
                              tibble::as_tibble() %>%
                              dplyr::mutate(variable1 = i[1],
                                            variable2 = i[2]) %>%
                              dplyr::select(variable1,variable2,Ecological,
                                            dplyr::everything()))
           res = list("ecological" = res)
           class(res) = "srs_ecological_detector"
         }
  )
  return(res)
}

#' @title print spatial rough set-based factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for spatial rough set-based factor detector in `srsgd()`.
#'
#' @param x Return by `srsgd()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print srs_factor_detector
#' @export
#'
print.srs_factor_detector = \(x, ...) {
  cat("Spatial Rough Set-based Geographical Detector \n")
  class(x) = 'factor_detector'
  print(x)
}

#' @title print spatial rough set-based interaction detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for spatial rough set-based interaction detector in `srsgd()`.
#'
#' @param x Return by `srsgd()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print srs_interaction_detector
#' @export
#'
print.srs_interaction_detector = \(x, ...) {
  cat("Spatial Rough Set-based Geographical Detector \n")
  class(x) = 'interaction_detector'
  print(x)
}

#' @title print spatial rough set-based ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for spatial rough set-based ecological detector in `srsgd()`.
#'
#' @param x Return by `srsgd()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print srs_ecological_detector
#' @export
#'
print.srs_ecological_detector = \(x, ...) {
  cat("Spatial Rough Set-based Geographical Detector \n")
  class(x) = 'ecological_detector'
  print(x)
}

#' @title plot spatial rough set-based factor detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for spatial rough set-based factor detector in `srsgd()`.
#'
#' @param x Return by `srsgd()`.
#' @param slicenum (optional) The number of labels facing inward. Default is `2`.
#' @param alpha (optional) Confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer.
#' @method plot srs_factor_detector
#' @export
#'
plot.srs_factor_detector = \(x, slicenum = 2, alpha = 0.95, ...) {
  g = x$factor %>%
    dplyr::select(variable, qv = `PD`) %>%
    dplyr::filter(!is.na(qv))
  ylimits = round(max(g$qv) + 0.05,1)
  fig_factor = ggplot2::ggplot(g,
                               ggplot2::aes(x = stats::reorder(variable,qv),
                                            y = qv)) +
    ggplot2::geom_bar(stat = "identity", fill = "#bebebe",
                      show.legend = FALSE) +
    ggplot2::geom_bar(data = dplyr::slice(g,1),
                      stat = "identity",fill = "#ff0000",
                      show.legend = FALSE) +
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
    ggplot2::labs(x = "", y = "Average local explanatory power") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,
                                                       hjust = 1,
                                                       color = 'black'),
                   axis.text.y = ggplot2::element_text(color = 'black'),
                   legend.position = "inside",
                   legend.justification.inside = c('right','bottom'),
                   panel.grid = ggplot2::element_blank(), ...)
  return(fig_factor)
}

#' @title plot spatial rough set-based interaction detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for spatial rough set-based interaction detector in `srsgd()`.
#'
#' @param x Return by `srsgd()`.
#' @param alpha (optional) Picture transparency. Default is `1`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @method plot srs_interaction_detector
#' @export
#'
plot.srs_interaction_detector = \(x,alpha = 1,...){
  class(x) = 'interaction_detector'
  x$interaction = x$interaction %>%
    dplyr::rename(`Variable1 and Variable2 interact Q-statistics` = `Variable1 and Variable2 interact PD`)
  fig_interaction = plot(x, alpha, ...)
  return(fig_interaction)
}

#' @title plot spatial rough set-based ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for spatial rough set-based ecological detector in `srsgd()`.
#'
#' @param x Return by `srsgd()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @method plot srs_ecological_detector
#' @export
#'
plot.srs_ecological_detector = \(x, ...) {
  class(x) = 'ecological_detector'
  fig_ed = plot(x)
  return(fig_ed)
}
