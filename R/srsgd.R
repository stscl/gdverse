#' @title spatial rough set-based geographical detector model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for spatial rough set-based geographical detector model.
#' @references
#' Bai, H., Li, D., Ge, Y., Wang, J., & Cao, F. (2022). Spatial rough set-based
#' geographical detectors for nominal target variables. Information Sciences, 586, 525–539.
#' https://doi.org/10.1016/j.ins.2021.12.019
#'
#' @param formula A formula of spatial rough set-based geographical detector model.
#' @param data A data.frame, tibble or sf object of observation data.
#' @param wt Spatial adjacency matrix. If `data` is a `sf` object, the queen adjacency
#' matrix is used when no `wt` object is provided. In other cases, you must provide a
#' `wt` object.
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
      nb_queen = spdep::poly2nb(data, queen=TRUE)
      wt = spdep::nb2mat(nb_queen, style='B',
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
#' @export
#'
print.srs_factor_detector = \(x, ...) {
  cat("spatial rough set-based geographical detector \n")
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
#' @export
#'
print.srs_interaction_detector = \(x, ...) {
  cat("spatial rough set-based geographical detector \n")
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
#' @export
#'
print.srs_ecological_detector = \(x, ...) {
  cat("spatial rough set-based geographical detector \n")
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
#' @export
#'
plot.srs_factor_detector = \(x, slicenum = 2, alpha = 0.95, ...) {
  class(x) = 'factor_detector'
  fig_factor = plot(x, slicenum, alpha, ...)
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
#' @export
#'
plot.srs_interaction_detector = \(x,alpha = 1,...){
  class(x) = 'interaction_detector'
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
#' @export
#'
plot.srs_ecological_detector = \(x, ...) {
  class(x) = 'ecological_detector'
  fig_ed = plot(x)
  return(fig_ed)
}
