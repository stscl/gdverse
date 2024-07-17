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
#' @param data A data.frame or tibble of observation data.
#' @param wt Spatial adjacency matrix.
#' @param type
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
srsgd = \(formula,data,wt,type = "factor",alpha = 0.95){
  if (!(type %in% c("factor","interaction","ecological"))){
    stop("`type` must be one of `factor`,`interaction` and `ecological`!")
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
                                                         data[,i,drop = TRUE]),
                                                         wt) %>%
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
