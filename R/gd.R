#' @title original geographical detector model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for original geographical detector model.
#' @references
#' Jin‐Feng Wang, Xin‐Hu Li, George Christakos, Yi‐Lan Liao, Tin Zhang, XueGu & Xiao‐Ying Zheng (2010)
#' Geographical Detectors‐Based Health Risk Assessment and its Application in the Neural Tube Defects Study
#' of the Heshun Region, China, International Journal of Geographical Information Science, 24:1, 107-127,
#' DOI: 10.1080/13658810802443457
#'
#' @param formula A formula of geographical detector model.
#' @param data A data.frame or tibble of observation data.
#' @param type (optional) The type of geographical detector,which must be one of `factor`(default),
#' `interaction`, `risk`, `ecological`.
#' @param ... (optional) Specifies the size of the alpha (confidence level).Default is `0.95`.
#'
#' @return A tibble of the corresponding result is stored under the corresponding detector type.
#' @export
#'
#' @examples
#' gd(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))))
#'
#' gd(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'interaction')
#'
#' gd(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'risk',alpha = 0.95)
#'
#' gd(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'ecological',alpha = 0.95)
#'
gd = \(formula,data,type = "factor",...){
  if (!(type %in% c("factor","interaction","risk", "ecological"))){
    stop("`type` must be one of `factor`,`interaction`,`risk` and `ecological`!")
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
                                 \(i) factor_detector(response,data[,i,drop = TRUE])) %>%
              dplyr::mutate(variable = names(explanatory)) %>%
              dplyr::select(variable,dplyr::everything()) %>%
              dplyr::arrange(dplyr::desc(`Q-statistic`))
            res = list("factor" = res)
            class(res) = "factor_detector"
          },
          "interaction" = {
            res = utils::combn(names(explanatory), 2, simplify = FALSE) %>%
              purrr::map_dfr(\(i) interaction_detector(response,
                                                       data[,i[1],drop = TRUE],
                                                       data[,i[2],drop = TRUE]) %>%
                               tibble::as_tibble() %>%
                               dplyr::mutate(variable1 = i[1],
                                             variable2 = i[2]) %>%
                               dplyr::select(variable1,variable2,Interaction,
                                             dplyr::everything()))
            res = list("interaction" = res)
            class(res) = "interaction_detector"
          },
          "risk" = {
            res = purrr::map_dfr(names(explanatory),
                                 \(i) risk_detector(response,
                                                    data[,i,drop = TRUE],
                                                    ...) %>%
                                   dplyr::mutate(variable = i) %>%
                                   dplyr::select(variable,zone1st,zone2nd,Risk,
                                                 dplyr::everything()))
            res = list("risk" = res)
            class(res) = "risk_detector"
          },
          "ecological" = {
            res = utils::combn(names(explanatory), 2, simplify = FALSE) %>%
              purrr::map_dfr(\(i) ecological_detector(response,
                                                      data[,i[1],drop = TRUE],
                                                      data[,i[2],drop = TRUE],
                                                      ...) %>%
                               tibble::as_tibble() %>%
                               dplyr::mutate(variable1 = i[1],
                                             variable2 = i[2]) %>%
                               dplyr::select(variable1,variable2,Ecological,
                                             dplyr::everything()))
            res = list("ecological" = res)
            class(res) = "ecological_detector"
          }
  )
  return(res)
}

#' @title print factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for factor detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param ... Other arguments.
#'
#' @return Formatted string output
#' @export
print.factor_detector = \(x, ...) {
  cat("\n Spatial Stratified Heterogeneity Test \n",
      "\n             Factor detector            ")
  # pander::pander(x$factor)
  print(kableExtra::kable(x$factor,format = "markdown",digits = 16,align = 'c'))
}

#' @title print interaction detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for interaction detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param ... Other arguments.
#'
#' @return Formatted string output
#' @export
print.interaction_detector = \(x, ...) {
  cat("\n Spatial Stratified Heterogeneity Test \n",
      "\n           Interaction detector         ")
  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  x = x$interaction %>%
    dplyr::mutate(`Interactive variable` = paste0(variable1,
                                                  IntersectionSymbol,
                                                  variable2)) %>%
    dplyr::select(`Interactive variable`,Interaction)
  # pander::pander(x)
  print(kableExtra::kable(x,format = "markdown",align = 'c'))
}

#' @title print risk detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for risk detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param ... Other arguments.
#'
#' @return Formatted string output
#' @export
print.risk_detector = \(x, ...) {
  cat("\n Spatial Stratified Heterogeneity Test \n",
      "\n               Risk detector           \n")
  x = dplyr::select(x$risk,variable,zone1st,zone2nd,Risk)
  xvar = x %>%
    dplyr::count(variable) %>%
    dplyr::pull(variable)
  rd2mat = \(x,zonevar){
    matt = x %>%
      dplyr::filter(variable == zonevar) %>%
      dplyr::select(-variable) %>%
      tidyr::pivot_wider(names_from = zone1st,
                         values_from = Risk)
    mattname = names(matt)
    mattname[1] = 'zone'
    names(matt) = mattname
    return(matt)
  }
  for (i in xvar){
    cat(sprintf("\n Variable %s:",i))
    # print(knitr::kable(rd2mat(x,i),format = "markdown"))
    print(kableExtra::kable(rd2mat(x,i),format = "markdown",align = 'c'))
  }
}

#' @title print ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for ecological detector in `gd()`.
#'
#' @param x Return by `gd()`.
#' @param ... Other arguments.
#'
#' @return Formatted string output
#' @export
print.ecological_detector = \(x, ...) {
  cat("\n Spatial Stratified Heterogeneity Test \n",
      "\n             Ecological detector         ")
  x = dplyr::select(x$ecological,
                    dplyr::all_of(c('variable1','variable2','Ecological')))
  ed2mat = \(x){
    matt = x %>%
      tidyr::pivot_wider(names_from = "variable2",
                         values_from = "Ecological")
    matname = matt$variable1
    matt = matt %>%
      dplyr::select(-variable1) %>%
      as.matrix()
    rownames(matt) = matname
    return(matt)
  }
  # print(knitr::kable(ed2mat(x),format = "markdown"))
  print(kableExtra::kable(ed2mat(x),format = "markdown",align = 'c'))
}
