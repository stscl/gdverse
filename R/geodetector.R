#' @title factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' The factor detector q-statistic measures the spatial stratified heterogeneity of a variable Y,
#' or the determinant power of a covariate X of Y.
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.
#'
#' @return A list.
#' \describe{
#' \item{\code{Q-statistic}}{the q statistic for factor detector}
#' \item{\code{P-value}}{the p value for factor detector}
#' }
#' @export
#'
#' @examples
#' factor_detector(y = 1:7,x = c('x',rep('y',3),rep('z',3)))
#'
factor_detector = \(y,x){
  gdf = tibble::tibble(x = x, y = y) %>%
    dplyr::group_by(x) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = factor(x))
  x = gdf$x
  y = gdf$y
  rss = \(y) (length(y) - 1) * stats::var(y)
  qv = 1 - sum(tapply(y, x, rss)) / rss(y)
  N = length(y)
  L = length(levels(x))
  Fv = ((N - L) * qv) / ((L - 1) * (1 - qv))
  hmean = tapply(y, x, mean)
  Nh = tapply(y, x, length)
  v1 = sum(hmean ^ 2)
  v2 = (sum(sqrt(Nh) * hmean)) ^ 2 / N
  lambda = (v1 - v2) / (stats::var(y) * (N - 1) / N)
  pv = suppressWarnings(stats::pf(Fv, df1 = (L - 1), df2 = (N - L),
                                  ncp = lambda, lower.tail = FALSE))
  fd = list("Q-statistic" = qv, "P-value" = pv)
  return(fd)
}

#' @title interaction detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Identify the interaction between different risk factors, that is, assess whether factors X1 and X2 together
#' increase or decrease the explanatory power of the dependent variable Y, or whether the effects of these factors
#' on Y are independent of each other.
#'
#' @param y Dependent variable, continuous numeric vector.
#' @param x1 Covariate \eqn{X_1}, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x2 Covariate \eqn{X_2}, \code{factor}, \code{character} or \code{discrete numeric}.
#'
#' @return A list.
#' \describe{
#' \item{\code{Variable1 Q-statistics}}{Q-statistics for variable1}
#' \item{\code{Variable2 Q-statistics}}{Q-statistics for variable2}
#' \item{\code{Variable1 and Variable2 interact Q-statistics}}{Q-statistics for variable1 and variable2 interact}
#' \item{\code{Interaction}}{the interact result type}
#' }
#' @export
#'
#' @examples
#' interaction_detector(y = 1:7,
#'                      x1 = c('x',rep('y',3),rep('z',3)),
#'                      x2 = c(rep('a',2),rep('b',2),rep('c',3)))
#'
interaction_detector = \(y,x1,x2){
  x12 = paste0(x1,x2,'_')
  qv1 = factor_detector(y,x1)[[1]]
  qv2 = factor_detector(y,x2)[[1]]
  qv12 = factor_detector(y,x12)[[1]]

  if (qv12 < min(qv1, qv2)) {
    interaction = c("Weaken, nonlinear")
  } else if (qv12 >= min(qv1, qv2) & qv12 <= max(qv1, qv2)) {
    interaction = c("Weaken, uni-")
  } else if (qv12 > max(qv1, qv2) & (qv12 < qv1 + qv2)) {
    interaction = c("Enhance, bi-")
  } else if (qv12 == qv1 + qv2) {
    interaction = c("Independent")
  } else {
    interaction = c("Enhance, nonlinear")
  }
  interd = list(qv1,qv2,qv12,interaction)
  names(interd) = c("Variable1 Q-statistics","Variable2 Q-statistics",
                    "Variable1 and Variable2 interact Q-statistics",
                    "Interaction")
  return(interd)
}

#' @title risk detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Determine whether there is a significant difference between the attribute means of two sub regions.
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param alpha (optional) Confidence level of the interval,default is `0.95`.
#'
#' @return A tibble. contains different combinations of covariate \code{X} level and student t-test statistics,
#' degrees of freedom, p-values, and whether has risk (Yes or No).
#' @export
#'
#' @examples
#' risk_detector(y = 1:7,
#'               x = c('x',rep('y',3),rep('z',3)))
#'
risk_detector = \(y,x,alpha = 0.95){
  x = factor(x)
  gdf = tibble::tibble(x = x, y = y)
  paradf = utils::combn(levels(x),2,simplify = FALSE)
  x1 =  purrr::map_chr(seq_along(paradf), \(i) paradf[[i]][1])
  x2 =  purrr::map_chr(seq_along(paradf), \(i) paradf[[i]][2])
  paradf = tibble::tibble(zone1st = paste0('zone',x1),
                          zone2nd = paste0('zone',x2))

  twounit_risk_detector = \(n1,n2,cutoff = 0.95){
    y1 = dplyr::filter(gdf, x == n1) %>% dplyr::pull(y)
    y2 = dplyr::filter(gdf, x == n2) %>% dplyr::pull(y)
    df0 = min(c(length(y1),length(y2))) - 1
    tt = tryCatch({
      stats::t.test(y1,y2,conf.level = cutoff)
    }, error = function(e){
      list("statistic" = 0,
           "parameter" = df0,
           "p.value" = 1)
    })

    risk = ifelse(tt$p.value < (1 - cutoff), "Yes", "No")
    risk = factor(risk,levels = c("Yes", "No"), labels = c("Yes", "No"))
    riskd = list(tt$statistic,tt$parameter,tt$p.value,risk)
    names(riskd) = c("T-statistic","Degree-freedom","P-value","Risk")
    return(riskd)
  }

  rd = purrr::map2_dfr(x1,x2,twounit_risk_detector,cutoff = alpha) %>%
    dplyr::bind_cols(paradf,.)
  return(rd)
}

#' @title ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Compare the effects of two factors \eqn{X_1} and \eqn{X_2} on the spatial distribution of the attribute \eqn{Y}.
#'
#' @param y Dependent variable, continuous numeric vector.
#' @param x1 Covariate \eqn{X_1}, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x2 Covariate \eqn{X_2}, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param alpha (optional) Confidence level of the interval,default is `0.95`.
#'
#' @return A list.
#' \describe{
#' \item{\code{F-statistic}}{the result of F statistic for ecological detector}
#' \item{\code{P-value}}{the result of P value for ecological detector}
#' \item{\code{Ecological}}{is there a significant difference between the two factors \eqn{X_1} and \eqn{X_2} on the spatial distribution of the attribute \eqn{Y}}
#' }
#' @export
#'
#' @examples
#' ecological_detector(y = 1:7,
#'                     x1 = c('x',rep('y',3),rep('z',3)),
#'                     x2 = c(rep('a',2),rep('b',2),rep('c',3)))
#'
ecological_detector = \(y,x1,x2,alpha = 0.95){
  q1 = factor_detector(y,x1)[[1]]
  q2 = factor_detector(y,x2)[[1]]
  fv = (1 - q1) / (1 - q2)
  n = length(y)
  p0 = stats::pf(fv, df1 = n - 1, df2 = n - 1, lower.tail = FALSE)
  eco = ifelse(p0 < (1 - alpha), "Yes", "No")
  eco = factor(eco,levels = c("Yes", "No"),labels = c("Yes", "No"))
  ecod = list(fv,p0,eco)
  names(ecod) = c("F-statistic","P-value","Ecological")
  return(ecod)
}

#' @title geographical detector(GD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @note
#' Note that only one type of geodetector is supported at a time in `geodetector()`.
#' @references
#' Jin‐Feng Wang, Xin‐Hu Li, George Christakos, Yi‐Lan Liao, Tin Zhang, XueGu & Xiao‐Ying Zheng (2010)
#' Geographical Detectors‐Based Health Risk Assessment and its Application in the Neural Tube Defects Study
#' of the Heshun Region, China, International Journal of Geographical Information Science, 24:1, 107-127,
#' DOI: 10.1080/13658810802443457
#'
#' @param formula A formula of geographical detector model.
#' @param data A data.frame or tibble of observation data.
#' @param type (optional) The type of geographical detector, which must be one of `factor`(default),
#' `interaction`, `risk`, `ecological`.
#' @param alpha (optional) Specifies the size of the alpha (confidence level). Default is `0.95`.
#'
#' @return A list of tibble with the corresponding result under different detector types.
#' \describe{
#' \item{\code{factor}}{the result of factor detector}
#' \item{\code{interaction}}{the result of interaction detector}
#' \item{\code{risk}}{the result of risk detector}
#' \item{\code{ecological}}{the result of ecological detector}
#' }
#' @export
#'
#' @examples
#' geodetector(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))))
#'
#' geodetector(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'interaction')
#'
#' geodetector(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'risk',alpha = 0.95)
#'
#' geodetector(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'ecological',alpha = 0.95)
#'
geodetector = \(formula,data,type = "factor",alpha = 0.95){
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
                                \(i) factor_detector(response,
                                                     data[,i,drop = TRUE])) %>%
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
                                                   alpha) %>%
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
                                                     alpha) %>%
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
#' S3 method to format output for factor detector in `geodetector()`.
#'
#' @param x Return by `geodetector()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print factor_detector
#' @export
#'
print.factor_detector = \(x, ...) {
  cat("***          Factor Detector            ")
  # pander::pander(x$factor)
  print(knitr::kable(x$factor,format = "markdown",digits = 12,align = 'c',...))
}

#' @title print interaction detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for interaction detector in `geodetector()`.
#'
#' @param x Return by `geodetector()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print interaction_detector
#' @export
#'
print.interaction_detector = \(x, ...) {
  cat("***        Interaction Detector         ")
  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  x = x$interaction %>%
    dplyr::mutate(`Interactive variable` = paste0(variable1,
                                                  IntersectionSymbol,
                                                  variable2)) %>%
    dplyr::select(`Interactive variable`,Interaction)
  # pander::pander(x)
  print(knitr::kable(x,format = "markdown",align = 'c',...))
}

#' @title print risk detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for risk detector in `geodetector()`.
#'
#' @param x Return by `geodetector()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print risk_detector
#' @export
#'
print.risk_detector = \(x, ...) {
  cat("***            Risk Detector            \n")
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
    print(knitr::kable(rd2mat(x,i),format = "markdown",align = 'c',...))
  }
}

#' @title print ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for ecological detector in `geodetector()`.
#'
#' @param x Return by `geodetector()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print ecological_detector
#' @export
#'
print.ecological_detector = \(x, ...) {
  cat("***          Ecological Detector         ")
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
  print(knitr::kable(ed2mat(x),format = "markdown",align = 'c',...))
}

#' @title plot factor detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for factor detector in `geodetector()`.
#'
#' @param x Return by `geodetector()`.
#' @param slicenum (optional) The number of labels facing inward. Default is `2`.
#' @param alpha (optional) Confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer.
#' @method plot factor_detector
#' @export
#'
plot.factor_detector = \(x, slicenum = 2, alpha = 0.95, ...) {
  g = x$factor %>%
    dplyr::select(variable, qv = `Q-statistic`,pv = `P-value`) %>%
    dplyr::filter(!is.na(qv)) %>%
    dplyr::mutate(variable = forcats::fct_reorder(variable, qv, .desc = TRUE)) %>%
    dplyr::mutate(variable_col = c("first",rep("others",times = nrow(.)-1))) %>%
    dplyr::mutate(significance = dplyr::if_else(pv <= 1-alpha,
                                                'Significant',
                                                'Not Significant',
                                                NA))
  fig_factor = ggplot2::ggplot(g,
                               ggplot2::aes(x = qv, y = variable, fill = variable_col)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(expand = ggplot2:::expansion(mult = c(0, 0.1))) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_fill_manual(breaks = c("first", "others"),
                               values = c("#DE3533","#808080")) +
    ggplot2::labs(x = "Q statistic", y = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   legend.position = "off")
  # ylimits = round(max(g$qv) + 0.05,1)
  # fig_factor = ggplot2::ggplot(g,
  #                              ggplot2::aes(x = stats::reorder(variable,qv),
  #                                           y = qv)) +
  #   ggplot2::geom_bar(stat = "identity", fill = "#bebebe",
  #                     ggplot2::aes(alpha = significance),
  #                     show.legend = FALSE) +
  #   ggplot2::geom_bar(data = dplyr::slice(g,1),
  #                     ggplot2::aes(alpha = significance),
  #                     stat = "identity",fill = "#ff0000",
  #                     show.legend = FALSE) +
  #   ggplot2::geom_text(data = dplyr::slice(g, seq(1,slicenum)),
  #                      ggplot2::aes(label = round(qv,4)),
  #                      hjust = 1.25, color = "black") +
  #   ggplot2::geom_text(data = dplyr::slice(g, -seq(1,slicenum)),
  #                      ggplot2::aes(label = round(qv,4)),
  #                      hjust = -0.1, color = "black") +
  #   ggplot2::scale_y_continuous(limits = c(0,ylimits),
  #                               breaks = seq(0,ylimits,by = 0.1),
  #                               expand = c(0,0)) +
  #   ggplot2::scale_alpha_manual(values = c("Not Significant" = 0.25,
  #                                          "Significant" = 1),
  #                               na.value = 0.85,name = '') +
  #   ggplot2::coord_flip() +
  #   ggplot2::theme_minimal() +
  #   ggplot2::labs(x = "", y = "Q statistic") +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,
  #                                                      hjust = 1,
  #                                                      color = 'black'),
  #                  axis.text.y = ggplot2::element_text(color = 'black'),
  #                  legend.position = "inside",
  #                  legend.justification.inside = c('right','bottom'),
  #                  panel.grid = ggplot2::element_blank(), ...)
  return(fig_factor)
}

#' @title plot interaction detector result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for interaction detector in `geodetector()`.
#'
#' @param x Return by `geodetector()`.
#' @param alpha (optional) Picture transparency. Default is `1`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @method plot interaction_detector
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
    ggplot2::guides(size = ggplot2::guide_legend(
      override.aes = list(shape = 21,
                          fill = "transparent",
                          color = "black"))) +
    ggplot2::scale_color_manual(values = c("Enhance, nonlinear" = "#EA4848",
                                           "Independent" = "#E08338",
                                           "Enhance, bi-" = "#F2C55E",
                                           "Weaken, uni-" = "#6EE9EF",
                                           "Weaken, nonlinear" = "#558DE8")) +
    ggplot2::labs(x = "", y = "", size = "", color = "") +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() +
    ggplot2::theme(...)
  return(fig_interaction)
}

#' @title plot risk detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for risk detector in `geodetector()`.
#'
#' @param x Return by `geodetector()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @method plot risk_detector
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
                     panel.grid = ggplot2::element_blank(), ...) +
      ggplot2::annotate("text", x = Inf, y = -Inf, label = gname,
                        vjust = -1.75, hjust = 1.25, color = "#ff0000")
    return(fig_rd)
  }
  g = dplyr::select(x$risk,variable,zone1st,zone2nd,Risk) %>%
    dplyr::mutate(risk = forcats::fct_recode(Risk,"Y" = "Yes", "N" = "No"))
  zonevars = unique(g$variable)
  fig_rds = purrr::map(zonevars,\(x) plot_rduni(dplyr::filter(g,variable == x)
                                                ,x,...))
  # fig_p = cowplot::plot_grid(plotlist = fig_rds,
  #                            ncol = ceiling(sqrt(length(zonevars))),
  #                            label_fontfamily = 'serif',
  #                            label_fontface = 'plain')
  fig_p = patchwork::wrap_plots(fig_rds,
                                ncol = ceiling(sqrt(length(zonevars))))
  return(fig_p)
}

#' @title plot ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for ecological detector in `geodetector()`.
#'
#' @param x Return by `geodetector()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @method plot ecological_detector
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
    ggplot2::geom_text(ggplot2::aes(label = eco), color = "black") +
    ggplot2::scale_fill_manual(values = c("N" = "#7fdbff", "Y" = "#ffa500")) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = 'black'),
                   legend.position = "none",
                   panel.grid = ggplot2::element_blank(), ...)
  return(fig_ed)
}
