#' @title spatial rough set-based factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @references
#' Bai, H., Li, D., Ge, Y., Wang, J., & Cao, F. (2022). Spatial rough set-based
#' geographical detectors for nominal target variables. Information Sciences, 586, 525–539.
#' https://doi.org/10.1016/j.ins.2021.12.019
#'
#' @param y Variable Y, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param wt Spatial adjacency matrix.
#'
#' @return A list.
#' \describe{
#' \item{\code{PD}}{the average local explanatory power}
#' \item{\code{SE_PD}}{the degree of spatial heterogeneity of the local explanatory power}
#' }
#' @export
#'
#' @examples
#' data('srs_table')
#' data('srs_wt')
#' srs_factor_detector(srs_table$d,srs_table$a1,srs_wt)
#'
srs_factor_detector = \(y,x,wt){
  y = all2int(y)
  x = all2int(x)
  obs = matrix(x, ncol = 1)
  diag(wt) = 0
  res = SRS_PD(y,obs,wt)
  return(res)
}

#' @title spatial rough set-based interaction detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @references
#' Bai, H., Li, D., Ge, Y., Wang, J., & Cao, F. (2022). Spatial rough set-based
#' geographical detectors for nominal target variables. Information Sciences, 586, 525–539.
#' https://doi.org/10.1016/j.ins.2021.12.019
#'
#' @param y Dependent variable, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x1 Covariate \eqn{X_1}, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x2 Covariate \eqn{X_2}, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param wt Spatial adjacency matrix.
#'
#' @return A list.
#' \describe{
#' \item{\code{Variable1 PD}}{the average local explanatory power for variable1}
#' \item{\code{Variable2 PD}}{the average local explanatory power for variable2}
#' \item{\code{Variable1 and Variable2 interact PD}}{the average local explanatory power for variable1 and variable2 interact}
#' \item{\code{Variable1 SE_PD}}{the degree of spatial heterogeneity of the local explanatory power for variable1}
#' \item{\code{Variable2 SE_PD}}{the degree of spatial heterogeneity of the local explanatory power for variable2}
#' \item{\code{Variable1 and Variable2 SE_PD}}{the degree of spatial heterogeneity of the local explanatory power for variable1 and variable2 interact}
#' \item{\code{Interaction}}{the interact result type}
#' }
#' @export
#'
#' @examples
#' data('srs_table')
#' data('srs_wt')
#' srs_interaction_detector(srs_table$d,srs_table$a1,srs_table$a2,srs_wt)
#'
srs_interaction_detector = \(y,x1,x2,wt){
  pd1a = srs_factor_detector(y,x1,wt)
  pd1 = pd1a[[1]]
  pd2a = srs_factor_detector(y,x2,wt)
  pd2 = pd2a[[1]]

  y = all2int(y)
  x1 = all2int(x1)
  x2 = all2int(x2)
  obs = cbind(x1,x2)
  diag(wt) = 0
  pd12a = SRS_MULTIPD(y,obs,wt)
  pd12 = pd12a[[1]]

  if (pd12 < min(pd1, pd2)) {
    interaction = c("Weaken, nonlinear")
  } else if (pd12 >= min(pd1, pd2) & pd12 <= max(pd1, pd2)) {
    interaction = c("Weaken, uni-")
  } else if (pd12 > max(pd1, pd2) & (pd12 < pd1 + pd2)) {
    interaction = c("Enhance, bi-")
  } else if (pd12 == pd1 + pd2) {
    interaction = c("Independent")
  } else {
    interaction = c("Enhance, nonlinear")
  }
  interd = list(pd1,pd2,pd12,pd1a[[2]],pd2a[[2]],pd12a[[2]],interaction)
  names(interd) = c("Variable1 PD","Variable2 PD",
                    "Variable1 and Variable2 interact PD",
                    "Variable1 SE_PD","Variable2 SE_PD",
                    "Variable1 and Variable2 interact SE_PD",
                    "Interaction")
  return(interd)
}

#' @title spatial rough set-based ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @references
#' Bai, H., Li, D., Ge, Y., Wang, J., & Cao, F. (2022). Spatial rough set-based
#' geographical detectors for nominal target variables. Information Sciences, 586, 525–539.
#' https://doi.org/10.1016/j.ins.2021.12.019
#'
#' @param y Dependent variable, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x1 Covariate \eqn{X_1}, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x2 Covariate \eqn{X_2}, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param wt Spatial adjacency matrix.
#' @param alpha (optional) Confidence level of the interval,default is `0.95`.
#'
#' @return A list.
#' \describe{
#' \item{\code{T-statistic}}{the result of T statistic for spatial rough set-based ecological detector}
#' \item{\code{P-value}}{the result of P value for spatial rough set-based ecological detector}
#' \item{\code{Ecological}}{does one spatial feature \eqn{X_1} play a more important role than \eqn{X_2}}
#' }
#' @export
#'
#' @examples
#' data('srs_table')
#' data('srs_wt')
#' srs_ecological_detector(srs_table$d,srs_table$a1,srs_table$a2,srs_wt)
#'
srs_ecological_detector = \(y,x1,x2,wt,alpha = 0.95){
  y = all2int(y)
  x1 = all2int(x1)
  x2 = all2int(x2)
  xobs1 = matrix(x1, ncol = 1)
  xobs2 = matrix(x2, ncol = 1)
  diag(wt) = 0
  pd1 = SRS_PDTEST(y,xobs1,wt)
  pd2 = SRS_PDTEST(y,xobs2,wt)
  tt = tryCatch({
    stats::t.test(pd1,pd2,conf.level = alpha)
  }, error = function(e){
    list("statistic" = 0,
         "parameter" = 0,
         "p.value" = 1)
  })
  risk = ifelse(tt$p.value < (1 - alpha), "Yes", "No")
  risk = factor(risk,levels = c("Yes", "No"), labels = c("Yes", "No"))
  ecod = list(tt$statistic,tt$p.value,risk)
  names(ecod) = c("T-statistic","P-value","Ecological")
  return(ecod)
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
