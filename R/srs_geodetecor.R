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
  obs = cbind(x,y)
  diag(wt) = 0
  res = SRS_PD(obs,wt)
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
  obs = cbind(x1,x2,y)
  diag(wt) = 0
  pd12a = SRS_PD(obs,wt)
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
  pd1a = srs_factor_detector(y,x1,wt)
  pd1 = pd1a[[1]]
  pd2a = srs_factor_detector(y,x2,wt)
  pd2 = pd2a[[1]]
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
