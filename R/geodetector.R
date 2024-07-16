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
risk_detector = \(y,x,alpha = 0.95,alternative = "greater"){
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
      stats::t.test(y1,y2,conf.level = cutoff, alternative = alternative)
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
