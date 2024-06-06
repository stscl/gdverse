#' @title factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' The factor detector q-statistic measures the spatial stratified heterogeneity of a variable Y,
#' or the determinant power of a covariate X of Y.
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.
#'
#' @return A list contains the Q-statistic and the p-value.
#' @importFrom stats var pf
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by n filter ungroup mutate
#' @export
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
  lambda = (v1 - v2) / (var(y) * (N - 1) / N)
  pv = stats::pf(Fv, df1 = (L - 1), df2 = (N - L),
                 ncp = lambda, lower.tail = FALSE)
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
#' @return A list contains the Q statistic when the factors \eqn{X_1} and \eqn{X_1} act on \eqn{Y} alone
#' and the Q statistic when the two interact on \eqn{Y} together with the result type of the interaction detector.
#'
#' @export
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
#' Determine whether there is a significant difference between the attribute means of two subregions.
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param alpha (optional) Confidence level of the interval,default is `0.95`.
#'
#' @return A tibble contains different combinations of covariate \code{X} level and student t-test statistics,
#' degrees of freedom, p-values, and whether has risk (Yes or No).
#'
#' @importFrom stats t.test
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @importFrom purrr map2_dfr
#' @importFrom dplyr filter pull bind_cols
#'
#' @export
risk_detector = \(y,x,alpha = 0.95){
  x = factor(x)
  gdf = tibble::tibble(x = x, y = y)
  paradf = tidyr::crossing(zone1 = levels(x),
                           zone2 = levels(x)) %>%
    dplyr::filter(zone1 != zone2)
  x1 = paradf$zone1
  x2 = paradf$zone2

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
#' @return A list contains \code{F} statistics, p-values, and is there a significant difference between the
#' two factors \eqn{X_1} and \eqn{X_2} on the spatial distribution of the attribute \eqn{Y}
#' @importFrom stats pf
#'
#' @export
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
