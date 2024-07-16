#' @title spatial rough set-based factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#'
#' @param y Variable Y, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param wt Spatial adjacency matrix
#' @param alpha (optional) Confidence level of the interval,default is `0.95`.
#' @param size_frac (opional) The fraction to calculate p value, default is `0.05`.
#' @param seed (optional) Random seed number, default is `123456789`.
#'
#' @return A list.
#' \describe{
#' \item{\code{PD}}{the average local explanatory power}
#' \item{\code{SE_PD}}{the degree of spatial heterogeneity of the local explanatory power}
#' \item{\code{P-value}}{P value for spatial rough set-based factor detector}
#' }
#' @export
#'
#' @examples
#' data('srs_table')
#' data('srs_wt')
#' srs_factor_detector(srs_table$d,srs_table$a1,srs_wt)
#'
srs_factor_detector = \(y,x,wt,alpha = 0.95,
                        size_frac = 0.05,
                        seed = 123456789){
  y = all2int(y)
  x = all2int(x)
  obs = cbind(x,y)
  diag(wt) = 1
  res = SRS_PD(obs,wt)

  set.seed(seed)
  size = ceiling(length(y) * size_frac)
  sindice = sample(seq_along(y), size)
  obsp = obs[sindice,]
  wtp = wt[sindice,sindice]
  if (!inherits(obsp,"matrix")){
    obsp = matrix(obsp,nrow = 1, byrow = TRUE)
  }
  if (!inherits(wtp,"matrix")){
    wtp = matrix(wtp, nrow = 1, byrow = TRUE)
  }
  pdp = SRSFactor_P(obsp,wtp)
  tt = tryCatch({
    stats::t.test(pdp,0,conf.level = alpha)
  }, error = function(e){
    list("statistic" = 0,
         "parameter" = 0,
         "p.value" = 1)
  })
  pv = tt$p.value
  names(pv) = 'P-value'
  res = append(res,pv)
  return(res)
}
