#' @title spatial rough set-based factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#'
#' @param y Variable Y, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param x Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param wt Spatial adjacency matrix
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
