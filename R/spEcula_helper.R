#' @title calculate inverse distance weight
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate inverse distance weight.
#' @details
#' The inverse distance weight formula is
#' \eqn{w_{ij} = 1 / d_{ij}^\alpha}
#'
#' @param locx The x axis location.
#' @param locy The y axis location.
#' @param power (optional) Default is 1. Set to 2 for gravity weights.
#' @param is_arc (optional) FALSE (default) or TRUE, whether to compute arc distance.
#'
#' @return A inverse distance weight matrices with class of `matrix`.
#' @importFrom stats as.dist
#' @importFrom stats dist
#' @importFrom geosphere distm
#' @export
#'
#' @examples
#' x = 1:10
#' y = 1:10
#' inverse_distance_weight(x,y)
#' inverse_distance_weight(x,y,is_arc = TRUE)
inverse_distance_weight = \(locx,locy,power = 1,is_arc = FALSE){
  if (is_arc) {
    distij = stats::as.dist(geosphere::distm(matrix(c(locx,locy),
                                                    ncol = 2)))
  } else {
    distij = stats::dist(data.frame(locx,locy))
  }
  wij = 1 / distij ^ power
  return(as.matrix(wij))
}
