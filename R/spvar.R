#' @title spatial variance
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate inverse distance weight.
#' @details
#' The spatial variance formula is
#' \eqn{\Gamma = \frac{\sum_i \sum_{j \neq i} \omega_{ij}\frac{(y_i-y_j)^2}{2}}{\sum_i \sum_{j \neq i} \omega_{ij}}}
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param yn The numerical vector of a response variable.
#' @param wtn The spatial weight matrix.
#'
#' @return A value of spatial variance.
#' @export
#'
#' @examples
#' y = c(42,56,73)
#' wt1 = inverse_distance_weight(1:length(y),1:length(y))
#' wt2 = matrix(1,ncol = length(y),nrow = length(y))
#' diag(wt2) = 0
#' spvar(y,wt1)
#' spvar(y,wt2)
#' var(y)
#'
spvar = \(yn,wtn){
  dn = data.frame(yn,yn) %>%
    stats::dist() %>%
    {.^2/4} %>%
    as.matrix()
  gammav = sum(dn * wtn) / sum(wtn)
  return(gammav)
}
