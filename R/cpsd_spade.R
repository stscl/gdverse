#' @title measure information loss by information entropy
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for measure information loss by information entropy.
#' @details
#' The power of spatial determinant formula is
#' \eqn{F = -\sum\limits_{i=1}^N p_{(i)}\log_2 p_{(i)} - \left(-\sum\limits_{h=1}^L p_{(h)}\log_2 p_{(h)}\right)}
#'
#' @param xvar The original un-discretized vector.
#' @param xdisc The discretized vector.
#'
#' @return A value of information loss as measured by information entropy.
#' @importFrom dplyr count mutate pull
#' @importFrom tibble tibble
#' @export
F_informationloss = \(xvar,xdisc){
  gdf = tibble::tibble(xvar = xvar,
                       xdisc = xdisc)
  term1 = gdf %>%
    dplyr::count(xvar) %>%
    dplyr::mutate(n = n / sum(n)) %>%
    dplyr::pull(n)
  term2 = gdf %>%
    dplyr::count(xdisc) %>%
    dplyr::mutate(n = n / sum(n)) %>%
    dplyr::pull(n)
  information_entropy = \(p) sum(p*log2(p)) * -1
  Fiiv = information_entropy(term1) - information_entropy(term2)
  return(Fiiv)
}
