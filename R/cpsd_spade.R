#' @title compensated power of spatial determinant(CPSD)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate compensated power of spatial determinant \eqn{Q_s}.
#' @details
#' The power of compensated spatial determinant formula is
#' \eqn{Q_s = \frac{q_s}{q_{s_{inforkep}}}
#' = \frac{1 - \frac{\sum_{h=1}^L N_h \Gamma_{kdep}}{N \Gamma_{totaldep}}}{1 - \frac{\sum_{h=1}^L N_h \Gamma_{hind}}{N \Gamma_{totalind}}}}
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param yobs Variable Y
#' @param xobs The original un-discretized covariable X.
#' @param xdisc The discretized covariable X.
#' @param wt The spatial weight matrix.
#'
#' @return A value of compensated power of spatial determinant \eqn{Q_s}.
#' @export
cpsd_spade = \(yobs,xobs,xdisc,wt){
  return(psd_spade(yobs,xdisc,wt) / psd_spade(xobs,xdisc,wt))
}

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
