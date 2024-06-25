#' optimal spatial data discretization for individual variables based on
#' locally estimated scatterplot smoothing (LOESS) model.
#'
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @references
#' Yongze Song & Peng Wu (2021) An interactive detector for spatial associations,
#' International Journal of Geographical Information Science, 35:8, 1676-1701,
#' DOI:10.1080/13658816.2021.1882680
#'
#' @param qvec A numeric vector of break numbers.
#' @param discnumvec A numeric vector of q statistics.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#'
#' @return A optimal number of spatial data discretization.
#' @export
#'
loess_optdiscnum = \(qvec, discnumvec,
                     increase_rate = 0.05){
  n = length(qvec)
  loessf = stats::loess(qvec ~ discnumvec)
  loessrate = (loessf$fitted - dplyr::lag(loessf$fitted)) / dplyr::lag(loessf$fitted)
  lrtbf = tibble::tibble(discnum = discnumvec,
                         qstatistic = qvec,
                         lr = loessrate,
                         lr_before = dplyr::lag(lr)) %>%
    dplyr::filter(lr <= increase_rate & lr_before > increase_rate)
  res = c('discnum' = lrtbf[1,1])
  return(res)
}
