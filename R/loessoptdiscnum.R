#' optimal spatial data discretization for individual variables based on
#' locally estimated scatterplot smoothing (LOESS) model.
#'
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @note
#' When `increase_rate` is not satisfied by the calculation, `increase_rate*0.1` is used first.
#' At this time, if `increase_rate*0.1` is not satisfied again, the discrete number corresponding
#' to the highest Q-statistic is selected as a return.
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
#' @examples
#' library(sf)
#' usfi = read_sf(system.file('extdata/USFI_Xian.gpkg',package = 'gdverse')) |>
#'   dplyr::select(dplyr::all_of(c("NDVI","BH","SUHI")))
#' 3:10 %>%
#' purrr::map_dbl(\(.k) st_unidisc(usfi$NDVI,.k) %>%
#'                factor_detector(usfi$SUHI,.) %>%
#'                {.[[1]]}) %>%
#'  loess_optdiscnum(3:10)
#'
loess_optdiscnum = \(qvec, discnumvec,
                     increase_rate = 0.05){
  qvec = qvec[which(!is.na(qvec))] # debug : remove NA value in qvec and discnumver
  discnumvec = discnumvec[which(!is.na(qvec))]
  loessf = stats::loess(qvec ~ discnumvec)
  loessrate = (loessf$fitted - dplyr::lag(loessf$fitted)) / dplyr::lag(loessf$fitted)
  increase_rate = ifelse(max(loessrate,na.rm = TRUE) < increase_rate,
                         increase_rate*0.1, increase_rate)
  lrtbf = tibble::tibble(discnum = discnumvec,
                         qstatistic = qvec,
                         lr = loessrate,
                         lr_before = dplyr::lag(lr)) %>%
    dplyr::filter(lr <= increase_rate & lr_before > increase_rate)

  # debug: when no increase_rate is satisfied, the highest Q-statistic is selected
  if (is.na(lrtbf[1,1,drop = TRUE])){
    res = c('discnum' = discnumvec[which.max(qvec)])
  } else {
    res = c('discnum' = lrtbf[1,1,drop = TRUE])
  }
  return(res)
}
