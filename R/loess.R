#' @title determine optimal spatial data discretization for individual variables
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for determining optimal spatial data discretization for individual variables
#' based on locally estimated scatterplot smoothing (LOESS) model.
#'
#' @note
#' When `increase_rate` is not satisfied by the calculation, `increase_rate*0.1` is used first.
#' At this time, if `increase_rate*0.1` is not satisfied again, the discrete number corresponding
#' to the highest Q-statistic is selected as a return.
#' @references
#' Yongze Song & Peng Wu (2021) An interactive detector for spatial associations,
#' International Journal of Geographical Information Science, 35:8, 1676-1701,
#' DOI:10.1080/13658816.2021.1882680
#' @note
#' Note that `gdverse` sorts `discnumvec` from smallest to largest and keeps `qvec` in
#' one-to-one correspondence with `discnumvec`.
#'
#' @param qvec A numeric vector of q statistics.
#' @param discnumvec A numeric vector of break numbers corresponding to `qvec`.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#'
#' @return A optimal number of spatial data discretization.
#' @export
#'
#' @examples
#' data('sim')
#' 3:10 %>%
#'   purrr::map_dbl(\(.k) st_unidisc(sim$xa,.k) %>%
#'                factor_detector(sim$y,.) %>%
#'                {.[[1]]}) %>%
#'  loess_optdiscnum(3:10)
#'
loess_optdiscnum = \(qvec, discnumvec, increase_rate = 0.05){
  qvec = qvec[which(!is.na(qvec))] # debug: remove NA value in qvec and discnumver.
  discnumvec = discnumvec[which(!is.na(qvec))]
  discnumrank = order(discnumvec) # debug: sort discnumvec from smallest to largest
  qvec = qvec[discnumrank]
  discnumvec = discnumvec[discnumrank]
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

#' @title determine optimal spatial data analysis scale
#' @description
#' Function for determining optimal spatial data analysis scale based on locally
#' estimated scatter plot smoothing (LOESS) model.
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#'
#' @param qvec A numeric vector of q statistics.
#' @param spscalevec A numeric vector of spatial scales corresponding to `qvec`.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#'
#' @return A optimal number of spatial scale
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code takes a long time to run:
#' library(tidyverse)
#' fvcpath = "https://github.com/SpatLyu/rdevdata/raw/main/FVC.tif"
#' fvc = terra::rast(paste0("/vsicurl/",fvcpath))
#' fvc1000 = fvc %>%
#'   terra::as.data.frame(na.rm = T) %>%
#'   as_tibble()
#' fvc5000 = fvc %>%
#'   terra::aggregate(fact = 5) %>%
#'   terra::as.data.frame(na.rm = T) %>%
#'   as_tibble()
#' qv1000 = factor_detector(fvc1000$fvc,
#'                          st_unidisc(fvc1000$premax,10,'quantile'))[[1]]
#' qv5000 = factor_detector(fvc5000$fvc,
#'                          st_unidisc(fvc5000$premax,10,'quantile'))[[1]]
#' loess_optscale(c(qv1000,qv5000),c(1000,5000))
#' }
loess_optscale = \(qvec, spscalevec, increase_rate = 0.05){
  optsu = loess_optdiscnum(qvec,spscalevec,increase_rate)
  names(optsu) = 'spscale'
  return(optsu)
}
