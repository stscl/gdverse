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
#' @return A numeric vector about optimal number of spatial scale and the critical
#' increase rate of q value.
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
#'                          sdsfun::discretize_vector(fvc1000$premax,10,'quantile'))[[1]]
#' qv5000 = factor_detector(fvc5000$fvc,
#'                          sdsfun::discretize_vector(fvc5000$premax,10,'quantile'))[[1]]
#' loess_optscale(c(qv1000,qv5000),c(1000,5000))
#' }
loess_optscale = \(qvec, spscalevec, increase_rate = 0.05){
  optsu = sdsfun::loess_optnum(qvec,spscalevec,increase_rate)
  names(optsu) = c('spscale','increase_rate')
  return(optsu)
}
