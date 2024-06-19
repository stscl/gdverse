#' @title power of spatial determinant(PSD)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate power of spatial determinant \eqn{q_s}.
#' @details
#' The power of spatial determinant formula is
#' \eqn{q_s = 1 - \frac{\sum_{h=1}^L N_h \Gamma_h}{N \Gamma}}
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param wt The spatial weight matrix.
#'
#' @return A value of power of spatial determinant \eqn{q_s}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(tidyverse)
#' ntdspath = system.file("extdata", "NTDs.gpkg",package = 'gdverse')
#' watershed = read_sf(ntdspath,layer = 'watershed')
#' elevation = read_sf(ntdspath,layer = 'elevation')
#' soiltype = read_sf(ntdspath,layer = 'soiltype')
#' disease = read_sf(ntdspath,layer = 'disease')
#' NTDs = disease %>%
#'   st_centroid() %>%
#'   st_join(watershed[,"watershed"]) %>%
#'   st_join(elevation[,"elevation"]) %>%
#'   st_join(soiltype[,"soiltype"])%>%
#'   dplyr::filter(if_all(everything(),~!is.na(.x)))
#' NTDs = NTDs %>%
#'   st_coordinates() %>%
#'   dplyr::bind_cols(NTDs,.)
#' wt = inverse_distance_weight(NTDs$X,NTDs$Y)
#' psd_spade(NTDs$disease,NTDs$soiltype,wt)
#' }

psd_spade = \(y,x,wt){
  gdf = tibble::tibble(x = x, y = y,
                       id_sample = seq_along(y)) %>%
    dplyr::group_by(x) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = factor(x))
  x = gdf$x
  y = gdf$y
  indices = gdf$id_sample
  sprss = \(indice){
    yn = y[indice]
    wtn = wt[indice,indice]
    return(spvar(yn,wtn))
  }
  qv = 1 - sum(tapply(indices, x, sprss)) / spvar(y,wt)
  return(qv)
}
