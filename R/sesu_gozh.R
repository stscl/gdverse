#' @title comparison of size effects of spatial units based on GOZH
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for comparison of size effects of spatial units in spatial heterogeneity analysis based on
#' geographically optimal zones-based heterogeneity(GOZH) model.
#' @references
#' Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based geographical detector
#' model enhances geographic characteristics of explanatory variables for spatial heterogeneity
#' analysis: Cases with different types of spatial data, GIScience & Remote Sensing, 57(5), 593-610.
#' doi: 10.1080/15481603.2020.1760434.
#'
#' Luo, P., Song, Y., Huang, X., Ma, H., Liu, J., Yao, Y., & Meng, L. (2022). Identifying determinants of
#' spatio-temporal disparities in soil moisture of the Northern Hemisphere using a geographically optimal
#' zones-based heterogeneity model. ISPRS Journal of Photogrammetry and Remote Sensing:
#' Official Publication of the International Society for Photogrammetry and Remote Sensing (ISPRS), 185, 111–128.
#' https://doi.org/10.1016/j.isprsjprs.2022.01.009
#'
#' @details
#' Firstly, the `GOZH` model is executed for each data in the datalist (all `significant`
#' Q statistic of each data are averaged to represent the spatial connection strength under
#' this spatial unit), and then the `loess_optscale` function is used to select the optimal
#' spatial analysis scale.
#'
#' @param formula A formula of comparison of size effects of spatial units.
#' @param datalist A list of \code{data.frame} or \code{tibble}.
#' @param su A vector of sizes of spatial units.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster object.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#' @param ... (optional) Other arguments passed to `gd_bestunidisc()`.
#'
#' @return A list with `sesu`(size effects of spatial units) and `optsu`(optimal spatial unit).
#' @export
#'
#' @examples
#' \dontrun{
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
#' sesu_gozh(fvc ~ .,
#'           datalist = list(fvc1000,fvc5000),
#'           su = c(1000,5000),
#'           cores = 6)
#' }
sesu_gozh = \(formula,datalist,su, cores = 1,
              increase_rate = 0.05, alpha = 0.95, ...){
  res_sesu = purrr::map2(datalist, su,
                         \(.tbf, .spsu) gozh(formula,.tbf,cores,type = "factor",
                                             alpha = alpha, ...) %>%
                           purrr::pluck('factor') %>%
                           dplyr::mutate(su = .spsu))
  sesu = tibble::tibble(spatial_units = su,
                        sesu_result = res_sesu)
  optsu = res_sesu %>%
    purrr::list_rbind() %>%
    dplyr::filter(`P-value` <= (1 - alpha)) %>%
    dplyr::group_by(su) %>%
    dplyr::summarise(qv = mean(`Q-statistic`,na.rm = T))
  optsu = loess_optscale(optsu$qv,optsu$su,increase_rate)
  res = list('sesu' = sesu,'optsu' = optsu)
  class(res) = 'sesu_gozh'
  return(res)
}
