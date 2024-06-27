#' @title comparison of size effects of spatial units based on OPGD.
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for comparison of size effects of spatial units in spatial heterogeneity analysis based on
#' optimal parameters geographical detector(OPGD) model.
#' @references
#' Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based geographical detector
#' model enhances geographic characteristics of explanatory variables for spatial heterogeneity
#' analysis: Cases with different types of spatial data, GIScience & Remote Sensing, 57(5), 593-610.
#' doi: 10.1080/15481603.2020.1760434.
#'
#' @param formula A formula of comparison of size effects of spatial units.
#' @param datalist A list of \code{data.frame} or \code{tibble}.
#' @param su A vector of sizes of spatial units.
#' @param discvar Name of continuous variable columns that need to be discretized.Noted that
#' when `formula` has `discvar`, `data` must have these columns.
#' @param discnum (optional) A vector of number of classes for discretization. Default is `2:15`.
#' @param discmethod (optional) A vector of methods for discretization,default is used
#' `c("sd","equal","pretty","quantile","fisher","headtails","maximum","box")`in `spEcula`.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#' @param ... (optional) Other arguments passed to `gd_bestunidisc()`.
#'
#' @return A nested tibble with `spatial_units`, `sesu_result` and `data`.
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
#' sesu_opgd(fvc ~ .,
#'           datalist = list(fvc1000,fvc5000),
#'           su = c(1000,5000),
#'           discnum = 2:15,
#'           discvar = names(select(fvc5000,-c(fvc,lulc))),
#'           cores = 6)
#' }
sesu_opgd = \(formula,datalist,su,discvar,discnum = NULL,
              discmethod = NULL,cores = 1,...){
  res_sesu = purrr::map(datalist,
                        \(.tbf) opgd(formula,.tbf,discvar,
                                     discnum,discmethod,cores,...) %>%
                                purrr::pluck('factor'))
  sesu = tibble::tibble(spatial_units = su,
                        sesu_result = res_sesu,
                        data = datalist)

  return(sesu)
}
