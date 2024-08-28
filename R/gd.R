#' @title native geographical detector(GD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for native geographical detector model.
#' @references
#' Jin‐Feng Wang, Xin‐Hu Li, George Christakos, Yi‐Lan Liao, Tin Zhang, XueGu & Xiao‐Ying Zheng (2010)
#' Geographical Detectors‐Based Health Risk Assessment and its Application in the Neural Tube Defects Study
#' of the Heshun Region, China, International Journal of Geographical Information Science, 24:1, 107-127,
#' DOI: 10.1080/13658810802443457
#'
#' @param formula A formula of geographical detector model.
#' @param data A data.frame or tibble of observation data.
#' @param type (optional) The type of geographical detector, which must be one of `factor`(default),
#' `interaction`, `risk`, `ecological`. You can run one or more types at one time.
#' @param alpha (optional) Specifies the size of the alpha (confidence level). Default is `0.95`.
#'
#' @return A list of the GD model result.
#' \describe{
#' \item{\code{factor}}{the result of factor detector}
#' \item{\code{interaction}}{the result of interaction detector}
#' \item{\code{risk}}{the result of risk detector}
#' \item{\code{ecological}}{the result of ecological detector}
#' }
#' @export
#'
#' @examples
#' gd(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))))
#'
#' gd(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'interaction')
#'
#' gd(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'risk',alpha = 0.95)
#'
#' gd(y ~ x1 + x2,
#'    tibble::tibble(y = 1:7,
#'                   x1 = c('x',rep('y',3),rep('z',3)),
#'                   x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'    type = 'ecological',alpha = 0.95)
#'
gd = \(formula, data, type = "factor", alpha = 0.95){
  if (length(type) == 1){
    res = geodetector(formula,data = data,type = type,alpha = alpha)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = geodetector(formula,data = data,
                             type = type[i],
                             alpha = alpha)[[1]]
    }
    names(res) = type
  }
  class(res) = "gd_result"
  return(res)
}
