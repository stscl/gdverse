#' @title discretization of variables based on recursive partitioning
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @references
#' Luo, P., Song, Y., Huang, X., Ma, H., Liu, J., Yao, Y., & Meng, L. (2022). Identifying determinants of
#' spatio-temporal disparities in soil moisture of the Northern Hemisphere using a geographically optimal
#' zones-based heterogeneity model. ISPRS Journal of Photogrammetry and Remote Sensing: Official
#' Publication of the International Society for Photogrammetry and Remote Sensing (ISPRS), 185, 111–128.
#' https://doi.org/10.1016/j.isprsjprs.2022.01.009
#'
#' @param formula A formula.
#' @param data A data.frame or tibble of observation data.
#' @param ... (optional) Other arguments passed to `rpart::rpart()`.
#'
#' @return A discrete vectors after being discretized.
#' @export
#'
#' @examples
#' \dontrun{
#' data('ndvi')
#' rpart_disc(NDVIchange ~ ., data = ndvi)
#' }
rpart_disc = \(formula,data,...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (formula.vars[2] != "."){
    dti = dplyr::select(data,dplyr::all_of(formula.vars))
  } else {
    dti = data
  }
  dti_tree = rpart::rpart(formula, data = dti, ...)
  return(as.character(as.numeric(dti_tree$where)))
}
