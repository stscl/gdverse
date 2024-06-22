#' @title geographically optimal zones-based heterogeneity(GOZH) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for geographically optimal zones-based heterogeneity(GOZH) model
#' @references
#' Luo, P., Song, Y., Huang, X., Ma, H., Liu, J., Yao, Y., & Meng, L. (2022). Identifying determinants of
#' spatio-temporal disparities in soil moisture of the Northern Hemisphere using a geographically optimal
#' zones-based heterogeneity model. ISPRS Journal of Photogrammetry and Remote Sensing: Official
#' Publication of the International Society for Photogrammetry and Remote Sensing (ISPRS), 185, 111–128.
#' https://doi.org/10.1016/j.isprsjprs.2022.01.009
#'
#' @param formula A formula of GOZH model.
#' @param data A data.frame or tibble of observation data.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster object.
#' @param type (optional) The type of geographical detector,which must be `factor`(default),
#' `interaction`, `risk`, `ecological`.You can run one or more types at one time.
#' @param alpha (optional) Specifies the size of confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `rpart_disc()`.
#'
#' @return A list of the GOZH model result.
#' @export
#'
#' @examples
#' \dontrun{
#' ndvi = GD::ndvi_40
#' g = gozh(NDVIchange ~ ., data = ndvi)
#' g
#' }
gozh = \(formula,data,cores = 1,
         type = 'factor',alpha = 0.95,...){
  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  yname = formula.vars[1]
  if (formula.vars[2] != "."){
    dti = dplyr::select(data,dplyr::all_of(formula.vars))
  } else {
    dti = data
  }
  xname = colnames(dti)[-which(colnames(dti) == yname)]

  calcul_rpartdisc = \(xvar){
    rpart_disc(paste(yname,'~',xvar),data = dti,...)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('rpart_disc'))
    out_g = parallel::parLapply(cores,xname,calcul_rpartdisc)
    out_g = tibble::as_tibble(do.call(cbind, out_g))%>%
      purrr::set_names(xname)
  } else {
    out_g = purrr::map_dfc(xname,calcul_rpartdisc)%>%
      purrr::set_names(xname)
  }
  newdata = dti %>%
    dplyr::select(dplyr::all_of(yname)) %>%
    dplyr::bind_cols(out_g)
  if (length(type) == 1){
    res = gd(paste0(yname,' ~ .'),data = newdata,type = type,alpha = alpha)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = gd(paste0(yname,' ~ .'),data = newdata,
                    type = type[i],alpha = alpha)
    }
  }
  return(res)
}


