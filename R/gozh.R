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
#' @param cores positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#' @param ... (optional) Other arguments passed to `rpart::rpart()`.
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
gozh = \(formula,data,cores = 1,...){
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

  calcul_gozh = \(xvar){
    gd_rpart(paste(yname,'~',xvar),data = dti)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('gd_rpart','factor_detector'))
    out_g = parallel::parLapply(cores,xname,calcul_gozh)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xname,calcul_gozh)
  }

  out_t = gd_rpart(paste(yname,'~ .'),data = dti)
  out_g = out_g %>%
    dplyr::bind_rows(out_t) %>%
    dplyr::mutate(variable = c(xname,
                               'Explanatory Variables Association')) %>%
    dplyr::select(variable,dplyr::everything()) %>%
    dplyr::arrange(dplyr::desc(`Q-statistic`))
  res = list("factor" = out_g)
  class(res) = "factor_detector"
  return(res)
}

#' @title q-statistics of geographical detector based on recursive partitioning
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#'
#' @param formula A formula.
#' @param data A data.frame or tibble of observation data.
#' @param ... (optional) Other arguments passed to `rpart::rpart()`.
#'
#' @importFrom rpart rpart
#' @importFrom tibble as_tibble_row
#' @return A tibble contains the Q-statistic and the p-value.
#' @export
gd_rpart = \(formula,data,...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (formula.vars[2] != "."){
    dti = dplyr::select(data,dplyr::all_of(formula.vars))
  } else {
    dti = data
  }
  dti_tree = rpart::rpart(formula, data = dti, ...)
  g = data[, formula.vars[1], drop = TRUE] %>%
    factor_detector(as.character(as.numeric(dti_tree$where))) %>%
    tibble::as_tibble_row()
  return(g)
}
