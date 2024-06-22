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
#' data('ndvi')
#' g = gozh(NDVIchange ~ ., data = ndvi)
#' g
#' }
gozh = \(formula, data, cores = 1,
         type = 'factor',alpha = 0.95,...){
  if (length(type) == 1){
    res = gozh_detector(formula, data, cores, type, alpha, ...)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = gozh_detector(formula, data, cores, type[i], alpha, ...)}
  }
  return(res)
}


#' @title geographically optimal zones-based heterogeneity detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for geographically optimal zones-based heterogeneity detector.
#' @references
#' Luo, P., Song, Y., Huang, X., Ma, H., Liu, J., Yao, Y., & Meng, L. (2022). Identifying determinants of
#' spatio-temporal disparities in soil moisture of the Northern Hemisphere using a geographically optimal
#' zones-based heterogeneity model. ISPRS Journal of Photogrammetry and Remote Sensing: Official
#' Publication of the International Society for Photogrammetry and Remote Sensing (ISPRS), 185, 111–128.
#' https://doi.org/10.1016/j.isprsjprs.2022.01.009
#'
#' @param formula A formula of GOZH detector.
#' @param data A data.frame or tibble of observation data.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster object.
#' @param type (optional) The type of geographical detector,which must be one of `factor`(default),
#' `interaction`, `risk`, `ecological`.
#' @param alpha (optional) Confidence level of the interval,default is `0.95`.
#' @param ... (optional) Other arguments passed to `rpart_disc()`.
#'
#' @return A tibble of the corresponding result is stored under the corresponding detector type.
#' @importFrom purrr map_chr map_dfc map_dbl pmap_chr
#' @importFrom dplyr case_when
#' @export
#'
#' @examples
#' \dontrun{
#' data('ndvi')
#' g = gozh_detector(NDVIchange ~ ., data = ndvi)
#' g
#' }
gozh_detector = \(formula, data, cores = 1,
                  type = 'factor',alpha = 0.95,...){
  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add = TRUE)
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

  calcul_rpartdisc = \(xvar,...){
    rpart_disc(paste(yname,'~',xvar),data = dti,...)
  }
  calcul_pd = \(xvar,...){
    discdf = rpart_disc(paste0(yname,'~',paste(xvar,collapse = '+')),data = dti,...)
    qv = factor_detector(dti[,yname,drop = TRUE],discdf)[[1]]
    return(qv)
  }
  interact_type = \(qv1,qv2,qv12){
    if (qv12 < min(qv1, qv2)) {
      interaction = c("Weaken, nonlinear")
    } else if (qv12 >= min(qv1, qv2) & qv12 <= max(qv1, qv2)) {
      interaction = c("Weaken, uni-")
    } else if (qv12 > max(qv1, qv2) & (qv12 < qv1 + qv2)) {
      interaction = c("Enhance, bi-")
    } else if (qv12 == qv1 + qv2) {
      interaction = c("Independent")
    } else {
      interaction = c("Enhance, nonlinear")
    }
    return(interaction)
  }

  newdata = xname %>%
    purrr::map_dfc(calcul_rpartdisc)%>%
    purrr::set_names(xname) %>%
    dplyr::bind_cols(dplyr::select(dti,
                                   dplyr::all_of(yname)),
                     .)

  switch(type,
          "factor" = {
            res = gd(paste0(yname,' ~ .'),data = newdata,type = "factor")
          },
          "interaction" = {
            xinteract = utils::combn(xname,2,simplify = FALSE)
            variable1 = purrr::map_chr(seq_along(xinteract), \(x) xinteract[[x]][1])
            variable2 = purrr::map_chr(seq_along(xinteract), \(x) xinteract[[x]][2])
            if (doclust) {
              parallel::clusterExport(cores,c('rpart_disc','factor_detector'))
              qv12 = as.numeric(parallel::parLapply(cores,xinteract,calcul_pd))
            } else {
              qv12 = purrr::map_dbl(xinteract,calcul_pd)
            }
            res = gd(paste0(yname,' ~ .'),data = newdata,type = "factor")[[1]]
            qv = res[,2,drop = TRUE]
            names(qv) = res[,1,drop = TRUE]
            qv1 = qv[variable1]
            qv2 = qv[variable2]
            res = tibble::tibble(
              "Variable1 Q-statistics" = qv1,"Variable2 Q-statistics" = qv2,
              "Variable1 and Variable2 interact Q-statistics" = qv12,
              "variable1" = variable1, "variable2" = variable2,
              "Interaction" = purrr::pmap_chr(list(qv1 = qv1,qv2 = qv2,qv12 = qv12),
                                              interact_type)) %>%
                               dplyr::select(variable1,variable2,Interaction,
                                             dplyr::everything())
            res = list("interaction" = res)
            class(res) = "interaction_detector"
          },
          "risk" = {
            res = gd(paste0(yname,' ~ .'),data = newdata,type = "risk",alpha = alpha)
          },
          "ecological" = {
            res = gd(paste0(yname,' ~ .'),data = newdata,type = "ecological",alpha = alpha)
          }
  )
  return(res)
}
