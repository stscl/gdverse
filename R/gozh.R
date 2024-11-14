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
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#' @param type (optional) The type of geographical detector, which must be `factor`(default),
#' `interaction`, `risk`, `ecological`. You can run one or more types at one time.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#' @param ... (optional) Other arguments passed to `rpart_disc()`.
#'
#' @return A list.
#' \describe{
#' \item{\code{factor}}{the result of factor detector}
#' \item{\code{interaction}}{the result of interaction detector}
#' \item{\code{risk}}{the result of risk detector}
#' \item{\code{ecological}}{the result of ecological detector}
#' }
#' @export
#'
#' @examples
#' data('ndvi')
#' g = gozh(NDVIchange ~ ., data = ndvi)
#' g
#'
gozh = \(formula, data, cores = 1,
         type = "factor",alpha = 0.95,...){
  if (inherits(data,'sf')) {data = sf::st_drop_geometry(data)}
  data = tibble::as_tibble(data)
  if (length(type) == 1){
    res = gdverse::gozh_detector(formula, data, cores, type, alpha, ...)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = gdverse::gozh_detector(formula, data, cores, type[i], alpha, ...)[[1]]
    }
    names(res) = type
  }
  class(res) = "gozh_result"
  return(res)
}

#' @title print GOZH result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for GOZH model from `gozh()`.
#'
#' @param x Return by `gozh()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @export
#'
print.gozh_result = \(x, ...) {
  cat("***   Geographically Optimal Zones-based Heterogeneity Model       \n")
  nx = names(x)
  for (i in seq_along(x)){
    res = x[i]
    class(res) = paste0(nx[i],"_detector")
    print(res)
    cat("\n")
  }
}

#' @title plot GOZH result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for GOZH model result in `gozh()`.
#'
#' @param x Return by `gozh()`.
#' @param ... (optional) Other arguments passed to `patchwork::wrap_plots()`.
#'
#' @return A ggplot2 layer
#' @export
#'
plot.gozh_result = \(x, ...) {
  if (length(x) == 1){
    res = x[1]
    nx = names(x)
    class(res) = paste0(nx[1],"_detector")
    fig_p = plot(res)
  } else {
    fig_p = vector("list",length(x))
    nx = names(x)
    for (i in seq_along(x)){
      res = x[i]
      class(res) = paste0(nx[i],"_detector")
      fig_p[[i]] = plot(res)
    }
    fig_p = patchwork::wrap_plots(fig_p, ncol = 2, ...)
  }
  return(fig_p)
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
#' @note
#' Only one type of detector is supported in a `gozh_detector()` run at a time.
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
#' @return A list of tibble with the corresponding result under different detector types.
#' \describe{
#' \item{\code{factor}}{the result of factor detector}
#' \item{\code{interaction}}{the result of interaction detector}
#' \item{\code{risk}}{the result of risk detector}
#' \item{\code{ecological}}{the result of ecological detector}
#' }
#' @export
#'
#' @examples
#' data('ndvi')
#' g = gozh_detector(NDVIchange ~ ., data = ndvi)
#' g
#'
gozh_detector = \(formula, data, cores = 1,
                  type = "factor",alpha = 0.95,...){
  if (!(type %in% c("factor","interaction","risk", "ecological"))){
    stop("`type` must be one of `factor`,`interaction`,`risk` and `ecological`!")
  }

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
  if (formula.vars[2] != "."){
    dti = dplyr::select(data,dplyr::all_of(formula.vars))
  } else {
    dti = data
  }
  yname = formula.vars[1]
  xname = colnames(dti)[-which(colnames(dti) == yname)]

  calcul_rpartdisc = \(xvar,...){
    gdverse::rpart_disc(paste(yname,'~',xvar),data = dti,...)
  }
  calcul_pd = \(xvar,...){
    discdf = gdverse::rpart_disc(paste0(yname,'~',paste(xvar,collapse = '+')),data = dti,...)
    qv = gdverse::factor_detector(dti[,yname,drop = TRUE],discdf)[[1]]
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

  suppressMessages({newdata = xname %>%
    purrr::map_dfc(calcul_rpartdisc)%>%
    purrr::set_names(xname) %>%
    dplyr::bind_cols(dplyr::select(dti,
                                   dplyr::all_of(yname)),
                     .)})

  switch(type,
          "factor" = {
            res = gdverse::gd(paste0(yname,' ~ .'),data = newdata,type = "factor")
          },
          "interaction" = {
            xinteract = utils::combn(xname,2,simplify = FALSE)
            variable1 = purrr::map_chr(seq_along(xinteract), \(x) xinteract[[x]][1])
            variable2 = purrr::map_chr(seq_along(xinteract), \(x) xinteract[[x]][2])
            if (doclust) {
              qv12 = as.numeric(parallel::parLapply(cores,xinteract,calcul_pd))
            } else {
              qv12 = purrr::map_dbl(xinteract,calcul_pd)
            }
            res = gdverse::gd(paste0(yname,' ~ .'),data = newdata,type = "factor")[[1]]
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
            res = gdverse::gd(paste0(yname,' ~ .'),data = newdata,type = "risk",alpha = alpha)
          },
          "ecological" = {
            res = gdverse::gd(paste0(yname,' ~ .'),data = newdata,type = "ecological",alpha = alpha)
          }
  )
  return(res)
}
