#' @title shap power of determinants
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate shap power of determinants \eqn{SPD}.
#' @details
#' The power of shap power of determinants formula is
#'
#' \eqn{\theta_{x_j} \left( S \right) = \sum\limits_{s \in M \setminus \{x_j\}} \frac{|S|! \left(|M| - |S| - 1\right)!}{|M|!}\left(v \left(S \cup \left\{x_j\right\} \right) - v\left(S\right)\right)}.
#'
#' shap power of determinants (SPD) is the contribution of variable \eqn{x_j} to the power of determinants.
#'
#' @note
#' The shap power of determinants (SPD) requires at least \eqn{2^n-1} calculations when has \eqn{n} explanatory variables.
#' When there are more than 10 explanatory variables, carefully consider the computational burden of this model.
#' When there are a large number of explanatory variables, the data dimensionality reduction method can be used
#' to ensure the trade-off between analysis results and calculation speed.
#'
#' @references
#' Li, Y., Luo, P., Song, Y., Zhang, L., Qu, Y., & Hou, Z. (2023). A locally explained heterogeneity model for
#' examining wetland disparity. International Journal of Digital Earth, 16(2), 4533–4552.
#' https://doi.org/10.1080/17538947.2023.2271883
#'
#' @param formula A formula of calculate shap power of determinants.
#' @param data A data.frame or tibble of observation data.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster object.
#' @param ... (optional) Other arguments passed to `rpart_disc()`.
#'
#' @return A tibble with variable and its corresponding \eqn{SPD} value.
#' @export
#'
#' @examples
#' data('ndvi')
#' g = spd_lesh(NDVIchange ~ ., data = ndvi)
#' g
#'
spd_lesh = \(formula,data,cores = 1,...){
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
  xs = sdsfun::generate_subsets(xname,empty = FALSE, self = TRUE)

  calcul_theta = \(.x,...){
    discdf = gdverse::rpart_disc(paste0(yname,'~',paste(.x,collapse = '+')),data = dti,...)
    qv = gdverse::factor_detector(dti[,yname,drop = TRUE],discdf)[[1]]
    names(qv) = 'qv_theta'
    return(qv)
  }

  if (doclust) {
    out_g = parallel::parLapply(cores,xs,calcul_theta,...)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xs,calcul_theta,...)
  }
  qv = dplyr::pull(out_g,qv_theta)
  m = length(xname)
  mf = factorial(m)

  get_value_by_varname = \(fv,namelist,valuevec){
    for (ni in seq_along(namelist)) {
      if (setequal(fv,namelist[[ni]])) {
        res = valuevec[ni]
        break
      } else {
        res = 0
      }
    }
    return(res)
  }

  calcul_shap = \(xvar){
    fullxvar = xname[-which(xname == xvar)]
    fullvar = sdsfun::generate_subsets(fullxvar,empty = FALSE,self = TRUE)

    calcul_unishap = \(xvar,fullxvar,namelist,valuevec){
      n = length(fullxvar)
      v1 = get_value_by_varname(c(xvar,fullxvar),namelist,valuevec)
      v2 = get_value_by_varname(fullxvar,namelist,valuevec)
      thetax = factorial(n) * factorial(m - n - 1) * (v1 - v2) / mf
      return(thetax)
    }

    thetaxs = purrr::map_dbl(fullvar, \(.x) calcul_unishap(xvar,.x,xs,qv))
    thetax = sum(thetaxs)
    names(thetax) = 'spd_theta'
    return(thetax)
  }

  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add = TRUE)
  }

  if (doclust) {
    out_g = parallel::parLapply(cores,xname,calcul_shap)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xname,calcul_shap)
  }

  out_g = tibble::tibble(variable = xname) %>%
    dplyr::bind_cols(out_g) %>%
    dplyr::arrange(dplyr::desc(spd_theta))
  return(out_g)
}
