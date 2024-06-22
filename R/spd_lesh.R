#' @title SHAP power of determinants (SPD)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate SHAP power of determinants \eqn{SPD}.
#' @details
#' The power of SHAP power of determinants formula is
#'
#' \eqn{\theta_{x_j} \left( S \right) = \sum\limits_{s \in M \setminus \{x_j\}} \frac{|S|! \left(|M| - |S| - 1\right)!}{|M|!}\left(v \left(S \cup \left\{x_j\right\} \right) - v\left(S\right)\right)}.
#'
#' SHAP power of determinants (SPD) is the contribution of variable \eqn{x_j} to the power of determinants.
#'
#' @references
#' Li, Y., Luo, P., Song, Y., Zhang, L., Qu, Y., & Hou, Z. (2023). A locally explained heterogeneity model for
#' examining wetland disparity. International Journal of Digital Earth, 16(2), 4533–4552.
#' https://doi.org/10.1080/17538947.2023.2271883
#'
#' @param formula A formula of calculate SHAP power of determinants \eqn{SPD}.
#' @param data A data.frame or tibble of observation data.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster object.
#' @param ... (optional) Other arguments passed to `rpart_disc()`.
#'
#' @return A tibble with variable and its corresponding \eqn{SPD} value.
#' @export
#'
#' @examples
#' \dontrun{
#' data('ndvi')
#' g = spd_lesh(NDVIchange ~ ., data = ndvi, cores = 6)
#' g
#' }
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
  m = length(xname)
  mf = factorial(m)

  calcul_theta = \(xvar,fullxvar){
    n = length(fullxvar)
    df1 = rpart_disc(paste0(yname,'~',paste(c(xvar,fullxvar),collapse = '+')),data = dti,...)
    df2 = rpart_disc(paste0(yname,'~',paste(fullxvar,collapse = '+')),data = dti,...)
    v1 = factor_detector(dti[,yname,drop = TRUE],df1)[[1]]
    v2 = factor_detector(dti[,yname,drop = TRUE],df2)[[1]]
    thetax = factorial(n) * factorial(m - n - 1) * (v1 - v2) / mf
    return(thetax)
  }
  calcul_shap = \(xvar){
    fullxvar = xname[-which(xname == xvar)]
    fullvar = generate_subsets(fullxvar,empty = FALSE,self = TRUE)
    thetaxs = purrr::map_dbl(fullvar, \(x) calcul_theta(xvar,x))
    thetax = sum(thetaxs)
    names(thetax) = 'spd_theta'
    return(thetax)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('generate_subsets','rpart_disc','factor_detector'))
    out_g = parallel::parLapply(cores,xname,calcul_shap)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xname,calcul_shap)
  }
  out_g = tibble::tibble(varibale = xname) %>%
    dplyr::bind_cols(out_g) %>%
    dplyr::arrange(dplyr::desc(spd_theta))
  return(out_g)
}
