#' @title best univariate discretization based on geodetector q-statistic
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for determining the best univariate discretization based on geodetector q-statistic.
#'
#' @param formula A formula of best univariate discretization.
#' @param data A data.frame or tibble of observation data.
#' @param discnum (optional) A vector of number of classes for discretization. Default is `3:22`.
#' @param discmethod (optional) A vector of methods for discretization, default is using
#' `c("sd","equal","geometric","quantile","natural")` by invoking `sdsfun`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#' @param return_disc (optional) Whether or not return discretized result used the optimal parameter.
#' Default is `TRUE`.
#' @param seed (optional) Random seed number, default is `123456789`.
#' @param ... (optional) Other arguments passed to `sdsfun::discretize_vector()`.
#'
#' @return A list with the optimal parameter in the provided parameter combination with `k`,
#' `method` and `disc`(when `return_disc` is `TRUE`).
#' \describe{
#' \item{\code{x}}{the name of the variable that needs to be discretized}
#' \item{\code{k}}{optimal discretization number}
#' \item{\code{method}}{optimal discretization method}
#' \item{\code{disc}}{optimal discretization results}
#' }
#' @export
#'
#' @examples
#' data('sim')
#' gd_bestunidisc(y ~ xa + xb + xc,
#'                data = sim,
#'                discnum = 3:6)
#'
gd_bestunidisc = \(formula, data, discnum = 3:22,
                   discmethod = c("sd","equal","geometric","quantile","natural"),
                   cores = 1, return_disc = TRUE, seed = 123456789, ...){
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
  response = data[, formula.vars[1], drop = TRUE]
  if (formula.vars[2] == "."){
    explanatory = data[,-which(colnames(data) == formula.vars[1])]
  } else {
    explanatory = subset(data, TRUE, match(formula.vars[-1], colnames(data)))
  }

  discname = names(explanatory)
  paradf = tidyr::crossing("x" = discname,
                           "k" = discnum,
                           "method" = discmethod)
  parak = split(paradf, seq_len(nrow(paradf)))

  calcul_disc = \(paramgd){
    xdisc = sdsfun::discretize_vector(explanatory[,paramgd[[1]],drop = TRUE],
                                      n = paramgd[[2]],method = paramgd[[3]],
                                      seed = seed, ...)
    fd = factor_detector(response,xdisc)
    q = fd[[1]]
    names(q) = "qstatistic"
    return(q)
  }

  if (doclust) {
    parallel::clusterExport(cores, c("factor_detector"))
    out_g = parallel::parLapply(cores,parak,calcul_disc)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(parak,calcul_disc)
  }

  out_g = dplyr::bind_cols(paradf,out_g) %>%
    dplyr::group_by(x) %>%
    dplyr::slice_max(order_by = qstatistic,
                     with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-qstatistic) %>%
    as.list()

  if(return_disc){
    suppressMessages({resdisc = purrr::pmap_dfc(out_g,
                              \(x,k,method) sdsfun::discretize_vector(
                                x = explanatory[,x,drop = TRUE],
                                n = k, method = method, ...)) %>%
      purrr::set_names(out_g[[1]])})
    out_g = append(out_g,list("disv" = resdisc))
  }
  return(out_g)
}
