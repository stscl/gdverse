#' @title optimal univariate discretization based on geodetector q-statistic
#'
#' @param formula A formula.
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param discnum (optional) A vector of numbers of discretization. Default is `3:8`.
#' @param discmethod (optional) A vector of methods for discretization, default is using
#' `c("sd","equal","geometric","quantile","natural")` by invoking `sdsfun`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#' @param seed (optional) Random seed number, default is `123456789`.
#' @param ... (optional) Other arguments passed to `sdsfun::discretize_vector()`.
#'
#' @return A list.
#' \describe{
#' \item{\code{x}}{the name of the variable that needs to be discretized}
#' \item{\code{k}}{optimal discretization number}
#' \item{\code{method}}{optimal discretization method}
#' \item{\code{qstatistic}}{optimal q-statistic}
#' \item{\code{disc}}{optimal discretization results}
#' }
#' @export
#'
#' @examples
#' data('sim')
#' gd_optunidisc(y ~ xa + xb + xc,
#'               data = sim,
#'               discnum = 3:6)
#'
gd_optunidisc = \(formula, data, discnum = 3:8,
                  discmethod = c("sd","equal","geometric","quantile","natural"),
                  cores = 1, seed = 123456789, ...){
  doclust = FALSE
  if (cores > 1) {
    doclust = TRUE
    cl = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add=TRUE)
  }

  if (inherits(data,"sf")){
    data = sf::st_drop_geometry(data)
  }

  formulavars = sdsfun::formula_varname(formula,data)
  response = data[, formulavars[[1]], drop = TRUE]
  explanatory = data[, formulavars[[2]]]

  discname = names(explanatory)
  paradf = tidyr::crossing("x" = discname,
                           "k" = discnum,
                           "method" = discmethod)
  parak = split(paradf, seq_len(nrow(paradf)))

  calcul_disc = \(paramgd){
    xdisc = sdsfun::discretize_vector(explanatory[,paramgd[[1]],drop = TRUE],
                                      n = paramgd[[2]],method = paramgd[[3]],
                                      seed = seed, ...)
    q = gdverse::factor_detector(response,xdisc)[[1]]
    names(q) = "qstatistic"
    return(q)
  }

  if (doclust) {
    out_g = parallel::parLapply(cl,parak,calcul_disc)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(parak,calcul_disc)
  }

  out_g = dplyr::bind_cols(paradf,out_g) %>%
    dplyr::group_by(x) %>%
    dplyr::slice_max(order_by = qstatistic,
                     with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::everything(),qstatistic) %>%
    as.list()

  suppressMessages({resdisc = purrr::pmap_dfc(out_g[1:3],
                                              \(x,k,method) sdsfun::discretize_vector(
                                                x = explanatory[,x,drop = TRUE],
                                                n = k, method = method, seed = seed, ...)) %>%
    purrr::set_names(out_g[[1]])})
  out_g = append(out_g,list("disc" = resdisc))

  return(out_g)
}
