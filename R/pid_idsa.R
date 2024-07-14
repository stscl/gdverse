#' @title optimal spatial data discretization based on SPADE q-statistics
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for determining the optimal spatial data discretization based on SPADE q-statistics.
#' @references
#' Yongze Song & Peng Wu (2021) An interactive detector for spatial associations,
#' International Journal of Geographical Information Science, 35:8, 1676-1701,
#' DOI:10.1080/13658816.2021.1882680
#'
#' @param formula A formula of optimal spatial data discretization.
#' @param data A data.frame or tibble of observation data.
#' @param wt The spatial weight matrix.
#' @param discnum (optional) A vector of number of classes for discretization. Default is `3:22`.
#' @param discmethod (optional) A vector of methods for discretization,default is all using `quantile`in `gdverse`.
#' see `method` parameter in `st_unidisc()` for more details.
#' @param strategy (optional) Discretization strategy. When `strategy` is `1L`, choose the highest SPADE model q-statistics to
#' determinate optimal spatial data discretization parameters. When `strategy` is `2L`, The optimal discrete parameters of
#' spatial data are selected by combining LOESS model.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#' @param return_disc (optional) Whether or not return discretized result used the optimal parameter.
#' Default is `TRUE`.
#' @param seed (optional) Random seed number, default is `123456789`.Setting random seed is useful when
#' the sample size is greater than `3000`(the default value for `largeN`) and the data is discretized
#' by sampling `10%`(the default value for `samp_prop` in `st_unidisc()`).
#' @param ... (optional) Other arguments passed to `st_unidisc()`.
#'
#' @return A list with the optimal parameter in the provided parameter combination with `k`,
#' `method` and `disc`(when `return_disc` is `TRUE`).
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' usfi = read_sf(system.file('extdata/USFI_Xian.gpkg',package = 'gdverse')) |>
#'   dplyr::select(dplyr::all_of(c("NDVI","BH","SUHI")))
#' coord = usfi |>
#'   st_centroid() |>
#'   st_coordinates()
#' wt = inverse_distance_weight(coord[,1],coord[,2])
#' usfi = st_drop_geometry(usfi)
#' cpsd_disc(SUHI ~ NDVI + BH, data = usfi,wt = wt,cores = 6)
#' }
cpsd_disc =  \(formula, data, wt, discnum = NULL, discmethod = NULL, strategy = 2L,
               increase_rate = 0.05,cores = 1,return_disc = TRUE,seed = 123456789,...){
  if (!(strategy %in% c(1L,2L))){
    stop("`strategy` must `1L` or `2L`!")
  }

  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }
  if (is.null(discmethod)) {discmethod = "quantile"}
  if (is.null(discnum)){discnum = 3:22}

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
  wtn = wt

  calcul_disc = \(paramgd, ...){
    xobs = explanatory[,paramgd[[1]],drop = TRUE]
    xdisc = st_unidisc(xobs, k = paramgd[[2]],
                       method = paramgd[[3]],
                       seed = seed, ...)
    q = cpsd_spade(response,xobs,xdisc,wtn)
    names(q) = "spade_cpsd"
    return(q)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('st_unidisc','psd_spade','cpsd_spade','spvar'))
    out_g = parallel::parLapply(cores,parak,calcul_disc,...)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(parak,calcul_disc)
  }

  if (strategy == 1L) {
    out_g = dplyr::bind_cols(paradf,out_g) %>%
      dplyr::group_by(x) %>%
      dplyr::slice_max(order_by = spade_cpsd,
                       with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(-spade_cpsd) %>%
      as.list()
  } else {
    out_g = dplyr::bind_cols(paradf,out_g)
    out_param = tidyr::expand(out_g,tidyr::nesting(x,method))
    optimalk = out_param$x %>%
      purrr::map2_dbl(out_param$method, \(.x,.method) loess_optdiscnum(
        out_g[which(out_g$x==.x & out_g$method==.method),"spade_cpsd",drop = TRUE],
        out_g[which(out_g$x==.x & out_g$method==.method),"k",drop = TRUE],increase_rate))
    out_g = out_param %>%
      dplyr::mutate(k = optimalk) %>%
      dplyr::select(x,k,method) %>%
      as.list()
  }

  if(return_disc){
    resdisc = purrr::pmap_dfc(out_g,
                              \(x,k,method) st_unidisc(x = explanatory[,x,drop = TRUE],
                                                       k = k, method = method, ...)) %>%
      purrr::set_names(out_g[[1]])
    out_g = append(out_g,list("disv" = resdisc))
  }
  return(out_g)
}

#' PSD of an interaction of explanatory variables (PSD-IEV)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @references
#' Yongze Song & Peng Wu (2021) An interactive detector for spatial associations,
#' International Journal of Geographical Information Science, 35:8, 1676-1701,
#' DOI:10.1080/13658816.2021.1882680
#'
#' @details
#' \eqn{\phi = 1 - \frac{\sum_{i=1}^m \sum_{k=1}^{n_i}N_{i,k}\tau_{i,k}}{\sum_{i=1}^m N_i \tau_i}}
#'
#' @param discdata Discreted explanatory variables data. A tibble or dataframe .
#' @param spzone Fuzzy overlay spatial zones. Returned from `st_fuzzyoverlay()`.
#' @param wt Spatial weight matrix
#'
#' @return The Value of \code{PSD-IEV}
#' @export
#'
#' @examples
#' data('NTDs')
#' wt = inverse_distance_weight(NTDs$X,NTDs$Y,power = 2)
#' sz = st_fuzzyoverlay(incidence ~ watershed + elevation + soiltype,
#'                      data = NTDs)
#' psd_iev(dplyr::select(NTDs,-c(X,Y,incidence)),sz,wt)
#'
psd_iev = \(discdata,spzone,wt){
  xname = names(discdata)
  totalsv = purrr::map_dbl(discdata,
                           \(.x) spvar(.x, wt))
  qv = purrr::map_dbl(discdata,
                      \(.y) psd_spade(.y,spzone,wt)) %>%
    {(-1*. + 1)*totalsv}
  return(1 - sum(qv) / sum(totalsv))
}

#' IDSA Q-saistics \code{PID}
#' @details
#' \eqn{Q_{IDSA} = \frac{\theta_r}{\phi}}
#'
#' @param formula A formula for IDSA Q-saistics
#' @param rawdata Raw observation data
#' @param discdata Discrete explanatory variables data
#' @param wt Spatial weight matrix
#' @param overlaymethod (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `and`.
#'
#' @return The value of IDSA Q-saistics \code{PID}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' usfi = read_sf(system.file('extdata/USFI_Xian.gpkg',package = 'gdverse')) |>
#'   dplyr::select(dplyr::all_of(c("NDVI","BH","WAR","SUHI")))
#' coord = usfi |>
#'   st_centroid() |>
#'   st_coordinates()
#' wt = inverse_distance_weight(coord[,1],coord[,2])
#' usf = usfi |>
#'   st_drop_geometry() |>
#'   dplyr::mutate(dplyr::across(1:3,\(.x) st_unidisc(.x,12)))
#' pid_idsa('NDVI~.',rawdata = usfi,discdata = usf,wt = wt)
#' }
pid_idsa = \(formula, rawdata, discdata,
             wt, overlaymethod = 'and'){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (formula.vars[2] != "."){
    rawdata = dplyr::select(rawdata,dplyr::all_of(formula.vars))
    discdata = dplyr::select(discdata,dplyr::all_of(formula.vars))
  }
  yname = formula.vars[1]
  if (overlaymethod == 'intersection'){
    fuzzyzone = discdata %>%
      dplyr::select(-dplyr::any_of(yname)) %>%
      purrr::reduce(paste,sep = '_')
  } else {
    fuzzyzone = st_fuzzyoverlay(formula,discdata,overlaymethod)
  }

  qtheta = psd_spade(rawdata[,yname,drop = TRUE],
                     fuzzyzone, wt)
  qphi = psd_iev(dplyr::select(discdata,-dplyr::any_of(yname)),
                 fuzzyzone, wt)
  return(qtheta / qphi)
}
