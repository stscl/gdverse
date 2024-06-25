#' @title interactive detector for spatial associations(IDSA)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for interactive detector for spatial associations model.
#' @references
#' Yongze Song & Peng Wu (2021) An interactive detector for spatial associations,
#' International Journal of Geographical Information Science, 35:8, 1676-1701,
#' DOI:10.1080/13658816.2021.1882680
#'
#' @param formula A formula of IDSA model.
#' @param data A data.frame or tibble of observation data.
#' @param wt (optional) The spatial weight matrix.When `wt` is not provided, must provide `locations`.
#' And `gdverse` will use `locations` columns to construct spatial weight use `inverse_distance_weight()`.
#' @param overlaymethod (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `and`.
#' @param locations (optional) The geospatial locations coordinate columns name which in `data`.
#' Useful and must provided when `wt` is not provided. When `wt` is provided, `locations` is not need.
#' @param discvar Name of continuous variable columns that need to be discretized.Noted that
#' when `formula` has `discvar`, `data` must have these columns.
#' @param discnum (optional) Number of multilevel discretization.Default will use `3:22`.
#' @param discmethod (optional) The discretization methods. Default all use `quantile`. More details to see `st_unidisc()`.
#' @param strategy (optional) Discretization strategy. When `strategy` is `1L`, choose the highest SPADE model q-statistics to
#' determinate optimal spatial data discretization parameters. When `strategy` is `2L`, The optimal discrete parameters of
#' spatial data are selected by combining LOESS model.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#' @param seed (optional) Random number seed, default is `123456789`.
#' @param ... (optional) Other arguments passed to `cpsd_disc()`.
#'
#' @return A tibble with PID values under different spatial overlays.
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
#' idsa(SUHI ~ NDVI + BH, data = usfi,wt = wt,
#'      discvar = c('NDVI','BH'),cores = 6)
#' }
idsa = \(formula, data, wt = NULL, overlaymethod = 'and', locations = NULL,
         discvar = NULL, discnum = NULL, discmethod = NULL, strategy = 2L,
         increase_rate = 0.05, cores = 6, seed = 123456789, ...){

  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(c(formula.vars,locations)))
  }
  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) %in% c(formula.vars[1],locations))]
  discdf =  dplyr::select(data,dplyr::all_of(c(yname,discvar)))

  if (is.null(wt)) {
    if (is.null(locations)) {
      stop("When `wt` is not provided, please provided `locations` coordinate columns name which in `data` !")
    } else {
      locations = data[, locations]
      wt_idsa = inverse_distance_weight(locations[,1,drop = TRUE],
                                        locations[,2,drop = TRUE])
    }
  } else {
    wt_idsa = wt
  }

  g = cpsd_disc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                data = discdf, wt = wt_idsa, discnum = discnum,
                discmethod = discmethod, strategy = strategy,
                increase_rate = increase_rate,
                cores = cores, seed = seed, ...)
  discedvar = colnames(data[,-which(colnames(data) %in% c(discvar,locations))])
  newdti = data %>%
    dplyr::select(dplyr::all_of(discedvar)) %>%
    dplyr::bind_cols(g$disv)
  dti = dplyr::select(data,dplyr::all_of(names(newdti)))
  xs = generate_subsets(xname,empty = FALSE, self = TRUE)
  spfom = overlaymethod

  calcul_pid = \(.x){
    if (length(.x) == 1) {
      qv = cpsd_spade(
        dti[,yname,drop = TRUE],
        dti[,.x,drop = TRUE],
        newdti[,.x,drop = TRUE],
        wt_idsa)
    } else {
      qv = pid_idsa(paste(yname,'~',paste0(.x,collapse = '+')),
                    dti, newdti, wt_idsa, spfom)
    }
    names(qv) = 'pid_idsa'
    return(qv)
  }

  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('spvar','psd_spade','cpsd_spade','psd_iev',
                                    'st_fuzzyoverlay','pid_idsa'))
    out_g = parallel::parLapply(cores,xs, calcul_pid)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xs, calcul_pid)
  }
  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  xsname = purrr::map_chr(xs,\(.x) paste(.x,collapse = IntersectionSymbol))
  out_g = tibble::tibble(varibale = xsname) %>%
    dplyr::bind_cols(out_g) %>%
    dplyr::arrange(dplyr::desc(pid_idsa))
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
#' @param rawdata Discrete explanatory variables data
#' @param spzone Fuzzy overlay spatial zones
#' @param wt Spatial weight matrix
#'
#' @return The Value of \code{PSD-IEV}
#' @export
#'
psd_iev = \(rawdata,spzone,wt){
  xname = names(rawdata)
  totalsv = purrr::map_dbl(rawdata,
                           \(.x) spvar(.x, wt))
  qv = purrr::map_dbl(rawdata,
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
