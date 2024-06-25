idsa = \(formula, data, wt = NULL, overlaymethod = 'and',
         locations = NULL, discvar = NULL, discnum = NULL, discmethod = NULL,
         strategy = 2L, increase_rate = 0.05, cores = 6, seed = 123456789, ...){
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

  if (overlaymethod == 'and')

  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(c(formula.vars,locations)))
  }
  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) %in% c(formula.vars[1],locations))]
  discdf =  dplyr::select(data,dplyr::all_of(c(yname,discvar)))
  g = cpsd_disc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                wt = wt_idsa, data = discdf, discnum = discnum,
                discmethod = discmethod, strategy = strategy,
                increase_rate = increase_rate,
                cores = cores, seed = seed, ...)
  discedvar = colnames(data[,-which(colnames(data) %in% c(discvar,locations))])
  newdata = data %>%
    dplyr::select(dplyr::all_of(discedvar)) %>%
    dplyr::bind_cols(g$disv)
  data = dplyr::select(dplyr::all_of(names(newdata)))
  qv = xname %>%
    purrr::map_dbl(\(.x) cpsd_spade(
      data[,yname,drop = TRUE],
      data[,xname,drop = TRUE],
      newdata[,xname,drop = TRUE],
      wt_idsa))
  interactvar = xname[which.max(qv)]
  xname = xname[-which.max(qv)]
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

#' IDSA Q-saistics
#' @details
#' \eqn{Q_{IDSA} = \frac{\theta_r}{\phi}}
#'
#' @param formula A formula for IDSA Q-saistics \code{PID}
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
