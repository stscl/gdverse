#' @title power of spatial determinant(PSD)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate power of spatial determinant \eqn{q_s}.
#' @details
#' The power of spatial determinant formula is
#' \eqn{q_s = 1 - \frac{\sum_{h=1}^L N_h \Gamma_h}{N \Gamma}}
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariable X, \code{factor}, \code{character} or \code{discrete numeric}.
#' @param wt The spatial weight matrix.
#'
#' @return A value of power of spatial determinant \eqn{q_s}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(tidyverse)
#' ntdspath = system.file("extdata", "NTDs.gpkg",package = 'gdverse')
#' watershed = read_sf(ntdspath,layer = 'watershed')
#' elevation = read_sf(ntdspath,layer = 'elevation')
#' soiltype = read_sf(ntdspath,layer = 'soiltype')
#' disease = read_sf(ntdspath,layer = 'disease')
#' NTDs = disease %>%
#'   st_centroid() %>%
#'   st_join(watershed[,"watershed"]) %>%
#'   st_join(elevation[,"elevation"]) %>%
#'   st_join(soiltype[,"soiltype"])%>%
#'   dplyr::filter(if_all(everything(),~!is.na(.x)))
#' NTDs = NTDs %>%
#'   st_coordinates() %>%
#'   dplyr::bind_cols(NTDs,.)
#' wt = inverse_distance_weight(NTDs$X,NTDs$Y)
#' psd_spade(NTDs$disease,NTDs$soiltype,wt)
#' }

psd_spade = \(y,x,wt){
  gdf = tibble::tibble(x = x, y = y,
                       id_sample = seq_along(y)) %>%
    dplyr::group_by(x) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = factor(x))
  x = gdf$x
  y = gdf$y
  indices = gdf$id_sample
  sprss = \(indice){
    yn = y[indice]
    wtn = wt[indice,indice]
    return(length(yn) * spvar(yn,wtn))
  }
  qv = 1 - sum(tapply(indices, x, sprss)) / (length(y) * spvar(y,wt))
  return(qv)
}

#' @title compensated power of spatial determinant(CPSD)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate compensated power of spatial determinant \eqn{Q_s}.
#' @details
#' The power of compensated spatial determinant formula is
#' \eqn{Q_s = \frac{q_s}{q_{s_{inforkep}}}
#' = \frac{1 - \frac{\sum_{h=1}^L N_h \Gamma_{kdep}}{N \Gamma_{totaldep}}}{1 - \frac{\sum_{h=1}^L N_h \Gamma_{hind}}{N \Gamma_{totalind}}}}
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param yobs Variable Y
#' @param xobs The original un-discretized covariable X.
#' @param xdisc The discretized covariable X.
#' @param wt The spatial weight matrix.
#'
#' @return A value of compensated power of spatial determinant \eqn{Q_s}.
#' @export
cpsd_spade = \(yobs,xobs,xdisc,wt){
  return(psd_spade(yobs,xdisc,wt) / psd_spade(xobs,xdisc,wt))
}

#' @title power of spatial and multilevel discretization determinant(PSMD)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate power of spatial and multilevel discretization determinant \eqn{PSMDQ_s}.
#' @details
#' The power of spatial and multilevel discretization determinant formula is
#' \eqn{PSMDQ_s = MEAN(Q_s)}
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param formula A formula of calculate power of spatial and multilevel discretization determinant \eqn{PSMDQ_s}.
#' @param data A data.frame or tibble of observation data.
#' @param wt (optional) The spatial weight matrix.When `wt` is not provided, must provide `locations`.
#' And gdverse will use `locations` columns to construct spatial weight use `inverse_distance_weight()`.
#' @param locations (optional) The geospatial locations coordinate columns name which in `data`.
#' Useful and must provided when `wt` is not provided.
#' @param discnum (optional) Number of multilevel discretization.Default will use `3:15`.
#' @param discmethod (optional) The discretization methods. Default will use `quantile`.
#' When `discmethod` is `robust` use `robust_disc()`, others use `st_unidisc()`.Now only support
#' one `discmethod` at one time.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, use parallel computation.
#' @param seed (optional) Random seed number, default is `123456789`.
#' @param ... (optional) Other arguments passed to `st_unidisc()` or `robust_disc()`.
#'
#' @return A value of power of spatial and multilevel discretization determinant \eqn{PSMDQ_s}.
#' @importFrom purrr map_dbl map_dfc set_names
#' @export
psmd_spade = \(formula,data,wt = NULL,locations = NULL,discnum = NULL,
               discmethod = 'quantile',cores = 1,seed = 123456789,...){
  doclust = FALSE
  if (cores > 1) {
    doclust = TRUE
    cores_rdisc = cores # distinguish between python and r parallel.
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  yobs = data[, formula.vars[1], drop = TRUE]
  if (formula.vars[2] == "."){
    if (length(!(which(colnames(data) %in% c(formula.vars[1],locations)))) > 1) {
      stop('please only keep `dependent` and `independent` columns in `data`; When `wt` is not provided, please make sure `locations` coordinate columns is also contained in `data` .')
    } else {
      xobs = data[, -which(colnames(data) %in% c(formula.vars[1],locations)), drop = TRUE]
    }
  } else {
    xobs = data[, which(colnames(data) == formula.vars[2]), drop = TRUE]
  }

  if (is.null(wt)) {
    if (is.null(locations)) {
      stop("When `wt` is not provided, please provided `locations` coordinate columns name which in `data` !")
    } else {
      wt_spade = inverse_distance_weight(locations[,1],locations[,2],power = 1)
    }
  } else {
    wt_spade = wt
  }
  if (is.null(discnum)) {
    discn = 3:15
  } else {
    discn = discnum
  }

  spade_disc = \(yv,xv,discn,discm,cores,...){
    if (discm == 'robust') {
      discdf = rep(list("xobs" = xv),length(discn))
      names(discdf) = paste0('xobs_',discn)
      discdf = tibble::tibble(yobs = yv,
                              xobs = xv) %>%
        dplyr::bind_cols(tibble::as_tibble(discdf))
      discdf = robust_disc("yobs ~ .",
                           data = discdf,
                           discnum = discn,
                           cores = cores,
                           ...)
    } else {
      discdf = discn %>%
        purrr::map_dfc(\(kn) st_unidisc(xv,kn,method = discm,...)) %>%
        purrr::set_names(paste0('xobs_',discn))
      discdf = tibble::tibble(yobs = yv,
                              xobs = xv) %>%
        dplyr::bind_cols(discdf)
    }

    return(discdf)
  }
  discdf = spade_disc(yobs,xobs,discn,discmethod,cores_rdisc,...)

  calcul_cpsd = \(paramn){
    yvar = discdf[,'yobs',drop = TRUE]
    xvar = discdf[,'xobs',drop = TRUE]
    xdisc = discdf[,paramn,drop = TRUE]
    return(cpsd_spade(yvar,xvar,xdisc,wt_spade))
  }

  if (doclust) {
    parallel::clusterExport(cores,c('st_unidisc','robust_disc','cpsd_spade'))
    out_g = parallel::parLapply(cores,paste0('xobs_',discn),calcul_cpsd)
    out_g = as.numeric(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dbl(paste0('xobs_',discn),calcul_cpsd)
  }

    return(mean(out_g))
}

#' @title measure information loss by information entropy
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for measure information loss by information entropy.
#' @details
#' The power of spatial determinant formula is
#' \eqn{F = -\sum\limits_{i=1}^N p_{(i)}\log_2 p_{(i)} - \left(-\sum\limits_{h=1}^L p_{(h)}\log_2 p_{(h)}\right)}
#'
#' @param xvar The original un-discretized vector.
#' @param xdisc The discretized vector.
#'
#' @return A value of information loss as measured by information entropy.
#' @importFrom dplyr count mutate pull
#' @importFrom tibble tibble
#' @export
F_informationloss = \(xvar,xdisc){
  gdf = tibble::tibble(xvar = xvar,
                       xdisc = xdisc)
  term1 = gdf %>%
    dplyr::count(xvar) %>%
    dplyr::mutate(n = n / sum(n)) %>%
    dplyr::pull(n)
  term2 = gdf %>%
    dplyr::count(xdisc) %>%
    dplyr::mutate(n = n / sum(n)) %>%
    dplyr::pull(n)
  information_entropy = \(p) sum(p*log2(p)) * -1
  Fiiv = information_entropy(term1) - information_entropy(term2)
  return(Fiiv)
}
