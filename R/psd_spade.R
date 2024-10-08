#' @title power of spatial determinant(PSD)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate power of spatial determinant `q_s`
#' @details
#' The power of spatial determinant formula is
#'
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
#' @return A value of power of spatial determinant `q_s`.
#' @export
#'
#' @examples
#' data('sim')
#' wt = inverse_distance_weight(sim$lo,sim$la,power = 2)
#' psd_spade(sim$y,st_unidisc(sim$xa,5),wt)
#'
psd_spade = \(y,x,wt){
  gdf = tibble::tibble(x = x, y = y,
                       id_sample = seq_along(y)) %>%
    dplyr::group_by(x) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = factor(x),
                  id_sample_new = seq_along(y))
  x = gdf$x
  y = gdf$y
  wt = wt[gdf$id_sample,gdf$id_sample]
  indices = gdf$id_sample_new
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
#' Function for calculate compensated power of spatial determinant `Q_s`.
#' @details
#' The power of compensated spatial determinant formula is
#'
#' \eqn{Q_s = \frac{q_s}{q_{s_{inforkep}}}
#' = \frac{1 - \frac{\sum_{h=1}^L N_h \Gamma_{kdep}}{N \Gamma_{totaldep}}}{1 - \frac{\sum_{h=1}^L N_h \Gamma_{hind}}{N \Gamma_{totalind}}}}
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param yobs Variable Y
#' @param xobs The original undiscretized covariable X.
#' @param xdisc The discretized covariable X.
#' @param wt The spatial weight matrix.
#'
#' @return A value of compensated power of spatial determinant `Q_s`.
#' @export
#'
#' @examples
#' data('sim')
#' wt = inverse_distance_weight(sim$lo,sim$la)
#' xa = sim$xa
#' xa_disc = st_unidisc(xa,5)
#' cpsd_spade(sim$y,xa,xa_disc,wt)
#'
cpsd_spade = \(yobs,xobs,xdisc,wt){
  return(psd_spade(yobs,xdisc,wt) / psd_spade(xobs,xdisc,wt))
}

#' @title power of spatial and multilevel discretization determinant(PSMD)
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate power of spatial and multilevel discretization determinant `PSMDQ_s`.
#' @details
#' The power of spatial and multilevel discretization determinant formula is
#' \eqn{PSMDQ_s = MEAN(Q_s)}
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param yobs Variable Y
#' @param xobs The original undiscretized covariable X.
#' @param wt The spatial weight matrix.
#' @param discnum (optional) Number of multilevel discretization. Default will use `3:22`.
#' @param discmethod (optional) The discretization methods. Default will use `quantile`.
#' If `discmethod` is set to `robust`, the function `robust_disc()` will be used. Conversely,
#' if `discmethod` is set to `rpart`, the `rpart_disc()` function will be used. Others use
#' `sdsfun::discretize_vector()`. Currently, only one `discmethod` can be used at a time.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, use parallel computation.
#' @param seed (optional) Random seed number, default is `123456789`.
#' @param ... (optional) Other arguments passed to `sdsfun::discretize_vector()`,`robust_disc()` or
#' `rpart_disc()`.
#'
#' @return A value of power of spatial and multilevel discretization determinant `PSMDQ_s`.
#' @export
#'
#' @examples
#' data('sim')
#' wt = inverse_distance_weight(sim$lo,sim$la)
#' psmd_spade(sim$y,sim$xa,wt)
#'
psmd_spade = \(yobs, xobs, wt, discnum = 3:22,
               discmethod = 'quantile', cores = 1,
               seed = 123456789, ...){
  doclust = FALSE
  if (cores > 1) {
    doclust = TRUE
    cores_rdisc = cores # distinguish between python and r parallel.
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  if (discmethod == 'rpart'){
    discdf = tibble::tibble(yobs = yobs,
                            xobs = xobs)
    xdisc = rpart_disc("yobs ~ .", data = discdf, ...)
    out_g = cpsd_spade(yobs,xobs,xdisc,wt)
  } else {
    wt_cpsd = wt # ensure that R can access wt during parallel computation.
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
        suppressMessages({discdf = discn %>%
          purrr::map_dfc(\(kn) sdsfun::discretize_vector(xv,kn,method = discm,...)) %>%
          purrr::set_names(paste0('xobs_',discn))})
        discdf = tibble::tibble(yobs = yv,
                                xobs = xv) %>%
          dplyr::bind_cols(discdf)
      }
      return(discdf)
    }
    discdf = spade_disc(yobs,xobs,discnum,discmethod,cores_rdisc,...)

    calcul_cpsd = \(paramn){
      yvar = discdf[,'yobs',drop = TRUE]
      xvar = discdf[,'xobs',drop = TRUE]
      xdisc = discdf[,paramn,drop = TRUE]
      return(cpsd_spade(yvar,xvar,xdisc,wt_cpsd))
    }

    if (doclust) {
      parallel::clusterExport(cores,c('robust_disc','psd_spade','cpsd_spade','spvar'))
      out_g = parallel::parLapply(cores,paste0('xobs_',discnum),calcul_cpsd)
      out_g = as.numeric(do.call(rbind, out_g))
    } else {
      out_g = purrr::map_dbl(paste0('xobs_',discnum),calcul_cpsd)
    }
    out_g = mean(out_g)
  }
  return(out_g)
}

#' @title measure information loss by information entropy
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for measure information loss by shannon information entropy.
#' @details
#' The information loss measured by information entropy formula is
#' \eqn{F = -\sum\limits_{i=1}^N p_{(i)}\log_2 p_{(i)} - \left(-\sum\limits_{h=1}^L p_{(h)}\log_2 p_{(h)}\right)}
#'
#' @param xvar The original undiscretized vector.
#' @param xdisc The discretized vector.
#'
#' @return A numeric value of information loss measured by information entropy.
#' @export
#'
#' @examples
#' F_informationloss(1:7,c('x',rep('y',3),rep('z',3)))
#'
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
