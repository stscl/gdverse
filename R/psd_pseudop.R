#' @title calculate power of spatial determinant(PSD) and the corresponding pseudo-p value
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
#' @param cores (optional) A positive integer(default is 1). If cores > 1, use parallel computation.
#' @param seed (optional) Random seed number, default is `123456789`.
#' @param permutations (optional) The number of permutations for the PSD computation. Default is `0`,
#' which means no pseudo-p values are calculated.
#'
#' @return A tibble of power of spatial determinant and the corresponding pseudo-p value.
#' @export
#'
#' @examples
#' data('sim')
#' wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')),
#'                                   power = 2)
#' psd_pseudop(sim$y,sdsfun::discretize_vector(sim$xa,5),wt)
#'
psd_pseudop = \(y,x,wt,cores = 1,
                seed = 123456789,
                permutations = 0){
  qs = gdverse::psd_spade(y,x,wt)
  if (permutations == 0){
    fd = tibble::tibble("Q-statistic" = qs, "P-value" = "No Pseudo-P Value")
  } else {
    set.seed(seed)
    random_seed = sample(1:100, 1)
    random_seeds = seq(random_seed, by = 1,
                       length.out = as.integer(permutations))

    doclust = FALSE
    if (cores > 1) {
      doclust = TRUE
      cores = parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cores), add=TRUE)
    }

    calcul_psd = \(seed,y,x,wt){
      yobs_shffule = gdverse::gen_permutations(y,seed)
      return(gdverse::psd_spade(yobs_shffule,x,wt))
    }

    if (doclust) {
      out_g = parallel::parLapply(cores,random_seeds,calcul_psd,
                                  y = y, x = x, wt = wt)
      out_g = as.numeric(do.call(rbind, out_g))
    } else {
      out_g = purrr::map_dbl(random_seeds,calcul_psd,
                             y = y, x = x, wt = wt)
    }

    R = sum(out_g >= qs)
    pp = (R + 1) / (permutations + 1)
    fd = tibble::tibble("Q-statistic" = qs, "P-value" = pp)
  }
  return(fd)
}

#' @title power of spatial and multilevel discretization determinant(PSMD) and the corresponding pseudo-p value
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate power of spatial and multilevel discretization determinant and the corresponding pseudo-p value.
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
#' @param discnum (optional) Number of multilevel discretization. Default will use `3:8`.
#' @param discmethod (optional) The discretization methods. Default will use `quantile`.
#' If `discmethod` is set to `robust`, the function `robust_disc()` will be used. Conversely,
#' if `discmethod` is set to `rpart`, the `rpart_disc()` function will be used. Others use
#' `sdsfun::discretize_vector()`. Currently, only one `discmethod` can be used at a time.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, use parallel computation.
#' @param seed (optional) Random seed number, default is `123456789`.
#' @param permutations (optional) The number of permutations for the PSD computation. Default is `0`,
#' which means no pseudo-p values are calculated.
#' @param ... (optional) Other arguments passed to `sdsfun::discretize_vector()`,`robust_disc()` or `rpart_disc()`.
#'
#' @return A tibble of power of spatial and multilevel discretization determinant and the corresponding pseudo-p value.
#' @export
#'
#' @examples
#' data('sim')
#' wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')))
#' psmd_pseudop(sim$y,sim$xa,wt)
#'
psmd_pseudop = \(yobs, xobs, wt, discnum = 3:8,
                 discmethod = 'quantile', cores = 1,
                 seed = 123456789, permutations = 0, ...){
  qs = gdverse::psmd_spade(yobs,xobs,wt,discnum,discmethod,cores,seed,...)
  if (permutations == 0){
    fd = tibble::tibble("Q-statistic" = qs, "P-value" = "No Pseudo-P Value")
  } else {
    set.seed(seed)
    random_seed = sample(1:100, 1)
    random_seeds = seq(random_seed, by = 1,
                       length.out = as.integer(permutations))

    doclust = FALSE
    if (cores > 1) {
      doclust = TRUE
      cores = parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cores), add=TRUE)
    }

    seedn = seed
    calcul_psmd = \(seed,y,x,wt,discnum,discmethod...){
      yobs_shffule = gdverse::gen_permutations(y,seed)
      return(gdverse::psmd_spade(yobs_shffule,x,wt,discnum,discmethod,
                                 cores = 1, seed = seedn, ...))
    }

    if (doclust) {
      out_g = parallel::parLapply(cores,random_seeds,calcul_psmd,y = y, x = x, wt = wt,
                                  discnum = discnum,discmethod = discmethod, ...)
      out_g = as.numeric(do.call(rbind, out_g))
    } else {
      out_g = purrr::map_dbl(random_seeds,calcul_psmd,y = y, x = x, wt = wt,
                             discnum = discnum,discmethod = discmethod, ...)
    }

    R = sum(out_g >= qs)
    pp = (R + 1) / (permutations + 1)
    fd = tibble::tibble("Q-statistic" = qs, "P-value" = pp)
  }
  return(fd)
}
