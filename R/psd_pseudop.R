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
#' wt = inverse_distance_weight(sim$lo,sim$la,power = 2)
#' psd_pseudop(sim$y,st_unidisc(sim$xa,5),wt)
#'
psd_pseudop = \(y,x,wt,cores = 1,
                seed = 123456789,
                permutations = 0){
  qs = psd_spade(y,x,wt)
  if (permutations == 0){
    fd = tibble::tibble("Q-statistic" = qs, "P-value" = "No Pseudo-P Value")
  } else {
    set.seed(seed)
    permutation = data.frame(
      x1 = stats::runif(permutations, min = 0, max = 1),
      x2 = rescale_vector(stats::rnorm(permutations), 0.001, 0.999)
    ) %>%
      split(.,seq_len(nrow(.)))

    doclust = FALSE
    if (cores > 1) {
      doclust = TRUE
      cores = parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cores), add=TRUE)
    }

    set.seed(seed)
    randomnum = stats::runif(1)
    xperm = shuffle_vector(x,randomnum,seed = seed)
    yperm = shuffle_vector(y,randomnum,seed = seed)
    calcul_psd = \(p_shuffle){
      yobs_shffule = shuffle_vector(yperm,p_shuffle[[2]],seed = seed)
      xobs_shffule = shuffle_vector(xperm,p_shuffle[[1]],seed = seed)
      return(psd_spade(yobs_shffule,xobs_shffule,wt))
    }

    if (doclust) {
      parallel::clusterExport(cores,c('st_unidisc','robust_disc','spvar',
                                      'psd_spade',"shuffle_vector"))
      out_g = parallel::parLapply(cores,permutation,calcul_psd)
      out_g = as.numeric(do.call(rbind, out_g))
    } else {
      out_g = purrr::map_dbl(permutation,calcul_psd)
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
#' @param discnum (optional) Number of multilevel discretization. Default will use `3:22`.
#' @param discmethod (optional) The discretization methods. Default will use `quantile`.
#' If `discmethod` is set to `robust`, the function `robust_disc()` will be used. Conversely,
#' if `discmethod` is set to `rpart`, the `rpart_disc()` function will be used. Others use
#' `st_unidisc()`. Currently, only one `discmethod` can be used at a time.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, use parallel computation.
#' @param seed (optional) Random seed number, default is `123456789`.
#' @param permutations (optional) The number of permutations for the PSD computation. Default is `0`,
#' which means no pseudo-p values are calculated.
#' @param ... (optional) Other arguments passed to `st_unidisc()`,`robust_disc()` or `rpart_disc()`.
#'
#' @return A tibble of power of spatial and multilevel discretization determinant and the corresponding pseudo-p value.
#' @export
#'
#' @examples
#' data('sim')
#' wt = inverse_distance_weight(sim$lo,sim$la)
#' psmd_pseudop(sim$y,sim$xa,wt)
#'
psmd_pseudop = \(yobs, xobs, wt, discnum = 3:22,
                 discmethod = 'quantile', cores = 1,
                 seed = 123456789, permutations = 0, ...){
  qs = psmd_spade(yobs,xobs,wt,discnum,discmethod,cores,seed,...)
  if (permutations == 0){
    fd = tibble::tibble("Q-statistic" = qs, "P-value" = "No Pseudo-P Value")
  } else {
    set.seed(seed)
    permutation = data.frame(
      x1 = stats::runif(permutations, min = 0, max = 1),
      x2 = rescale_vector(stats::rnorm(permutations), 0.001, 0.999)
    ) %>%
      split(.,seq_len(nrow(.)))

    doclust = FALSE
    if (cores > 1) {
      doclust = TRUE
      cores = parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cores), add=TRUE)
    }

    set.seed(seed)
    randomnum = stats::runif(1)
    xperm = shuffle_vector(xobs,randomnum,seed = seed)
    yperm = shuffle_vector(yobs,randomnum,seed = seed)
    wt_perm = wt
    seedn = seed
    discn = discnum
    discm = discmethod
    calcul_psmd = \(p_shuffle){
      xperm_new = shuffle_vector(xperm,p_shuffle[[1]],seed = seed)
      yperm_new = shuffle_vector(yperm,p_shuffle[[2]],seed = seed)
      return(psmd_spade(yperm_new,xperm_new,wt_perm,discn,discm,1,seedn,...))
    }

    if (doclust) {
      parallel::clusterExport(cores,c('st_unidisc','robust_disc','rpart_disc','spvar','shuffle_vector',
                                      'psd_spade','cpsd_spade','psmd_spade','inverse_distance_weight'))
      out_g = parallel::parLapply(cores,permutation,calcul_psmd)
      out_g = as.numeric(do.call(rbind, out_g))
    } else {
      out_g = purrr::map_dbl(permutation,calcul_psmd)
    }

    R = sum(out_g >= qs)
    pp = (R + 1) / (permutations + 1)
    fd = tibble::tibble("Q-statistic" = qs, "P-value" = pp)
  }
  return(fd)
}
