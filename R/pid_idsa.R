cpsd_disc =  \(formula,data,wt,discnum = NULL,discmethod = NULL,
               cores = 1,return_disc = TRUE,seed = 123456789,...){
  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }
  if (is.null(discmethod)) {
    discmethod = c("sd","equal","pretty","quantile","fisher","headtails","maximum","box")
  }
  if (is.null(discnum)){discnum = 3:15}

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
    xobs = explanatory[,paramgd[[1]],drop = TRUE]
    xdisc = st_unidisc(xobs, k = paramgd[[2]],
                       method = paramgd[[3]],
                       seed = seed, ...)
    q = cpsd_spade(response,xobs,xdisc,wt)
    names(q) = "spade_cpsd"
    return(q)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('st_unidisc','psd_spade','cpsd_spade','spvar'))
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
  return(out_g)

}
