esp = \(formula, data, wt = NULL, discvar = NULL,
        discnum = 10, overlaymethod = 'and',
        minsize = 1, cores = 1, alpha = 0.95){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)

  if (inherits(data,'sf')) {
    if (is.null(wt)){
      wt_esp = sdsfun::inverse_distance_swm(data)
    } else {
      wt_esp = wt
    }
    data = sf::st_drop_geometry(data)
  } else if (inherits(data,'data.frame')) {
    if (is.null(wt)){
      stop("When `data` is `data.frame` or `tibble`, please provide `wt` in idsa input!")
    } else {
      wt_esp = wt
    }
  }

  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }

  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) == yname)]
  if (is.null(discvar)) {
    xdiscname = xname
    xundiscname = NULL
  } else {
    xdiscname = discvar
    xundiscname = xname[-which(xname %in% discvar)]
  }
  discdf = dplyr::select(data,dplyr::all_of(c(yname,xdiscname)))

  cores_disc = cores
  dti = robust_disc(paste0(yname,"~ ."), discdf, discnum, minsize, cores_disc)
  if (!is.null(xundiscname)){
    dti = data %>%
      dplyr::select(dplyr::any_of(c(yname,xundiscname))) %>%
      dplyr::bind_cols(dti)
  } else {
    dti = data %>%
      dplyr::select(dplyr::any_of(yname)) %>%
      dplyr::bind_cols(dti)
  }

  cores_spvar = cores
  xs = generate_subsets(xname,empty = FALSE, self = TRUE)
  spfom = overlaymethod

  psd_esp = \(formula, discdata, wt,
              overlaymethod = 'and'){
    formula = stats::as.formula(formula)
    formula.vars = all.vars(formula)
    if (formula.vars[2] != "."){
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
    qtheta = psd_spade(discdata[,yname,drop = TRUE],
                       fuzzyzone, wt)
    return(qtheta)
  }

  calcul_psd = \(.x){
    qv = psd_esp(paste(yname,'~',paste0(.x,collapse = '+')),
                 dti, wt_esp, spfom)
    names(qv) = 'psd'
    return(qv)
  }

  doclust = FALSE
  if (cores_spvar > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores_spvar)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('spvar','psd_spade','st_fuzzyoverlay'))
    out_g = parallel::parLapply(cores, xs, calcul_psd)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xs, calcul_psd)
  }
 return(out_g)

}
