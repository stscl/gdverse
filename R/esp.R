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
  cores_shap = cores
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
    names(qv) = 'PSD'
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
  out_psd = dplyr::pull(out_g,1)
  m = length(xname)
  mf = factorial(m)

  get_value_by_varname = \(fv,namelist,valuevec){
    for (ni in seq_along(namelist)) {
      if (setequal(fv,namelist[[ni]])) {
        res = valuevec[ni]
        break
      } else {
        res = 0
      }
    }
    return(res)
  }

  calcul_shap = \(xvar){
    fullxvar = xname[-which(xname == xvar)]
    fullvar = generate_subsets(fullxvar,empty = FALSE,self = TRUE)

    calcul_unishap = \(xvar,fullxvar,namelist,valuevec){
      n = length(fullxvar)
      v1 = get_value_by_varname(c(xvar,fullxvar),namelist,valuevec)
      v2 = get_value_by_varname(fullxvar,namelist,valuevec)
      thetax = factorial(n) * factorial(m - n - 1) * (v1 - v2) / mf
      return(thetax)
    }

    thetaxs = purrr::map_dbl(fullvar, \(.x) calcul_unishap(xvar,.x,xs,out_psd))
    thetax = sum(thetaxs)
    names(thetax) = 'SPD'
    return(thetax)
  }

  doclust = FALSE
  if (cores_shap > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores_shap)
    on.exit(parallel::stopCluster(cores), add = TRUE)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('generate_subsets'))
    out_g = parallel::parLapply(cores,xname,calcul_shap)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xname,calcul_shap)
  }
  out_spd = dplyr::pull(out_g,1)

  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  xsname = purrr::map_chr(xs,\(.x) paste(.x,collapse = IntersectionSymbol))
  interactvar = xs[[which.max(out_psd)]]
  if (overlaymethod == 'intersection'){
    reszone = dti %>%
      dplyr::select(dplyr::all_of(interactvar)) %>%
      purrr::reduce(paste,sep = '_')
  } else {
    reszone = st_fuzzyoverlay(paste(yname,'~',paste0(interactvar,collapse = '+')),
                              dti, spfom)
  }
  zonenum = as.numeric(table(reszone))
  percentzone = length(which(zonenum==1)) / length(reszone)
  risk1 = risk_detector(dti[,yname,drop = TRUE],reszone,alpha)
  risk2 = risk1 %>%
    dplyr::select(dplyr::all_of(c('zone1st','zone2nd','Risk'))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(c('zone1st','zone2nd')),
                        names_to = 'zone', values_to = 'zone_risk') %>%
    dplyr::distinct(zone_risk,.keep_all = TRUE)
  risk2 = tibble::tibble(reszone = paste0('zone',reszone)) %>%
    dplyr::left_join(risk2, by = c('reszone' = 'zone_risk')) %>%
    dplyr::pull(Risk)
  res_psd = tibble::tibble(variable = xsname,
                           psd = out_psd) %>%
    dplyr::arrange(dplyr::desc(psd))
  res_spd = tibble::tibble(variable = xname,
                           spd = out_spd) %>%
    dplyr::arrange(dplyr::desc(spd))

  res = list("psd" = res_psd, "spd" = res_spd, "risk1" = risk1, "risk2" = risk2,
             "number_individual_explanatory_variables" = length(interactvar),
             "number_overlay_zones" = length(zonenum),
             "percentage_finely_divided_zones" =  percentzone)
  class(res) = "esp_result"
  return(res)
}
