idsa = \(formula, data, wt = NULL, locations = NULL,
         discvar = NULL, discnum = NULL, discmethod = NULL,
         strategy = 2L, increase_rate = 0.05, cores = 6,
         seed = 123456789, ...){
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

  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  data = dplyr::select(data,-dplyr::any_of(locations))
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(c(formula.vars,locations)))
  }
  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) %in% c(formula.vars[1],locations))]
  xname_disc = xname[which(xname %in% discvar)]
  if (is.null(discmethod)) {discmethod = rep('quantile',length(xname_disc))}
  qv_disc = vector("list",length = length(xname_disc))
  for (i in seq_along(xname_spade)){
    qv_spade[[i]] = psmd_pseudop(
      formula = paste(yname,'~',xname_spade[i]),
      data = dplyr::select(data,
                           dplyr::all_of(c(yname,xname_spade[i],locations))),
      wt = wt, locations = locations, discnum = discnum, discmethod = discmethod[i],
      cores = cores, seed = seed, permutations = permutations, ...)
  }
  qv_spade = purrr::list_rbind(qv_spade) %>%
    dplyr::mutate(variable = xname_spade) %>%
    dplyr::select(variable,dplyr::everything())
  if (length(xname[which(!(xname %in% discvar))]) >= 1){
    qv_psd = xname[which(!(xname %in% discvar))] %>%
      purrr::map(\(xvar) psd_pseudop(data[,yname,drop = TRUE],
                                     data[,xvar,drop = TRUE],wt)) %>%
      purrr::list_rbind() %>%
      dplyr::mutate(variable = xname[which(!(xname %in% discvar))]) %>%
      dplyr::select(variable,dplyr::everything())
    res = dplyr::bind_rows(qv_spade,qv_psd)%>%
      dplyr::arrange(dplyr::desc(`Q-statistic`))} else {
        res = qv_spade %>%
          dplyr::arrange(dplyr::desc(`Q-statistic`))
      }
  res = list("factor" = res)
  class(res) = "spade_result"
  return(res)
}
