esp = \(formula, data, wt = NULL, discnum = 3:22,
         discmethod = "quantile", overlaymethod = 'and',
         cores = 1, seed = 123456789, alpha = 0.95, ...){
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
}
