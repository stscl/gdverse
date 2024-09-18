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
