lesh = \(formula,data,interact = 2, cores = 1,...){
  spd = spd_lesh(formula,data,cores,...)
  spdv = spd[,2,drop = TRUE]
  names(spdv) = spd[,1,drop = TRUE]
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  yname = formula.vars[1]
  if (formula.vars[2] != "."){
    dti = dplyr::select(data,dplyr::all_of(formula.vars))
  } else {
    dti = data
  }
  xname = colnames(dti)[-which(colnames(dti) == yname)]
  xinteract = utils::combn(xname,interact,simplify = FALSE)

  calcul_pd = \(xvar){
    discdf = rpart_disc(paste0(yname,'~',paste(xvar,collapse = '+')),data = dti,...)
    qv = factor_detector(dti[,yname,drop = TRUE],discdf)[[1]]
    names(qv) = 'pd'
    return(qv)
  }
  get_v_by_wt = \(namelist,wtvec,vassign){
    newwtvec = wtvec[unlist(namelist)]
    res = weight_assign(vassign,newwtvec)
    return(res)
  }

  out_g = purrr::map_dfr(xinteract,calcul_pd)
  out_g$interact = xinteract
  pdinteract = purrr::map2(out_g$interact,
                           out_g$pd,
                           \(x,y) get_v_by_wt(x,spdv,y))
  return(pdinteract)
}
