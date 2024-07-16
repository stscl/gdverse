srs_factor_detector = \(y,x,wt,alpha = 0.95,
                        size_frac = 0.05,
                        seed = 123456789){
  y = all2int(y)
  x = all2int(x)
  obs = cbind(x,y)
  res = SRS_PD(obs,wt)

  set.seed(seed)
  size = ceiling(length(y) * size_frac)
  sindice = sample.int(seq_along(y), size)
  obsp = obs[sindice,]
  wtp = wt[sindice,sindice]
  pdp = SRSFactor_P(obsp,wtp)
  tt = tryCatch({
    stats::t.test(pdp,0,conf.level = alpha)
  }, error = function(e){
    list("statistic" = 0,
         "parameter" = df0,
         "p.value" = 1)
  })
  pv = tt$p.value
  names(pv) = 'P-value'
  append(res,pv)
  return(res)
}
