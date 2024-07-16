srs_factor_detector = \(y,x,wt,seed = 123456789){
  y = all2int(y)
  x = all2int(x)
  obs = cbind(x,y)
  res = SRS_PD(obs,wt)

}
