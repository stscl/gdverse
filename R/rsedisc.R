rse_disc = \(formula,data,discnum,minsize = 1,alpha = 0.5,cores = 1,...) {
  geom = sf::st_geometry(data)
  data = sf::st_drop_geometry(data)
  formulavars = sdsfun::formula_varname(formula,data)
  response = data[, formulavars[[1]], drop = TRUE]
  explanatory = data[, formulavars[[2]]]
  y = formulavars[[1]]
  xvars = formulavars[[2]]
  if (length(minsize)==1) {minsize = rep(1,length(xvars))}
  if (length(discnum)==1) {discnum = rep(discnum,length(xvars))}
  gs = as.integer(discnum)
  minsizes = as.integer(minsize)
  cores = as.integer(cores)
  gdf = dplyr::select(data,dplyr::all_of(c(y,xvars)))
  os = matrix(0,nrow = nrow(explanatory),ncol = ncol(explanatory))
  for (indices in seq_along(explanatory)) {
    os[,indices] = sdsfun::hclustgeo_disc(
      data = sf::st_set_geometry(
        tibble::tibble(x = explanatory[,indices,drop = TRUE]),geom
      ), alpha = alpha, cut = FALSE, ...
    )
  }

  script_path = system.file("python", "rse_disc.py", package = "gdverse")
  rse_disc_lib = utils_source_python(script_path)
  out_g = rse_disc_lib$rse_disc(gdf,y,xvars,gs,os,minsizes,cores) %>%
    #utils_py_to_r() %>%
    tibble::as_tibble()
  return(out_g)
}
