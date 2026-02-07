.onLoad = function(...) {
  loadNamespace("sf")
  loadNamespace("tibble")
  reticulate::py_require(c('numpy','pandas','ruptures','joblib'))
}

.onAttach = function(...){
  packageStartupMessage("This is gdverse ", utils::packageVersion("gdverse"), ".")
  packageStartupMessage("\nTo cite gdverse in publications, please use:
                         \nLv, W., Lei, Y., Liu, F., Yan, J., Song, Y. and Zhao, W. (2025), gdverse: An R Package for Spatial Stratified Heterogeneity Family. Transactions in GIS, 29(2). https://doi.org/10.1111/tgis.70032")
}
