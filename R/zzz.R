.onLoad = function(...) {
  loadNamespace("sf")
  loadNamespace("tibble")
  reticulate::py_require(c('numpy','pandas','ruptures','joblib'))
}

.onAttach = function(...){
  packageStartupMessage("gdverse ", utils::packageVersion("gdverse"))
  packageStartupMessage("See itmsa package for entropy-based spatial association measures.")
}
