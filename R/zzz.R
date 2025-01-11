.onLoad = function(...) {
  loadNamespace("Rcpp")
  loadNamespace("sf")
  loadNamespace("tibble")
}

.onAttach = function(...){
  packageStartupMessage("gdverse ", utils::packageVersion("gdverse"))
  packageStartupMessage("See itmsa package for entropy-based spatial association measures.")
}
