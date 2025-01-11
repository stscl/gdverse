.onLoad = function(...) {
  loadNamespace("Rcpp")
  loadNamespace("sf")
  loadNamespace("tibble")
}

.onAttach = function(...){
  packageStartupMessage("gdverse ", utils::packageVersion("gdverse"))
  packageStartupMessage("See itmsa package for information entropy-driven spatial stratified heterogeneity measurement.")
}
