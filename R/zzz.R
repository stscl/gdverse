.onLoad = function(...) {
  loadNamespace("Rcpp")
  loadNamespace("sf")
  loadNamespace("tibble")
}

.onAttach = function(...){
  packageStartupMessage("This is gdverse 1.3-2.
                        \nUse our itmsa package for information entropy-driven spatial stratified heterogeneity measurement.
                        ")
}
