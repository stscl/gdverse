.onLoad = function(...) {
  loadNamespace("Rcpp")
  loadNamespace("tibble")
  loadNamespace("magrittr")
}

.onAttach = function(...){
  packageStartupMessage("This is gdverse 1.3-2.
                        \nUse our itmsa package for information entropy-driven spatial stratified heterogeneity measurement.
                        ")
}
