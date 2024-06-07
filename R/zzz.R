rpt = NULL

.onLoad = function(libname, pkgname) {
  rpt <<- reticulate::import("ruptures", delay_load = TRUE)
}
