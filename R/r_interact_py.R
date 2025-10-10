#' @title Read and evaluate a python script
#' @noRd
utils_source_python = \(oauth_func_path) {
  module_name = gsub("\\.py$", "", basename(oauth_func_path))
  module_path = dirname(oauth_func_path)
  reticulate::import_from_path(module_name, path = module_path, convert = TRUE)
}
