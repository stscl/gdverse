#' @title robust interaction detector(RID) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for robust interaction detector(RID) model.
#' @references
#' Zhang, Z., Song, Y., Karunaratne, L., & Wu, P. (2024). Robust interaction detector:
#' A case of road life expectancy analysis. Spatial Statistics, 59(100814), 100814.
#' https://doi.org/10.1016/j.spasta.2024.100814
#' @note
#'
#' The RID model requires at least \eqn{2^n-1} calculations when has \eqn{n} explanatory variables.
#' When there are more than 10 explanatory variables, carefully consider the computational burden of this model.
#' When there are a large number of explanatory variables, the data dimensionality reduction method can be used
#' to ensure the trade-off between analysis results and calculation speed.
#'
#' Please set up python dependence and configure `GDVERSE_PYTHON` environment variable if you want to run `rid()`.
#' See `vignette('rgdrid',package = 'gdverse')` for more details.
#'
#' @param formula A formula of RID model.
#' @param data A data.frame, tibble or sf object of observation data.
#' @param discvar Name of continuous variable columns that need to be discretized. Noted that
#' when `formula` has `discvar`, `data` must have these columns. By default, all independent
#' variables are used as `discvar`.
#' @param discnum A numeric vector for the number of discretized classes of columns that need
#' to be discretized. Default all `discvar` use `10`.
#' @param overlaymethod (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `intersection`.
#' @param minsize (optional) The min size of each discretization group. Default all use `1`.
#' @param cores (optional) Positive integer(default is 1). If cores > 1, use parallel computation.
#'
#' @return A list of the RID model result.
#' \describe{
#' \item{\code{interaction}}{the result of RID model}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code needs to configure the Python environment to run:
#' data('sim')
#' g = rid(y ~ .,
#'         data =  dplyr::select(sim,-dplyr::any_of(c('lo','la'))),
#'         discnum = 4, cores = 6)
#' g
#' }
rid = \(formula, data, discvar = NULL, discnum = 10,
        overlaymethod = 'intersection', minsize = 1, cores = 1){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  yname = formula.vars[1]
  if (inherits(data,'sf')) {data = sf::st_drop_geometry(data)}
  if (formula.vars[2] != "."){
    dti = dplyr::select(data,dplyr::all_of(formula.vars))
  } else {
    dti = data
  }
  xname = colnames(dti)[-which(colnames(dti) == yname)]
  if (is.null(discvar)) {
    discvar = xname
  }
  discdf =  dplyr::select(dti,dplyr::all_of(c(yname,discvar)))
  if (length(discnum)==0) {discnum = rep(discnum,length(discvar))}
  g = robust_disc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                  discdf, discnum, minsize, cores = cores)
  discedvar = colnames(dti[,-which(colnames(dti) %in% discvar)])
  newdti = dti %>%
    dplyr::select(dplyr::all_of(discedvar)) %>%
    dplyr::bind_cols(g)
  xs = generate_subsets(xname,empty = FALSE, self = TRUE)
  spfom = overlaymethod

  interact_pd = \(xvar){
    if (spfom == 'intersection'){
      reszone = newdti %>%
        dplyr::select(dplyr::all_of(xvar)) %>%
        purrr::reduce(paste, sep = '_')
    } else {
      reszone = st_fuzzyoverlay(paste(yname,'~',paste0(xvar,collapse = '+')),
                                newdti,spfom)
    }
    qv = factor_detector(newdti[,yname,drop = TRUE],reszone)[[1]]
    names(qv) = 'qv_rid'
    return(qv)
  }

  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('factor_detector','st_fuzzyoverlay'))
    out_g = parallel::parLapply(cores,xs, interact_pd)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xs, interact_pd)
  }
  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  xsname = purrr::map_chr(xs,\(.x) paste(.x,collapse = IntersectionSymbol))
  out_g = tibble::tibble(varibale = xsname) %>%
    dplyr::bind_cols(out_g) %>%
    dplyr::arrange(dplyr::desc(qv_rid))
  res = list("interaction" = out_g)
  class(res) = "rid_result"
  return(res)
}

#' @title print RID result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for RID model from `rid()`.
#'
#' @param x Return by `rid()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @export
print.rid_result = \(x, ...) {
  cat("***          Robust Interaction Detector       ")
  print(knitr::kable(dplyr::rename(x$interaction, PD = qv_rid),
                     format = "markdown",digits = 12,align = 'c',...))
}
