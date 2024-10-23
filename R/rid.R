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
#' @param overlay (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `intersection`.
#' @param minsize (optional) The min size of each discretization group. Default all use `1`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#'
#' @return A list.
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
rid = \(formula, data, discvar = NULL,
        discnum = 10, minsize = 1, cores = 1){
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
  discdf = dplyr::select(dti,dplyr::all_of(c(yname,discvar)))
  if (length(discnum)==0) {discnum = rep(discnum,length(discvar))}
  g = robust_disc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                  discdf, discnum, minsize, cores = cores)
  discedvar = colnames(dti[,-which(colnames(dti) %in% discvar)])
  newdti = dti %>%
    dplyr::select(dplyr::all_of(discedvar)) %>%
    dplyr::bind_cols(g)
  res = utils::combn(names(xname), 2, simplify = FALSE) %>%
    purrr::map_dfr(\(i) interaction_detector(dti[,yname,drop = TRUE],
                                             newdti[,i[1],drop = TRUE],
                                             newdti[,i[2],drop = TRUE]) %>%
                     tibble::as_tibble() %>%
                     dplyr::mutate(variable1 = i[1],
                                   variable2 = i[2]) %>%
                     dplyr::select(variable1,variable2,Interaction,
                                   dplyr::everything()))
  res = list("interaction" = res)
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
  print(knitr::kable(x$interaction, format = "markdown",
                     digits = 12, align = 'c', ...))
}
