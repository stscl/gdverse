#' @title spatial association detector (SPADE) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for spatial association detector (SPADE) model.
#'
#' @references
#' Xuezhi Cang & Wei Luo (2018) Spatial association detector (SPADE),International
#' Journal of Geographical Information Science, 32:10, 2055-2075, DOI:  10.1080/13658816.2018.1476693
#'
#' @param formula A formula of spatial association detector (SPADE) model.
#' @param data A data.frame or tibble of observation data.
#' @param wt (optional) The spatial weight matrix.When `wt` is not provided, must provide `locations`.
#' And `gdverse` will use `locations` columns to construct spatial weight use `inverse_distance_weight()`.
#' @param locations (optional) The geospatial locations coordinate columns name which in `data`.
#' Useful and must provided when `wt` is not provided.
#' @param discvar Name of continuous variable columns that need to be discretized.Noted that
#' when `formula` has `discvar`, `data` must have these columns.
#' @param discnum (optional) Number of multilevel discretization.Default will use `3:15`.
#' @param discmethod (optional) The discretization methods. Default all use `quantile`.
#' When `discmethod` is `robust` use `robust_disc()`, others use `st_unidisc()`
#' @param cores (optional) A positive integer(default is 6). If cores > 1, use parallel computation.
#' @param seed (optional) Random seed number, default is `123456789`.
#' @param permutations (optional) The number of permutations for the PSD computation. Default is `99`.
#' @param ... (optional) Other arguments passed to `st_unidisc()` or `robust_disc()`.
#'
#' @return A list of the SPADE model result.
#' @importFrom purrr map list_rbind
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' usfi = read_sf(system.file('extdata/USFI_Xian.gpkg',package = 'gdverse')) |>
#'   dplyr::select(dplyr::all_of(c("NDVI","BH","SUHI")))
#' coord = usfi |>
#'   st_centroid() |>
#'   st_coordinates()
#' wt = inverse_distance_weight(coord[,1],coord[,2])
#' usfi = usfi |>
#'   dplyr::bind_cols(coord) |>
#'   st_drop_geometry()
#' spade('SUHI~.', data = usfi,locations = c('X','Y'),
#'       discvar = c('BH','NDVI'), cores = 6)
#' spade('SUHI~.', data = usfi, wt = wt,
#'       discvar = c('BH','NDVI'),locations = c('X','Y'),
#'       discmethod = c('sd','equal'),cores = 6)
#' }
spade = \(formula,data,wt = NULL,locations = NULL,discvar = NULL,discnum = NULL,
          discmethod = NULL,cores = 6,seed = 123456789,permutations = 99,...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(c(formula.vars,locations)))
  }
  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) %in% c(formula.vars[1],locations))]
  xname_spade = xname[which(xname %in% discvar)]
  if (is.null(discmethod)) {discmethod = rep('quantile',length(xname_spade))}
  qv_spade = vector("list",length = length(xname_spade))
  for (i in seq_along(xname_spade)){
    qv_spade[[i]] = psmd_pseudop(
      formula = paste(yname,'~',xname_spade[i]),
      data = dplyr::select(data,
                           dplyr::all_of(c(yname,xname_spade[i],locations))),
      wt = wt, locations = locations, discnum = discnum, discmethod = discmethod[i],
      cores = cores, seed = seed, permutations = permutations, ...)
  }
  qv_spade = purrr::list_rbind(qv_spade) %>%
    dplyr::mutate(variable = xname_spade) %>%
    dplyr::select(variable,dplyr::everything())
  if (length(xname[which(!(xname %in% discvar))]) >= 1){
    qv_psd = xname[which(!(xname %in% discvar))] %>%
      purrr::map(\(xvar) psd_pseudop(data[,yname,drop = TRUE],
                                     data[,xvar,drop = TRUE],wt)) %>%
      purrr::list_rbind() %>%
      dplyr::mutate(variable = xname[which(!(xname %in% discvar))]) %>%
      dplyr::select(variable,dplyr::everything())
    res = dplyr::bind_rows(qv_spade,qv_psd)%>%
          dplyr::arrange(dplyr::desc(`Q-statistic`))} else {
    res = qv_spade %>%
      dplyr::arrange(dplyr::desc(`Q-statistic`))
  }
  res = list("factor" = res)
  class(res) = "factor_detector"
  return(res)
}
