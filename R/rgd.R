#' @title robust geographical detector(RGD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for robust geographical detector(RGD) model.
#' @references
#' Zhang, Z., Song, Y.*, & Wu, P., 2022. Robust geographical detector. International Journal of Applied Earth Observation and Geoinformation. 109, 102782.
#' DOI: 10.1016/j.jag.2022.102782.
#'
#' @param formula A formula of RGD model.
#' @param data A data.frame or tibble of observation data.
#' @param discvar Name of continuous variable columns that need to be discretized.Noted that
#' when `formula` has `discvar`, `data` must have these columns.
#' @param discnum A numeric vector of discretized classes of columns that need to be discretized.
#' Default all `discvar` use `10`.
#' @param minsize (optional) The min size of each discretization group.Default all use `1`.
#' @param cores (optional) Positive integer(default is 1). If cores > 1, use `python`'s `joblib` package to
#' parallel computation.
#' @param type (optional) The type of geographical detector,which must be `factor`(default),
#' `interaction`, `risk`, `ecological`.You can run one or more types at one time.
#' @param alpha (optional) Specifies the size of confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `robust_disc()`.
#'
#' @return A list of the RGD model result.
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(tidyverse)
#' fvcpath = "https://github.com/SpatLyu/rdevdata/raw/main/FVC.tif"
#' fvc = terra::rast(paste0("/vsicurl/",fvcpath))
#' fvc = terra::aggregate(fvc,fact = 5)
#' fvc = as_tibble(terra::as.data.frame(fvc,na.rm = T))
#' rgd(fvc ~ ., data = fvc, discnum = 10,
#'     discvar = names(select(fvc,-c(fvc,lulc))),
#'     cores = 6, type =c('factor','interaction'))
#' }
rgd = \(formula,data,discvar,discnum = NULL,minsize = NULL,
        cores = 1, type = "factor", alpha = 0.95, ...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  yname = formula.vars[1]
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
  discdf =  dplyr::select(data,dplyr::all_of(c(yname,discvar)))
  if (is.null(discnum)) {discnum = rep(10,length(discvar))}
  g = robust_disc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                  discdf, discnum, minsize, cores = cores)
  discedvar = colnames(data[,-which(colnames(data) %in% discvar)])
  newdata = data %>%
    dplyr::select(dplyr::all_of(discedvar)) %>%
    dplyr::bind_cols(g)
  if (length(type) == 1){
    res = gd(paste0(yname,' ~ .'),data = newdata,
             type = type,alpha = alpha)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = gd(paste0(yname,' ~ .'),data = newdata,
                    type = type[i],alpha = alpha)
    }
  }

  return(res)
}
