#' @title optimal parameters geographic detector(OPGD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for optimal parameters geographic detector(OPGD) model.
#'
#' @param formula A formula of OPGD model.
#' @param data A data.frame or tibble of observation data.
#' @param discvar Name of continuous variable columns that need to be discretized.Noted that
#' when `formula` has `discvar`, `data` must have these columns.
#' @param discnum (optional) A vector of number of classes for discretization. Default is `2:15`.
#' @param discmethod (optional) A vector of methods for discretization,default is used
#' `c("sd","equal","pretty","quantile","fisher","headtails","maximum","box")`in `spEcula`.
#' @param cores positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#' @param type (optional) The type of geographical detector,which must be `factor`(default),
#' `interaction`, `risk`, `ecological`.You can run one or more at a time.
#' @param alpha (optional) Specifies the size of confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `gd_bestunidisc()`.
#'
#' @return A list of the opgd model result.
#' @importFrom stats as.formula
#' @importFrom magrittr `%>%`
#' @importFrom purrr map
#' @importFrom dplyr select all_of bind_cols
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(tidyverse)
#' fvcpath = system.file("extdata", "FVC.zip",package = 'spEcula')
#' fvc = terra::rast(paste0("/vsizip/",fvcpath))
#' fvc = as_tibble(terra::as.data.frame(fvc,na.rm = T))
#' opgd(fvc ~ ., data = fvc,
#'      discvar = names(select(fvc,-c(fvc,lulc))),
#'      cores = 6, type =c(`factor`,`interaction`))
#' }
opgd = \(formula,data,discvar,discnum = NULL,discmethod = NULL,
         cores = 1,type = 'factor',alpha = 0.95,...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  yname = formula.vars[1]
  discdf =  dplyr::select(data,dplyr::all_of(c(yname,discvar)))
  g = gd_bestunidisc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                     data = discdf,discnum = discnum,
                     discmethod = discmethod,cores = cores,...)
  discedvar = colnames(data[,-which(colnames(data) %in% discvar)])
  newdata = data %>%
    dplyr::select(dplyr::all_of(discedvar)) %>%
    dplyr::bind_cols(g$disv)
  if (length(type) == 1){
    res = gd(paste0(yname,' ~ .'),data = newdata,type = type,alpha = alpha)
  } else {
    res = vector("list", length(type))
    for (i in seq_along(type)){
      res[[i]] = gd(paste0(yname,' ~ .'),data = newdata,
                    type = i,alpha = alpha)
    }
  }

  return(res)
}
