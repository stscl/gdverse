#' @title optimal parameters-based geographical detector(OPGD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for optimal parameters-based geographical detector(OPGD) model.
#' @references
#' Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based geographical detector
#' model enhances geographic characteristics of explanatory variables for spatial heterogeneity
#' analysis: Cases with different types of spatial data, GIScience & Remote Sensing, 57(5), 593-610.
#' doi: 10.1080/15481603.2020.1760434.
#'
#' @param formula A formula of OPGD model.
#' @param data A data.frame or tibble of observation data.
#' @param discvar Name of continuous variable columns that need to be discretized.Noted that
#' when `formula` has `discvar`, `data` must have these columns.
#' @param discnum (optional) A vector of number of classes for discretization. Default is `3:22`.
#' @param discmethod (optional) A vector of methods for discretization,default is using
#' `c("sd","equal","pretty","quantile","fisher","headtails","maximum","box")`in `gdverse`.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster object.
#' @param type (optional) The type of geographical detector,which must be `factor`(default),
#' `interaction`, `risk`, `ecological`. You can run one or more types at one time.
#' @param alpha (optional) Specifies the size of confidence level.Default is `0.95`.
#' @param ... (optional) Other arguments passed to `gd_bestunidisc()`.A useful parameter is `seed`,
#'  which is used to set the random number seed.
#'
#' @return A list of the OPGD model result.
#' \describe{
#' \item{\code{factor}}{the result of factor detector}
#' \item{\code{interaction}}{the result of interaction detector}
#' \item{\code{risk}}{the result of risk detector}
#' \item{\code{ecological}}{the result of ecological detector}
#' }
#' @export
#'
#' @examples
#' data('sim')
#' opgd(y~ xa + xb + xc, data = sim,
#'      discvar = paste0('x',letters[1:3]),
#'      discnum = 3:6)
#'
opgd = \(formula,data,discvar,discnum = NULL,discmethod = NULL,
         cores = 1, type = "factor", alpha = 0.95, ...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  yname = formula.vars[1]
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
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
                    type = type[i],alpha = alpha)[[1]]
    }
    names(res) = type
  }
  class(res) = "opgd_result"
  return(res)
}

#' @title print OPGD result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for OPGD model from `opgd()`.
#'
#' @param x Return by `opgd()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print opgd_result
#' @export
#'
print.opgd_result = \(x, ...) {
  cat("                OPGD Model                  \n")
  nx = names(x)
  for (i in seq_along(x)){
    res = x[i]
    class(res) = paste0(nx[i],"_detector")
    print(res)
    cat("\n")
  }
}

#' @title plot OPGD result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for OPGD model result in `opgd()`.
#'
#' @param x Return by `opgd()`.
#' @param ... (optional) Other arguments passed to `patchwork::wrap_plots()`.
#'
#' @return A ggplot2 layer
#' @method plot opgd_result
#' @export
#'
plot.opgd_result = \(x, ...) {
  if (length(x) == 1){
    res = x[1]
    nx = names(x)
    class(res) = paste0(nx[1],"_detector")
    fig_p = plot(res)
  } else {
    fig_p = vector("list",length(x))
    nx = names(x)
    for (i in seq_along(x)){
      res = x[i]
      class(res) = paste0(nx[i],"_detector")
      fig_p[[i]] = plot(res)
    }
    fig_p = patchwork::wrap_plots(fig_p, ncol = 2, ...)
  }
  return(fig_p)
}
