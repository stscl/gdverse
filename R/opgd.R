#' @title optimal parameters-based geographical detector(OPGD) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @references
#' Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based geographical detector
#' model enhances geographic characteristics of explanatory variables for spatial heterogeneity
#' analysis: Cases with different types of spatial data, GIScience & Remote Sensing, 57(5), 593-610.
#' doi: 10.1080/15481603.2020.1760434.
#'
#' @param formula A formula of OPGD model.
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param discvar Name of continuous variable columns that need to be discretized. Noted that
#' when `formula` has `discvar`, `data` must have these columns. By default, all independent
#' variables are used as `discvar`.
#' @param discnum (optional) A vector of number of classes for discretization. Default is `3:8`.
#' @param discmethod (optional) A vector of methods for discretization, default is using
#' `c("sd","equal","geometric","quantile","natural")` by invoking `sdsfun`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#' @param type (optional) The type of geographical detector, which must be `factor`(default),
#' `interaction`, `risk`, `ecological`. You can run one or more types at one time.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#' @param ... (optional) Other arguments passed to `gd_optunidisc()`. A useful parameter is `seed`,
#' which is used to set the random number seed.
#'
#' @return A list.
#' \describe{
#' \item{\code{opt_param}}{optimal discretization parameter}
#' \item{\code{factor}}{the result of factor detector}
#' \item{\code{interaction}}{the result of interaction detector}
#' \item{\code{risk}}{the result of risk detector}
#' \item{\code{ecological}}{the result of ecological detector}
#' }
#' @export
#'
#' @examples
#' data('sim')
#' opgd(y ~ xa + xb + xc, data = sim,
#'      discvar = paste0('x',letters[1:3]),
#'      discnum = 3:6)
#'
opgd = \(formula, data, discvar = NULL, discnum = 3:8,
         discmethod = c("sd","equal","geometric","quantile","natural"),
         cores = 1, type = "factor", alpha = 0.95, ...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (inherits(data,'sf')) {data = sf::st_drop_geometry(data)}
  data = tibble::as_tibble(data)
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
  yname = formula.vars[1]
  if (is.null(discvar)) {
    discvar = colnames(data)[-which(colnames(data) == yname)]
  }
  discdf = dplyr::select(data,dplyr::all_of(c(yname,discvar)))
  g = gd_optunidisc(paste0(yname,'~',paste0(discvar,collapse = '+')),
                    data = discdf, discnum = discnum,
                    discmethod = discmethod, cores = cores, ...)
  opt_param = tibble::as_tibble(g[1:3])
  names(opt_param) = c("varibale","discnum","method")
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
  res = append(list("opt_param" = opt_param),res)
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
#' @export
#'
print.opgd_result = \(x, ...) {
  x = x[-1] # rm opt_param to print
  cat("***   Optimal Parameters-based Geographical Detector     \n")
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
#' @param ... (optional) Other arguments.
#'
#' @return A ggplot2 layer
#' @export
#'
plot.opgd_result = \(x, ...) {
  x = x[-1] # rm opt_param to plot
  if (length(x) == 1){
    res = x[1]
    nx = names(x)
    class(res) = paste0(nx[1],"_detector")
    fig_p = plot(res,...)
  } else {
    fig_p = vector("list",length(x))
    nx = names(x)
    for (i in seq_along(x)){
      res = x[i]
      class(res) = paste0(nx[i],"_detector")
      fig_p[[i]] = plot(res,...)
    }
    fig_p = patchwork::wrap_plots(fig_p, ncol = 2)
  }
  return(fig_p)
}
