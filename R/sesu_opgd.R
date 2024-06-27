#' @title comparison of size effects of spatial units based on OPGD
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for comparison of size effects of spatial units in spatial heterogeneity analysis based on
#' optimal parameters geographical detector(OPGD) model.
#' @references
#' Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based geographical detector
#' model enhances geographic characteristics of explanatory variables for spatial heterogeneity
#' analysis: Cases with different types of spatial data, GIScience & Remote Sensing, 57(5), 593-610.
#' doi: 10.1080/15481603.2020.1760434.
#' @details
#' Firstly, the `OPGD` model is executed for each data in the datalist (all `significant`
#' Q statistic of each data are averaged to represent the spatial connection strength under
#' this spatial unit), and then the `loess_optscale` function is used to select the optimal
#' spatial analysis scale.
#'
#' @param formula A formula of comparison of size effects of spatial units.
#' @param datalist A list of \code{data.frame} or \code{tibble}.
#' @param su A vector of sizes of spatial units.
#' @param discvar Name of continuous variable columns that need to be discretized.Noted that
#' when `formula` has `discvar`, `data` must have these columns.
#' @param discnum (optional) A vector of number of classes for discretization. Default is `3:22`.
#' @param discmethod (optional) A vector of methods for discretization,default is used
#' `c("sd","equal","pretty","quantile","fisher","headtails","maximum","box")`in `gdverse`.
#' @param cores (optional) A positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster object.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#' @param ... (optional) Other arguments passed to `gd_bestunidisc()`.
#'
#' @return A nested tibble with `spatial_units`, `sesu_result` and `data`.
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' fvcpath = "https://github.com/SpatLyu/rdevdata/raw/main/FVC.tif"
#' fvc = terra::rast(paste0("/vsicurl/",fvcpath))
#' fvc1000 = fvc %>%
#'   terra::as.data.frame(na.rm = T) %>%
#'   as_tibble()
#' fvc5000 = fvc %>%
#'   terra::aggregate(fact = 5) %>%
#'   terra::as.data.frame(na.rm = T) %>%
#'   as_tibble()
#' sesu_opgd(fvc ~ .,
#'           datalist = list(fvc1000,fvc5000),
#'           su = c(1000,5000),
#'           discvar = names(select(fvc5000,-c(fvc,lulc))),
#'           cores = 6)
#' }
sesu_opgd = \(formula,datalist,su,discvar,discnum = NULL,discmethod = NULL,
              cores = 1, increase_rate = 0.05, alpha = 0.95, ...){
  res_sesu = purrr::map2(datalist, su,
                         \(.tbf, .spsu) opgd(formula,.tbf,discvar,discnum,
                                             discmethod,cores,type = "factor",
                                             alpha = alpha, ...) %>%
                                purrr::pluck('factor') %>%
                                dplyr::mutate(su = .spsu))
  sesu = tibble::tibble(spatial_units = su,
                        sesu_result = res_sesu)
  optsu = res_sesu %>%
    purrr::list_rbind() %>%
    dplyr::filter(`P-value` <= (1 - alpha)) %>%
    dplyr::group_by(su) %>%
    dplyr::summarise(qv = mean(`Q-statistic`,na.rm = T))
  optsu = loess_optscale(optsu$qv,optsu$su,increase_rate)
  res = list('sesu' = sesu,'optsu' = optsu)
  class(res) = 'sesu_opgd'
  return(res)
}

#' @title print opgd sedu
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for iopgd sedu in `sesu_opgd()`.
#'
#' @param x Return by `sesu_opgd()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @export
#'
print.sesu_opgd = \(x,...){
  g = purrr::list_rbind(x$sesu$sesu_result)
  spunits = x$sesu$spatial_units
  cat("\n    Size Effects Of Spatial Unit    \n",
      "\n              OPGD Model            \n",
      "\n  ***  Optimal Spatial Unit : ", x$optsu, "  ***\n")
  for (i in spunits){
    cat(sprintf("\n Spatial Unit: %s ",i))
    print(kableExtra::kable(dplyr::filter(g,su==i) %>% dplyr::select(-su),
                            format = "markdown",digits = 16, align = 'c', ...))
  }
}

#' @title plot opgd sedu
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for iopgd sedu in `sesu_opgd()`.
#'
#' @param x Return by `sesu_opgd()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer.
#' @export
#'
plot.sesu_opgd = \(x,...){
  g = purrr::list_rbind(x$sesu$sesu_result) %>%
    dplyr::rename(qv = `Q-statistic`, pv = `P-value`)
  spunits = x$sesu$spatial_units
  namev = g[which(g$su== spunits[1]),"variable",drop = TRUE]
  shapev = seq_along(namev)
  names(shapev) = namev
  qvrange = range(g$qv)
  qv95 = g %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(qv95 = stats::quantile(qv,probs = 0.9)) %>%
    dplyr::pull(qv95) %>%
    round(3)
  qv95 = seq(range(qv95)[1],range(qv95)[2],length.out = length(qv95))
  colv = c("#0000ff","#ff0000","#0ecf0e","#000000","#3effff","#A6CEE3","#FFFF33",
           "#B2DF8A","#33A02C","#FB9A99","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A",
           "#FFFF99","#B15928","#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E",
           "#E6AB02","#A6761D","#1F78B4","#666666")[seq_along(qv95)]
  names(colv) = namev
  if (length(shapev) <= 25) {
    fig_g = ggplot2::ggplot(g, ggplot2::aes(x = su, y = qv)) +
      ggplot2::geom_point(ggplot2::aes(shape = variable,
                                       color = variable),
                          size = 2.75) +
      ggplot2::geom_line(ggplot2::aes(color = variable),
                         linetype = 3,show.legend = FALSE) +
      ggplot2::scale_x_continuous(name = 'Size of spatial uint',
                         breaks = x$sesu$spatial_units,
                         limits = x$sesu$spatial_units + c(-100,100),
                         expand = c(0,0)) +
      ggplot2::scale_y_continuous(name = "Q statistic", expand = c(0,0),
                         limits = c(ifelse(qvrange[1]>=0.05,0,qvrange[1]-0.05),
                                    ifelse(qvrange[2]<=0.95,qvrange[2]+0.05,qvrange[2])),
                         breaks = round(seq(qvrange[1],qvrange[2],length.out = length(qv95)),3),
                         sec.axis = ggplot2::sec_axis(name = "The 90% quantile of Q statistic",
                                                      labels = qv95, breaks = qv95,
                                                      transform = ~ .)) +
      ggplot2::scale_shape_manual(name = "", values = shapev) +
      ggplot2::scale_color_manual(name = "", values = colv) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), ...)
  } else {
    fig_g = ggplot2::ggplot(g, ggplot2::aes(x = su, y = qv)) +
      ggplot2::geom_point(ggplot2::aes(color = variable),
                          size = 2.75) +
      ggplot2::geom_line(ggplot2::aes(color = variable),
                         linetype = 3,show.legend = FALSE) +
      ggplot2::scale_x_continuous(name = 'Size of spatial uint',
                         breaks = x$sesu$spatial_units,
                         limits = x$sesu$spatial_units + c(-100,100),
                         expand = c(0,0)) +
      ggplot2::scale_y_continuous(name = "Q statistic", expand = c(0,0),
                                  limits = c(ifelse(qvrange[1]>=0.05,0,qvrange[1]-0.05),
                                             ifelse(qvrange[2]<=0.95,qvrange[2]+0.05,qvrange[2])),
                                  breaks = round(seq(qvrange[1],qvrange[2],length.out = length(qv95)),3),
                                  sec.axis = ggplot2::sec_axis(name = "The 90% quantile of Q statistic",
                                                               labels = qv95, breaks = qv95,
                                                               transform = ~ .)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), ...)
  }
 return(fig_g)
}
