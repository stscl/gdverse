#' @title comparison of size effects of spatial units based on GOZH
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for comparison of size effects of spatial units in spatial heterogeneity analysis based on
#' geographically optimal zones-based heterogeneity(GOZH) model.
#' @references
#' Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based geographical detector
#' model enhances geographic characteristics of explanatory variables for spatial heterogeneity
#' analysis: Cases with different types of spatial data, GIScience & Remote Sensing, 57(5), 593-610.
#' doi: 10.1080/15481603.2020.1760434.
#'
#' Luo, P., Song, Y., Huang, X., Ma, H., Liu, J., Yao, Y., & Meng, L. (2022). Identifying determinants of
#' spatio-temporal disparities in soil moisture of the Northern Hemisphere using a geographically optimal
#' zones-based heterogeneity model. ISPRS Journal of Photogrammetry and Remote Sensing:
#' Official Publication of the International Society for Photogrammetry and Remote Sensing (ISPRS), 185, 111–128.
#' https://doi.org/10.1016/j.isprsjprs.2022.01.009
#'
#' @details
#' When `strategy` is `1`, use the same process as `sesu_opgd()`.If not, all explanatory
#' variables are used to generate a unique Q statistic corresponding to the data in the
#' datalist based on `rpart_disc()` and `gd()`, and then `loess_optscale()`is used to
#' determine the optimal analysis scale.
#'
#' @param formula A formula of comparison of size effects of spatial units.
#' @param datalist A list of `data.frame` or `tibble`.
#' @param su A vector of sizes of spatial units.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#' @param strategy (optional) Calculation strategies of Q statistics at different scales. Default
#' is `2L`, see `details` for more contents.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#' @param ... (optional) Other arguments passed to `rpart_disc()`.
#'
#' @return A list.
#' \describe{
#' \item{\code{sesu}}{a tibble representing size effects of spatial units}
#' \item{\code{optsu}}{optimal spatial unit}
#' \item{\code{strategy}}{the optimal analytical scale selection strategy}
#' \item{\code{increase_rate}}{the critical increase rate of q value}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code takes a long time to run:
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
#' sesu_gozh(fvc ~ .,
#'           datalist = list(fvc1000,fvc5000),
#'           su = c(1000,5000),
#'           cores = 6)
#' }
sesu_gozh = \(formula,datalist,su,cores = 1,strategy = 2L,
              increase_rate = 0.05, alpha = 0.95, ...){
  if (strategy == 1) {
    res_sesu = purrr::map2(datalist, su,
                           \(.tbf, .spsu) gdverse::gozh(formula,.tbf,cores,
                                               type = "factor",
                                               alpha = alpha, ...) %>%
                             purrr::pluck('factor') %>%
                             dplyr::mutate(su = .spsu))
    sesu = tibble::tibble(spatial_units = su,
                          sesu_result = res_sesu)
    optsu = res_sesu %>%
      purrr::list_rbind() %>%
      dplyr::filter(`P-value` <= (1 - alpha) | is.na(`P-value`)) %>%
      dplyr::group_by(su) %>%
      dplyr::summarise(qv = mean(`Q-statistic`,na.rm = T))
    optsu = gdverse::loess_optscale(optsu$qv,optsu$su,increase_rate)
  } else {
    gozh_total = \(formula,data,...){
      formula = stats::as.formula(formula)
      formula.vars = all.vars(formula)
      if (formula.vars[2] != "."){
        dti = dplyr::select(data,dplyr::all_of(formula.vars))
      } else {
        dti = data
      }
      newdti = tibble::tibble("Yvec" = dti[,formula.vars[1],drop = TRUE],
                               'TotalVariable' = rpart_disc(formula,dti,...))
      return(gdverse::gd(paste0("Yvec~TotalVariable"), newdti, type = "factor"))
    }
    res_sesu = purrr::map2(datalist, su,
                           \(.tbf, .spsu) gozh_total(formula,.tbf, ...) %>%
                             purrr::pluck('factor') %>%
                             dplyr::mutate(su = .spsu))
    sesu = tibble::tibble(spatial_units = su,
                          sesu_result = res_sesu)
    qv = res_sesu %>%
      purrr::list_rbind() %>%
      dplyr::pull(`Q-statistic`)
    optsu = gdverse::loess_optscale(qv,su,increase_rate)
  }
  res = list('sesu' = sesu,
             'optsu' = optsu[1],
             'strategy' = strategy,
             'increase_rate' = optsu[2])
  class(res) = 'sesu_gozh'
  return(res)
}

#' @title print gozh sesu
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for gozh sesu from `sesu_gozh()`.
#'
#' @param x Return by `sesu_gozh()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @export
#'
print.sesu_gozh = \(x,...){
  g = purrr::list_rbind(x$sesu$sesu_result)
  spunits = x$sesu$spatial_units
  cat("   Size Effect Of Spatial Units Using GOZH Model   \n",
      "***        Optimal Spatial Unit:",x$optsu)
  for (i in spunits){
    cat(sprintf("\n Spatial Unit: %s ",i))
    print(knitr::kable(dplyr::filter(g,su==i) %>% dplyr::select(-su),
                       format = "markdown",digits = 12, align = 'c', ...))
  }
}

#' @title plot gozh sesu
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for gozh sesu in `sesu_gozh()`.
#'
#' @param x Return by `sesu_gozh()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer.
#' @export
#'
plot.sesu_gozh = \(x,...){
  if (x$strategy == 1){
    class(x) ='sesu_opgd'
    return(plot.sesu_opgd(x,...))
  } else {
    increase_rate = x$increase_rate
    g = purrr::list_rbind(x$sesu$sesu_result) %>%
      dplyr::rename(qv = `Q-statistic`, pv = `P-value`)
    loessf = stats::loess(qv ~ su, data = g)
    loessrate = (loessf$fitted - dplyr::lag(loessf$fitted)) / dplyr::lag(loessf$fitted)
    g$rate = loessrate
    maxrate = max(g$rate, na.rm = TRUE)
    maxqv = max(g$qv)

    fig_g = fig_sesu = ggplot2::ggplot(data = g, ggplot2::aes(x = su)) +
      ggplot2::geom_line(ggplot2::aes(y = qv, color = "qv")) +
      ggplot2::geom_point(ggplot2::aes(y = qv, color = "qv")) +
      ggplot2::geom_line(ggplot2::aes(y = rate * maxqv / maxrate, color = "rate")) +
      ggplot2::geom_point(ggplot2::aes(y = rate * maxqv / maxrate, color = "rate")) +
      ggplot2::geom_hline(yintercept = increase_rate * maxqv / maxrate,
                          color = "grey40", linetype = "dashed") +
      ggplot2::scale_color_manual(values = c("qv" = "#f8766d", "rate" = "#6598cc"),
                                  labels = c("Q value", "Increase rate"), name = "") +
      ggplot2::scale_x_continuous(
        name = "Size of spatial unit",
        breaks = g$su
      ) +
      ggplot2::scale_y_continuous(
        name = "Q value",
        sec.axis = ggplot2::sec_axis(~ . * maxrate / maxqv,
                                     name = "Increase rate")
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(family = "serif",color = "#6598cc"),
        axis.text.y.right = ggplot2::element_text(family = "serif",color = "#6598cc"),
        axis.title.y.left = ggplot2::element_text(family = "serif",color = "#f8766d"),
        axis.text.y.left = ggplot2::element_text(family = "serif",color = "#f8766d"),
        panel.grid = ggplot2::element_blank(),
        legend.position = "inside",
        legend.justification = c('left','bottom'),
        legend.background = ggplot2::element_rect(fill = 'transparent'),
        ...
      )
    return(fig_g)
  }
}
