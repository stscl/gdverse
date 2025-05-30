#' @title interactive detector for spatial associations(IDSA) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @references
#' Yongze Song & Peng Wu (2021) An interactive detector for spatial associations,
#' International Journal of Geographical Information Science, 35:8, 1676-1701,
#' DOI:10.1080/13658816.2021.1882680
#' @note
#' **Please note that all variables in the IDSA model need to be continuous data**.
#'
#' The IDSA model requires at least \eqn{2^n-1} calculations when has \eqn{n} explanatory variables.
#' When there are more than 10 explanatory variables, carefully consider the computational burden of this model.
#' When there are a large number of explanatory variables, the data dimensionality reduction method can be used
#' to ensure the trade-off between analysis results and calculation speed.
#'
#' @param formula A formula of IDSA model.
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param wt (optional) The spatial weight matrix. When `data` is not an `sf` object, must provide `wt`.
#' @param discnum (optional) Number of multilevel discretization. Default will use `3:8`.
#' @param discmethod (optional) The discretization methods. Default all use `quantile`.
#' Noted that  `rpart` will use `rpart_disc()`; Others use `sdsfun::discretize_vector()`.
#' @param overlay (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `and`.
#' @param strategy (optional) Discretization strategy. When `strategy` is `1L`, choose the highest SPADE model q-statistics to
#' determinate optimal spatial data discretization parameters. When `strategy` is `2L`, The optimal discrete parameters of
#' spatial data are selected by combining LOESS model.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `5%`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#' @param seed (optional) Random number seed, default is `123456789`.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#' @param ... (optional) Other arguments passed to `cpsd_disc()`.
#'
#' @return A list.
#' \describe{
#' \item{\code{interaction}}{the interaction result of IDSA model}
#' \item{\code{risk}}{whether values of the response variable between a pair of overlay zones are significantly different}
#' \item{\code{number_individual_explanatory_variables}}{the number of individual explanatory variables used for examining the interaction effects}
#' \item{\code{number_overlay_zones}}{the number of overlay zones}
#' \item{\code{percentage_finely_divided_zones}}{the percentage of finely divided zones that are determined by the interaction of variables}
#' }
#' @export
#'
#' @examples
#' data('sim')
#' sim1 = sf::st_as_sf(sim,coords = c('lo','la'))
#' g = idsa(y ~ ., data = sim1)
#' g
#'
idsa = \(formula,data,wt = NULL,discnum = 3:8,discmethod = "quantile",
         overlay = 'and', strategy = 2L, increase_rate = 0.05,
         cores = 1, seed = 123456789, alpha = 0.95, ...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (inherits(data,'sf')) {
    if (is.null(wt)){
      wt = sdsfun::inverse_distance_swm(data)
    }
    data = sf::st_drop_geometry(data)
  } else if (inherits(data,'data.frame')) {
    if (is.null(wt)){
      stop("When `data` is `data.frame` or `tibble`, please provide `wt`!")
    }
  }
  data = tibble::as_tibble(data)
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) == yname)]
  discdf =  dplyr::select(data,dplyr::all_of(c(yname,xname)))
  g = gdverse::cpsd_disc(paste0(yname,'~',paste0(xname,collapse = '+')),
                data = discdf, wt = wt, discnum = discnum,
                discmethod = discmethod, strategy = strategy,
                increase_rate = increase_rate,
                cores = cores, seed = seed, ...)
  newdti = data %>%
    dplyr::select(dplyr::all_of(yname)) %>%
    dplyr::bind_cols(g$disv)
  dti = dplyr::select(data,dplyr::all_of(names(newdti)))
  xs = sdsfun::generate_subsets(xname,empty = FALSE, self = TRUE)

  calcul_pid = \(.x,wt,overlay){
    if (length(.x) == 1) {
      qv = gdverse::cpsd_spade(
        dti[,yname,drop = TRUE],
        dti[,.x,drop = TRUE],
        newdti[,.x,drop = TRUE],
        wt)
    } else {
      qv = gdverse::pid_idsa(paste(yname,'~',paste0(.x,collapse = '+')),
                             dti, newdti, wt, overlay)
    }
    names(qv) = 'pid_idsa'
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
    out_g = parallel::parLapply(cores,xs,calcul_pid,wt = wt,overlay = overlay)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xs, calcul_pid,wt = wt,overlay = overlay)
  }
  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  xsname = purrr::map_chr(xs,\(.x) paste(.x,collapse = IntersectionSymbol))
  interactvar = xs[[which.max(out_g$pid_idsa)]]
  if (overlay == 'intersection'){
    reszone = newdti %>%
      dplyr::select(dplyr::all_of(interactvar)) %>%
      purrr::reduce(paste,sep = '_')
  } else {
    reszone = sdsfun::fuzzyoverlay(paste(yname,'~',
                                         paste0(interactvar,collapse = '+')),
                                   newdti, overlay)
  }
  zonenum = as.numeric(table(reszone))
  percentzone = length(which(zonenum==1)) / length(reszone)
  risk1 = gdverse::risk_detector(dti[,yname,drop = TRUE],reszone,alpha)
  out_g = tibble::tibble(variable = xsname) %>%
    dplyr::bind_cols(out_g) %>%
    dplyr::arrange(dplyr::desc(pid_idsa))

  res = list("interaction" = out_g, "risk" = risk1,
             "number_individual_explanatory_variables" = length(interactvar),
             "number_overlay_zones" = length(zonenum),
             "percentage_finely_divided_zones" =  percentzone)
  class(res) = "idsa_result"
  return(res)
}

#' @title print IDSA result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for IDSA model from `idsa()`.
#'
#' @param x Return by `idsa()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @export
#'
print.idsa_result = \(x, ...) {
  cat("***     Interactive Detector For Spatial Associations ")
  print(knitr::kable(utils::head(dplyr::rename(x$interaction, PID = pid_idsa),5),
                     format = "markdown", digits = 12, align = 'c', ...))
  cat("\n --------- IDSA model performance evaluation: --------\n",
      "* Number of overlay zones : ", x$number_overlay_zones, "\n",
      "* Percentage of finely divided zones : ",x$percentage_finely_divided_zones,"\n",
      "* Number of individual explanatory variables : ",x$number_individual_explanatory_variables,"\n",
      "\n ## Different of response variable between a pair of overlay zones:")
  x = dplyr::select(x$risk,zone1st,zone2nd,Risk)
  print(knitr::kable(utils::head(x,5), format = "markdown", align = 'c', ...))
  cat("\n #### Only the first five pairs of interactions and overlay zones are displayed! ####")
}

#' @title plot IDSA risk result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for IDSA risk result in `idsa()`.
#'
#' @param x Return by `idsa()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @export
#'
plot.idsa_result = \(x, ...) {
  grd = dplyr::select(x$risk,zone1st,zone2nd,Risk) %>%
    dplyr::mutate(risk = forcats::fct_recode(Risk,"Y" = "Yes", "N" = "No"))
  fig_rd = ggplot2::ggplot(data = grd,
                           ggplot2::aes(x = zone1st, y = zone2nd, fill = risk)) +
    ggplot2::geom_tile(color = "white", size = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = risk), color = "black", family = "serif") +
    ggplot2::scale_fill_manual(values = c("N" = "#7fdbff", "Y" = "#ffa500")) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 60,hjust = 1,family = "serif"),
                   axis.text.y = ggplot2::element_text(family = "serif"),
                   legend.position = "none",
                   panel.grid = ggplot2::element_blank(), ...)
  return(fig_rd)
}
