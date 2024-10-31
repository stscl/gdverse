#' interpretable stratified power(ISP) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for interpretable stratified power model.
#' @note
#' Please set up python dependence and configure `GDVERSE_PYTHON` environment variable if you want to run `rgd()`.
#' See `vignette('rgdrid',package = 'gdverse')` for more details.
#'
#' @param formula A formula of ISP model.
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#' @param discvar (optional) Name of continuous variable columns that need to be discretized. Noted that
#' when `formula` has `discvar`, `data` must have these columns. By default, all independent variables are
#' used as `discvar`.
#' @param discnum A numeric vector of discretized classes of columns that need to be discretized.
#' Default all `discvar` use `3:22`.
#' @param overlay (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `and`.
#' @param strategy (optional) Optimal discretization strategy. When `strategy` is `1L`, choose the highest q-statistics to
#' determinate optimal spatial data discretization parameters. When `strategy` is `2L`, The optimal discrete parameters of
#' spatial data are selected by combining LOESS model.
#' @param increase_rate (optional) The critical increase rate of the number of discretization. Default is `5%`.
#' @param minsize (optional) The min size of each discretization group. Default all use `1`.
#' @param cores (optional) Positive integer (default is 1). When cores are greater than 1, use
#' multi-core parallel computing.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#'
#' @return A list.
#' \describe{
#' \item{\code{factor}}{factor detect results}
#' \item{\code{interaction}}{interaction detect results}
#' \item{\code{optdisc}}{independent variable optimal spatial discretization}
#' \item{\code{risk1}}{whether values of the response variable between a pair of overlay zones are significantly different}
#' \item{\code{risk2}}{risk detection result of the input data}
#' \item{\code{rpd}}{robust power of determinants}
#' \item{\code{spd}}{shap power of determinants}
#' \item{\code{determination}}{determination of the optimal interaction of variables}
#' \item{\code{number_individual_explanatory_variables}}{the number of individual explanatory variables used for examining the interaction effects}
#' \item{\code{number_overlay_zones}}{the number of overlay zones}
#' \item{\code{percentage_finely_divided_zones}}{the percentage of finely divided zones that are determined by the interaction of variables}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ## The following code needs to configure the Python environment to run:
#' data('sim')
#' sim1 = dplyr::select(sim,-dplyr::any_of(c('lo','la')))
#' g = isp(y ~ ., data = sim1, discnum = 3:8, cores = 6)
#' g
#' }
isp = \(formula, data, discvar = NULL, discnum = 3:22,
        overlay = 'and', strategy = 2L, increase_rate = 0.05,
        minsize = 1, cores = 1, alpha = 0.95){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)

  if (inherits(data,'sf')) {
    data = sf::st_drop_geometry(data)
  }
  data = tibble::as_tibble(data)

  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }

  yname = formula.vars[1]
  xname = colnames(data)[-which(colnames(data) == yname)]
  if (is.null(discvar)) {
    xdiscname = xname
    xundiscname = NULL
  } else {
    xdiscname = discvar
    xundiscname = xname[-which(xname %in% discvar)]
  }
  discdf = dplyr::select(data,dplyr::all_of(c(yname,xdiscname)))
  rgd_res = gdverse::rgd(paste0(yname," ~ ."), data = discdf,
                         discnum = discnum, minsize = minsize, cores = cores)
  qs = rgd_res[[1]]
  dti = rgd_res[[2]]
  qs$variable = factor(qs$variable,levels = xdiscname)
  qs = dplyr::rename(qs,qvalue = `Q-statistic`)

  if (strategy == 1L) {
    opt_discnum = dplyr::group_split(qs,variable) |>
      purrr::map_dbl(\(.df) .df$discnum[which.max(.df$qvalue)])
  } else {
    suppressWarnings({opt_discnum = dplyr::group_split(qs,variable) |>
      purrr::map_dbl(\(.df) sdsfun::loess_optnum(.df$qvalue, .df$discnum,
                                                 increase_rate = increase_rate)[1])})
  }
  res_discdf = purrr::map_dfc(seq_along(opt_discnum),
                       \(.n) {dn = which(dti$discnum == opt_discnum[.n])
                       return(dti[dn,.n])})

  if (!is.null(xundiscname)){
    dti = data %>%
      dplyr::select(dplyr::any_of(c(yname,xundiscname))) %>%
      dplyr::bind_cols(res_discdf)
  } else {
    dti = data %>%
      dplyr::select(dplyr::any_of(yname)) %>%
      dplyr::bind_cols(res_discdf)
  }

  rpd_factor = gd(paste0(yname,' ~ .'),data = dti,type = "factor")[[1]]

  xname = colnames(dti)[-which(colnames(dti) == yname)]
  xs = generate_subsets(xname,empty = FALSE, self = TRUE)
  spfom = overlay

  rpd_isp = \(formula, discdata, overlaymethod = 'and'){
    formula = stats::as.formula(formula)
    formula.vars = all.vars(formula)
    if (formula.vars[2] != "."){
      discdata = dplyr::select(discdata,dplyr::all_of(formula.vars))
    }
    yname = formula.vars[1]
    if (overlaymethod == 'intersection'){
      fuzzyzone = discdata %>%
        dplyr::select(-dplyr::any_of(yname)) %>%
        purrr::reduce(paste,sep = '_')
    } else {
      fuzzyzone = sdsfun::fuzzyoverlay(formula,discdata,overlaymethod)
    }
    qtheta = factor_detector(discdata[,yname,drop = TRUE],fuzzyzone)[[1]]
    return(qtheta)
  }

  calcul_rpd = \(.x){
    qv = rpd_isp(paste(yname,'~',paste0(.x,collapse = '+')),dti,spfom)
    names(qv) = 'RPD'
    return(qv)
  }

  doclust = FALSE
  if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('factor_detector'))
    out_g = parallel::parLapply(cores, xs, calcul_rpd)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xs, calcul_rpd)
  }
  out_rpd = dplyr::pull(out_g,1)
  m = length(xname)
  mf = factorial(m)

  get_value_by_varname = \(fv,namelist,valuevec){
    for (ni in seq_along(namelist)) {
      if (setequal(fv,namelist[[ni]])) {
        res = valuevec[ni]
        break
      } else {
        res = 0
      }
    }
    return(res)
  }

  calcul_shap = \(xvar){
    fullxvar = xname[-which(xname == xvar)]
    fullvar = generate_subsets(fullxvar,empty = FALSE,self = TRUE)

    calcul_unishap = \(xvar,fullxvar,namelist,valuevec){
      n = length(fullxvar)
      v1 = get_value_by_varname(c(xvar,fullxvar),namelist,valuevec)
      v2 = get_value_by_varname(fullxvar,namelist,valuevec)
      thetax = factorial(n) * factorial(m - n - 1) * (v1 - v2) / mf
      return(thetax)
    }

    thetaxs = purrr::map_dbl(fullvar, \(.x) calcul_unishap(xvar,.x,xs,out_rpd))
    thetax = sum(thetaxs)
    names(thetax) = 'SPD'
    return(thetax)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('generate_subsets'))
    out_g = parallel::parLapply(cores,xname,calcul_shap)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xname,calcul_shap)
  }
  out_spd = dplyr::pull(out_g,1)

  IntersectionSymbol = rawToChar(as.raw(c(0x20, 0xE2, 0x88, 0xA9, 0x20)))
  xsname = purrr::map_chr(xs,\(.x) paste(.x,collapse = IntersectionSymbol))
  interactvar = xs[[which.max(out_rpd)]]
  if (overlay == 'intersection'){
    reszone = dti %>%
      dplyr::select(dplyr::all_of(interactvar)) %>%
      purrr::reduce(paste,sep = '_')
  } else {
    reszone = sdsfun::fuzzyoverlay(paste(yname,'~',paste0(interactvar,collapse = '+')),
                                   dti, overlay)
  }
  zonenum = as.numeric(table(reszone))
  percentzone = length(which(zonenum==1)) / length(reszone)
  risk1 = risk_detector(dti[,yname,drop = TRUE],reszone,alpha)
  risk2 = risk1 %>%
    dplyr::select(dplyr::all_of(c('zone1st','zone2nd','Risk'))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(c('zone1st','zone2nd')),
                        names_to = 'zone', values_to = 'zone_risk') %>%
    dplyr::distinct(zone_risk,.keep_all = TRUE)
  risk2 = tibble::tibble(reszone = paste0('zone',reszone)) %>%
    dplyr::left_join(risk2, by = c('reszone' = 'zone_risk')) %>%
    dplyr::pull(Risk)
  res_rpd = tibble::tibble(variable = xsname,
                           rpd = out_rpd) %>%
    dplyr::arrange(dplyr::desc(rpd))
  res_spd = tibble::tibble(variable = xname,
                           spd = out_spd) %>%
    dplyr::arrange(dplyr::desc(spd))

  interact_type = \(qv1,qv2,qv12){
    if (qv12 < min(qv1, qv2)) {
      interaction = c("Weaken, nonlinear")
    } else if (qv12 >= min(qv1, qv2) & qv12 <= max(qv1, qv2)) {
      interaction = c("Weaken, uni-")
    } else if (qv12 > max(qv1, qv2) & (qv12 < qv1 + qv2)) {
      interaction = c("Enhance, bi-")
    } else if (qv12 == qv1 + qv2) {
      interaction = c("Independent")
    } else {
      interaction = c("Enhance, nonlinear")
    }
    return(interaction)
  }

  interaction_indice = which(sapply(xs, length) == 2)
  interaction_qv = out_rpd[interaction_indice]
  interaction_xv = xs[interaction_indice]
  variable1 = purrr::map_chr(seq_along(interaction_xv),
                             \(.x) interaction_xv[[.x]][1])
  variable2 = purrr::map_chr(seq_along(interaction_xv),
                             \(.x) interaction_xv[[.x]][2])
  qv1 = purrr::map_dbl(variable1, \(.x) get_value_by_varname(.x,xs,out_rpd))
  qv2 = purrr::map_dbl(variable2, \(.x) get_value_by_varname(.x,xs,out_rpd))
  interaction = tibble::tibble(
    "Variable1 Q-statistics" = qv1, "Variable2 Q-statistics" = qv2,
    "Variable1 and Variable2 interact Q-statistics" = interaction_qv,
    "variable1" = variable1, "variable2" = variable2,
    "Interaction" = purrr::pmap_chr(list(qv1 = qv1, qv2 = qv2,
                                         qv12 = interaction_qv),
                                    interact_type)) %>%
    dplyr::select(variable1,variable2,Interaction,
                  dplyr::everything()) %>%
    dplyr::left_join(dplyr::select(res_spd,variable,spd1 = spd),
                     by = c("variable1" = "variable")) %>%
    dplyr::left_join(dplyr::select(res_spd,variable,spd2 = spd),
                     by = c("variable2" = "variable")) %>%
    dplyr::mutate(spd = (abs(spd1) + abs(spd2)), spd1 = abs(spd1) / spd, spd2 = abs(spd2) / spd,
                  `Variable1 SPD` = `Variable1 and Variable2 interact Q-statistics`*spd1,
                  `Variable2 SPD` = `Variable1 and Variable2 interact Q-statistics`*spd2) %>%
    dplyr::select(-dplyr::starts_with('spd'))

  step_interaction = sapply(xs, length)
  max_rpd_names = sapply(unique(step_interaction), function(.s) {
    step_rpd = out_rpd[which(step_interaction == .s)]
    step_indice = which.max(step_rpd)
    return(xs[which(step_interaction == .s)][step_indice])
  })
  new_rpd_indice = vector("logical",length(xs))
  new_xsname = vector("character",length(xs))
  for (i in seq_along(xs)) {
    if (step_interaction[i] == 1){
      new_rpd_indice[i] = TRUE
      new_xsname[i] = xs[[i]]
    } else {
      current_name = max_rpd_names[step_interaction[i] - 1]
      if (all(unlist(current_name) %in% unlist(xs[i]))) {
        new_rpd_indice[i] = TRUE
        new_xsname[i] = setdiff(xs[[i]], current_name[[1]])
      } else {
        new_rpd_indice[i] = FALSE
      }
    }
  }
  determination = tibble::tibble(variable = xsname[new_rpd_indice],
                                 rpd = out_rpd[new_rpd_indice],
                                 step = step_interaction[new_rpd_indice],
                                 name = new_xsname[nchar(new_xsname) > 0]) %>%
    dplyr::group_by(step) %>%
    dplyr::arrange(rpd,.by_group=TRUE) %>%
    dplyr::ungroup()

  res = list("factor" = rpd_factor, "interaction" = interaction, "optdisc" = res_discdf,
             "risk1" = risk1, "risk2" = risk2, "rpd" = res_rpd, "spd" = res_spd,
             "determination" = determination,
             "number_individual_explanatory_variables" = length(interactvar),
             "number_overlay_zones" = length(zonenum),
             "percentage_finely_divided_zones" =  percentzone)
  class(res) = "isp_result"

  return(res)
}

#' @title print ISP result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for ISP model from `isp()`.
#'
#' @param x Return by `isp()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @export
#'
print.isp_result = \(x, ...) {
  cat("***     Interpretable Stratified Power Model     \n")
  cat("\n ---------- Global Power of Determinat : ------------")
  print(knitr::kable(x$factor, format = "markdown", digits = 12, align = 'c', ...))
  cat("\n ---------- Global Variable Interaction : ------------")
  print(knitr::kable(x$interaction[,1:3], format = "markdown", digits = 12, align = 'c', ...))
  cat("\n ---------- ISP Model Variable Interaction : ------------")
  print(knitr::kable(utils::head(dplyr::rename(x$rpd, RPD = rpd),5),
                     format = "markdown", digits = 12, align = 'c', ...))
  cat("\n ---------- ISP Model Performance Evaluation: ---------\n",
      "* Number of overlay zones : ", x$number_overlay_zones, "\n",
      "* Percentage of finely divided zones : ",x$percentage_finely_divided_zones,"\n",
      "* Number of individual explanatory variables : ",x$number_individual_explanatory_variables,"\n",
      "\n ## Different of response variable between a pair of overlay zones:")
  x = dplyr::select(x$risk1,zone1st,zone2nd,Risk)
  print(knitr::kable(utils::head(x,5), format = "markdown", align = 'c', ...))
  cat("\n #### Only the first five pairs of interactions and overlay zones are displayed! ####")
}

#' @title plot ISP result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for ISP result in `isp()`.
#'
#' @param x Return by `isp()`.
#' @param low_color (optional) The low color of the color gradient, default is `#6600CC`.
#' @param high_color (optional) The high color of the color gradient, default is `#FFCC33`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @export
#'
plot.isp_result = \(x, low_color = "#6600CC",
                    high_color = "#FFCC33", ...){
  g = x$determination
  gv1 = dplyr::count(g,name)
  g = g %>%
    dplyr::left_join(gv1,by = "name") %>%
    dplyr::mutate(name = forcats::fct_reorder(name, n, .desc = FALSE),
                  step = factor(step))
  g_arrow1 = dplyr::slice_tail(g,n = 1,by = step) %>%
    dplyr::rename(x = step, y = name) %>%
    dplyr::mutate(xend = c(utils::tail(x, n = -1), NA),
                  yend = c(utils::tail(y, n = -1), NA))
  fig_p = ggplot2::ggplot(g,
                          ggplot2::aes(x = step, y = name)) +
    ggplot2::geom_point(ggplot2::aes(col = rpd, size = rpd)) +
    ggplot2::geom_segment(data = g_arrow1,
                          ggplot2::aes(x = x, y = y,
                                       xend = xend,
                                       yend = yend),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm")),
                          color = "grey40", na.rm = TRUE) +
    ggplot2::geom_point(data = dplyr::filter(g,rpd == max(g$rpd)),
                        ggplot2::aes(x = step, y = name),
                        color = "red", shape = "*", size = 12.5) +
    ggplot2::scale_color_gradient(low = low_color,
                                  high = high_color) +
    ggplot2::labs(x = "No. of variables in fuzzy overlay", y = "",
                  size = "", color = "Q value") +
    ggplot2::guides(size = "none") +
    ggplot2::coord_fixed() +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "inside",
                   legend.justification = c('right','bottom'),
                   ...)
  return(fig_p)
}
