#' enhanced stratified power(ESP) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for enhanced stratified power model.
#' @note
#' Please set up python dependence and configure `GDVERSE_PYTHON` environment variable if you want to run `rgd()`.
#' See `vignette('rgdrid',package = 'gdverse')` for more details.
#'
#' @param formula A formula of ESP model.
#' @param data A data.frame, tibble or sf object of observation data.
#' @param wt (optional) The spatial weight matrix. When `data` is not an `sf` object, must provide `wt`.
#' @param discvar (optional) Name of continuous variable columns that need to be discretized. Noted that
#' when `formula` has `discvar`, `data` must have these columns. By default, all independent variables are
#' used as `discvar`.
#' @param discnum A numeric vector of discretized classes of columns that need to be discretized.
#' Default all `discvar` use `10`.
#' @param overlaymethod (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
#' Default is `and`.
#' @param minsize (optional) The min size of each discretization group. Default all use `1`.
#' @param cores (optional) Positive integer(default is 1). If cores > 1, use `python` `joblib` package to
#' parallel computation.
#' @param alpha (optional) Specifies the size of confidence level. Default is `0.95`.
#'
#' @return A list with ESP model result.
#' \describe{
#' \item{\code{factor}}{results of ESP model factor detection}
#' \item{\code{interaction}}{results of ESP model interaction detection}
#' \item{\code{risk1}}{whether values of the response variable between a pair of overlay zones are significantly different}
#' \item{\code{risk2}}{risk detection result of the input data}
#' \item{\code{psd}}{power of spatial determinants}
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
#' sim1 = sf::st_as_sf(sim,coords = c('lo','la'))
#' g = esp(y ~ ., data = sim1, discnum = 5)
#' }
esp = \(formula, data, wt = NULL, discvar = NULL,
        discnum = 10, overlaymethod = 'and',
        minsize = 1, cores = 1, alpha = 0.95){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)

  if (inherits(data,'sf')) {
    if (is.null(wt)){
      wt_esp = sdsfun::inverse_distance_swm(data)
    } else {
      wt_esp = wt
    }
    data = sf::st_drop_geometry(data)
  } else if (inherits(data,'data.frame')) {
    if (is.null(wt)){
      stop("When `data` is `data.frame` or `tibble`, please provide `wt` in idsa input!")
    } else {
      wt_esp = wt
    }
  }

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

  cores_disc = cores
  dti = robust_disc(paste0(yname,"~ ."), discdf, discnum, minsize, cores_disc)
  if (!is.null(xundiscname)){
    dti = data %>%
      dplyr::select(dplyr::any_of(c(yname,xundiscname))) %>%
      dplyr::bind_cols(dti)
  } else {
    dti = data %>%
      dplyr::select(dplyr::any_of(yname)) %>%
      dplyr::bind_cols(dti)
  }

  cores_spvar = cores
  cores_shap = cores
  xs = generate_subsets(xname,empty = FALSE, self = TRUE)
  spfom = overlaymethod

  psd_esp = \(formula, discdata, wt,
              overlaymethod = 'and'){
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
      fuzzyzone = st_fuzzyoverlay(formula,discdata,overlaymethod)
    }
    qtheta = psd_spade(discdata[,yname,drop = TRUE],
                       fuzzyzone, wt)
    return(qtheta)
  }

  calcul_psd = \(.x){
    qv = psd_esp(paste(yname,'~',paste0(.x,collapse = '+')),
                 dti, wt_esp, spfom)
    names(qv) = 'PSD'
    return(qv)
  }

  doclust = FALSE
  if (cores_spvar > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores_spvar)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  if (doclust) {
    parallel::clusterExport(cores,c('spvar','psd_spade','st_fuzzyoverlay'))
    out_g = parallel::parLapply(cores, xs, calcul_psd)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(xs, calcul_psd)
  }
  out_psd = dplyr::pull(out_g,1)
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

    thetaxs = purrr::map_dbl(fullvar, \(.x) calcul_unishap(xvar,.x,xs,out_psd))
    thetax = sum(thetaxs)
    names(thetax) = 'SPD'
    return(thetax)
  }

  doclust = FALSE
  if (cores_shap > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores_shap)
    on.exit(parallel::stopCluster(cores), add = TRUE)
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
  interactvar = xs[[which.max(out_psd)]]
  if (overlaymethod == 'intersection'){
    reszone = dti %>%
      dplyr::select(dplyr::all_of(interactvar)) %>%
      purrr::reduce(paste,sep = '_')
  } else {
    reszone = st_fuzzyoverlay(paste(yname,'~',paste0(interactvar,collapse = '+')),
                              dti, spfom)
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
  res_psd = tibble::tibble(variable = xsname,
                           psd = out_psd) %>%
    dplyr::arrange(dplyr::desc(psd))
  res_spd = tibble::tibble(variable = xname,
                           spd = out_spd) %>%
    dplyr::arrange(dplyr::desc(spd))

  factor_indice = which(sapply(xs, length) == 1)
  factor = tibble::tibble(variable = xsname[factor_indice],
                          `Q-statistics` = out_psd[factor_indice])

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
  interaction_qv = out_psd[interaction_indice]
  interaction_xv = xs[interaction_indice]
  variable1 = purrr::map_chr(seq_along(interaction_xv),
                             \(.x) interaction_xv[[.x]][1])
  variable2 = purrr::map_chr(seq_along(interaction_xv),
                             \(.x) interaction_xv[[.x]][2])
  qv1 = purrr::map_dbl(variable1, \(.x) get_value_by_varname(.x,xs,out_psd))
  qv2 = purrr::map_dbl(variable2, \(.x) get_value_by_varname(.x,xs,out_psd))
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
    dplyr::mutate(spd = (spd1 + spd2), spd1 = spd1 / spd, spd2 = spd2 / spd,
                  `Variable1 SPD` = `Variable1 and Variable2 interact Q-statistics`*spd1,
                  `Variable2 SPD` = `Variable1 and Variable2 interact Q-statistics`*spd2) %>%
    dplyr::select(-dplyr::starts_with('spd'))

  step_interaction = sapply(xs, length)
  max_psd_names = sapply(unique(step_interaction), function(.s) {
    step_psd = out_psd[which(step_interaction == .s)]
    step_indice = which.max(step_psd)
    return(xs[which(step_interaction == .s)][step_indice])
  })
  new_psd_indice = vector("logical",length(xs))
  new_xsname = vector("character",length(xs))
  for (i in seq_along(xs)) {
    if (step_interaction[i] == 1){
      new_psd_indice[i] = TRUE
      new_xsname[i] = xs[[i]]
    } else {
      current_name = max_psd_names[step_interaction[i] - 1]
      if (all(unlist(current_name) %in% unlist(xs[i]))) {
        new_psd_indice[i] = TRUE
        new_xsname[i] = setdiff(xs[[i]], current_name[[1]])
      } else {
        new_psd_indice[i] = FALSE
      }
    }
  }
  determination = tibble::tibble(variable = xsname[new_psd_indice],
                                 psd = out_psd[new_psd_indice],
                                 step = step_interaction[new_psd_indice],
                                 name = new_xsname[nchar(new_xsname) > 0]) %>%
    dplyr::group_by(step) %>%
    dplyr::arrange(psd,.by_group=TRUE) %>%
    dplyr::ungroup()

  res = list("factor" = factor, "interaction" = interaction, "risk1" = risk1, "risk2" = risk2,
             "psd" = res_psd, "spd" = res_spd, "determination" = determination,
             "number_individual_explanatory_variables" = length(interactvar),
             "number_overlay_zones" = length(zonenum),
             "percentage_finely_divided_zones" =  percentzone)
  class(res) = "esp_result"

  return(res)
}

#' @title print ESP result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to format output for ESP model from `esp()`.
#'
#' @param x Return by `esp()`.
#' @param ... (optional) Other arguments passed to `knitr::kable()`.
#'
#' @return Formatted string output
#' @method print esp_result
#' @export
#'
print.esp_result = \(x, ...) {
  cat("***       Enhanced Stratified Power Model   ")
  print(knitr::kable(utils::head(dplyr::rename(x$psd, PSD = psd),5),
                     format = "markdown", digits = 12, align = 'c', ...))
  cat("\n ---------- ESP model performance evaluation: ---------\n",
      "* Number of overlay zones : ", x$number_overlay_zones, "\n",
      "* Percentage of finely divided zones : ",x$percentage_finely_divided_zones,"\n",
      "* Number of individual explanatory variables : ",x$number_individual_explanatory_variables,"\n",
      "\n ## Different of response variable between a pair of overlay zones:")
  x = dplyr::select(x$risk1,zone1st,zone2nd,Risk)
  print(knitr::kable(utils::head(x,5), format = "markdown", align = 'c', ...))
  cat("\n #### Only the first five pairs of interactions and overlay zones are displayed! ####")
}

#' @title plot ESP result
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' S3 method to plot output for ESP result in `esp()`.
#'
#' @param x Return by `esp()`.
#' @param ... (optional) Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 layer
#' @method plot esp_result
#' @export
#'
plot.esp_result = \(x, shrink_factor = 0.8,
                    low_color = "#6600CC",
                    high_color = "#FFCC33", ...){
  g = x$determination
  gv1 = dplyr::count(g,name)
  g = g %>%
    dplyr::left_join(gv1,by = "name") %>%
    dplyr::mutate(name = forcats::fct_reorder(name, n, .desc = FALSE),
                  step = factor(step))
  g_text = dplyr::slice_tail(g,n = 1,by = step)
  fig_p = ggplot2::ggplot(g,
                          ggplot2::aes(x = step, y = name)) +
    ggplot2::geom_point(ggplot2::aes(col = psd, size = psd)) +
    ggplot2::scale_color_gradient(low = low_color,
                                  high = high_color) +
    ggplot2::geom_segment(data = g_text,
                          ggplot2::aes(x = step,y = name,
                                       xend = c(tail(step, n = -1), NA),
                                       yend = c(tail(name, n = -1), NA)),
                                       arrow = arrow(length = unit(0.5, "cm")),
                          color = "grey40") +
    ggplot2::labs(x = "No. of variables in fuzzy overlay", y = "",
                  size = "", color = "Q value") +
    ggplot2::guides(size = "none") +
    ggplot2::coord_fixed() +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "inside",
                   legend.justification = c('right','bottom'))
  return(fig_p)
}
