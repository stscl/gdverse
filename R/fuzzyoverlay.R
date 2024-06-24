#' @title spatial fuzzy overlay
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for spatial fuzzy overlay.
#' @references
#' Yongze Song & Peng Wu (2021) An interactive detector for spatial associations,
#' International Journal of Geographical Information Science, 35:8, 1676-1701,
#' DOI:10.1080/13658816.2021.1882680
#'
#' @param formula A formula of spatial fuzzy overlay.
#' @param data A data.frame or tibble of observation data.
#' @param method (optional) Overlay methods. When `method` is `and`, use `min` to do
#' fuzzy overlay;and when `method` is `or`,use `max` to do fuzzy overlay. Default is `and`.
#'
#' @return A spatial fuzzy overlay vector.
#' @export
#'
st_fuzzyoverlay = \(formula, data, method = "and"){
  if (!(method %in% c("and","or"))){
    stop("`method` must `and` or `or`!")
  }

  fuzzyf = ifelse(method == "and","which.min","which.max")

  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  y = data[, formula.vars[1], drop = TRUE]
  if (formula.vars[2] == "."){
    xs = data[,-which(colnames(data) == formula.vars[1])]
  } else {
    xs = subset(data, TRUE, match(formula.vars[-1], colnames(data)))
  }
  xs = xs %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.factor),
                                as.character)) %>%
    purrr::map2_dfc(colnames(xs),
                    \(.x,.y) paste(.y,.x,sep = "_"))
  meanrisk = purrr::map(xs, \(.x) tapply(y,.x,mean))
  fuzzynum = rescale_vector(unlist(meanrisk,use.names = FALSE))
  names(fuzzynum) = unlist(lapply(meanrisk, names),use.names = FALSE)
  xsfn = dplyr::mutate(xs, dplyr::across(dplyr::everything(),
                                   \(.x) return(fuzzynum[.x])))
  fuzzyindice = apply(xsfn, 1, fuzzyf)
  fuzzyzone = xs %>%
    split(seq(nrow(xs))) %>%
    purrr::map2_chr(fuzzyindice,
                   \(.tdf,.indice) .tdf[1,.indice,drop = TRUE])
  return(fuzzyzone)
}
