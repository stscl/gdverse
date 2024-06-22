#' @title calculate inverse distance weight
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for calculate inverse distance weight.
#' @details
#' The inverse distance weight formula is
#' \eqn{w_{ij} = 1 / d_{ij}^\alpha}
#'
#' @param locx The x axis location.
#' @param locy The y axis location.
#' @param power (optional) Default is 1. Set to 2 for gravity weights.
#' @param is_arc (optional) FALSE (default) or TRUE, whether to compute arc distance.
#'
#' @return A inverse distance weight matrices with class of `matrix`.
#' @importFrom stats as.dist
#' @importFrom stats dist
#' @importFrom geosphere distm
#' @export
#'
#' @examples
#' x = 1:10
#' y = 1:10
#' inverse_distance_weight(x,y)
#' inverse_distance_weight(x,y,is_arc = TRUE)
inverse_distance_weight = \(locx,locy,power = 1,is_arc = FALSE){
  if (is_arc) {
    distij = stats::as.dist(geosphere::distm(matrix(c(locx,locy),
                                                    ncol = 2)))
  } else {
    distij = stats::dist(data.frame(locx,locy))
  }
  wij = 1 / distij ^ power
  return(as.matrix(wij))
}

#' @title randomly shuffling vector
#'
#' @param x A vector.
#' @param shuffle_rate The shuffling rate.
#' @param seed (optional) Random seed number. Default is `123456789`.
#'
#' @return A shuffled vector.
#' @export
#'
shuffle_vector = \(x,shuffle_rate,seed = 123456789){
  set.seed(seed)
  n = length(x)
  shuffle_size = floor(n * shuffle_rate)
  if (shuffle_size == 0) {
    return(x)
  } else {
    shuffle_indices = sample(1:n, size = shuffle_size)
    new_x = c(x[shuffle_indices], x[-shuffle_indices])
    return(new_x)
  }
}

#' @title rescale continuous vector to specified minimum and maximum
#'
#' @param x A continuous numeric vector.
#' @param to_left (optional) Specified minimum. Default is `0`.
#' @param to_right (optional) Specified maximum. Default is `1`.
#'
#' @return A continuous vector which has rescaled.
#' @export
#'
#' @examples
#' rescale_vector(c(-5,1,5),0.01,0.99)
#'
rescale_vector = \(x,to_left = 0,to_right = 1){
  xmin = range(x,na.rm = TRUE)[1]
  xmax = range(x,na.rm = TRUE)[2]
  xnew = (x - xmin) / (xmax - xmin) * (to_right - to_left) + to_left
  return(xnew)
}

#' @title generates subsets of a set
#'
#' @param set A vector
#' including the empty set and the set itself. Default is `TRUE`.
#' @param empty (optional) When `empty` is `TRUE`,the generated subset includes the empty set,
#' otherwise the empty set is removed. Default is `TRUE`.
#' @param self (optional) When `self` is `TRUE`,the resulting subset includes the set itself,
#' otherwise the set itself is removed. Default is `TRUE`.
#'
#' @return A list with the subsets
#' @export
#'
#' @examples
#' generate_subsets(letters[1:3])
#' generate_subsets(letters[1:3],empty = FALSE)
#' generate_subsets(letters[1:3],self = FALSE)
#' generate_subsets(letters[1:3],empty = FALSE,self = FALSE)
#'
generate_subsets = \(set,empty = TRUE,self = TRUE) {
  n = length(set)
  subsets = list(c())
  for (i in seq(set)) {
    subsets = c(subsets, utils::combn(set, i, simplify = FALSE))
  }
  if (!empty) {subsets = subsets[-1]}
  if (!self & empty) {subsets = subsets[-2^n]}
  if (!self & !empty) {subsets = subsets[-(2^n-1)]}
  return(subsets)
}
