#' @title Pipe operator
#' @description
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @usage lhs \%>\% rhs
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @return `NULL` (this is the magrittr pipe operator)
#' @export
NULL

#' @title generate permutations
#'
#' @param x A vector.
#' @param seed (optional) Random seed number. Default is `123456789`.
#'
#' @return A permutations vector.
#' @export
#'
#' @examples
#' gen_permutations(1:100,42)
#'
gen_permutations = \(x,seed = 123456789){
  set.seed(seed)
  return(sample(x))
}

#' @title assign values by weight
#'
#' @param x A numeric value
#' @param w A weight vector
#' @param list (optional) Return list or not. if `list` is `TRUE`, return a list,
#' otherwise return a vector. Default is `FALSE`.
#'
#' @return A numeric Vector.
#' @export
#'
#' @examples
#' weight_assign(0.875,1:3)
#'
weight_assign = \(x,w,list = FALSE){
  if (list) {
    return(list(x * w / sum(w)))
  } else {
    return(x * w / sum(w))
  }
}

#' @title convert all discretized vectors to integer
#'
#' @param x A discretized vector.
#'
#' @return An integer vector.
#' @export
#'
#' @examples
#' all2int(factor(letters[1:3],levels = c('b','a','c')))
#' all2int(letters[1:3])
#'
all2int = \(x){
  if (inherits(x,"factor")){
    x = as.integer(x)
  } else if (inherits(x,'numeric')) {
    x = as.integer(x)
  } else if (inherits(x,'character')) {
    x = as.integer(as.factor(x))
  }
  return(x)
}
