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

.calc_ncfncp = \(Fv, df1, df2, alpha = 0.05) {
  if (Fv <= 0 || !is.finite(Fv)) return(0)
  if (df1 <= 0 || df2 <= 0 || alpha <= 0 || alpha >= 1) return(0)
  p0 = stats::pf(Fv, df1, df2, ncp = 0)
  if (p0 <= alpha) return(0)

  lambda_upper = 1.0
  max_iter = 20
  iter = 0

  while (iter < max_iter) {
    p_upper = stats::pf(Fv, df1, df2, ncp = lambda_upper)
    if (is.nan(p_upper) || is.na(p_upper) || p_upper < alpha) break
    lambda_upper = lambda_upper * 10
    iter = iter + 1
  }
  if (iter == max_iter) lambda_upper = 1e6

  tryCatch({
    root = stats::uniroot(
      f = function(lambda) stats::pf(Fv, df1, df2, ncp = lambda) - alpha,
      interval = c(0, lambda_upper),
      tol = .Machine$double.eps^0.25,
      maxiter = 1000
    )$root
    if (is.finite(root) && root >= 0) return(root) else 0
  }, error = function(e) 0)
}
