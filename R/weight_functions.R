#' Weight proportional to inverse concentration
#'
#' @param x Numeric vector of concentrations.
#'
#' @return Numeric vector of weights, using a small lower bound to avoid
#'   division by zero.
weights_x <- function(x) {
  1 / pmax(x, 1e-6)
}

#' Weight proportional to inverse squared concentration
#'
#' @param x Numeric vector of concentrations.
#'
#' @return Numeric vector of weights, using a small lower bound to avoid
#'   division by zero.
weights_x2 <- function(x) {
  1 / pmax(x, 1e-6)^2
}
