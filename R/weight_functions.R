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

#' Calculate inverse-variance weights from simulated response SDs
#'
#' @param dt Simulated dataset containing a `sigma` column.
#'
#' @return Numeric vector of inverse-variance weights.
weights_from_simulated_sigma <- function(dt) {
  1 / dt$sigma^2
}
