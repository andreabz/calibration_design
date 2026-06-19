#' Linear calibration response
#'
#' @param x Numeric vector of concentrations.
#' @param intercept Response at zero concentration.
#' @param slope Response increase per concentration unit.
#'
#' @return Numeric vector of expected responses.
f_linear <- function(
  x,
  intercept = 0,
  slope = 1
) {
  intercept + slope * x
}

#' Plateau-like calibration response
#'
#' @param x Numeric vector of concentrations.
#' @param vmax Maximum response approached at high concentration.
#' @param km Concentration giving half of `vmax`.
#'
#' @return Numeric vector of expected responses.
f_plateau <- function(
  x,
  vmax = 140,
  km = 40
) {
  vmax * x / (km + x)
}

#' Mild downward curvature response
#'
#' @param x Numeric vector of concentrations.
#' @param slope Initial low-concentration slope.
#' @param curvature Positive coefficient controlling the downward bend.
#'
#' @return Numeric vector of expected responses. The response remains increasing
#'   over the range used in the examples, but its slope decreases with
#'   concentration.
f_saturation_quadratic <- function(
  x,
  slope = 1,
  curvature = 0.0008
) {
  slope * x - curvature * x^2
}
