#' Constant absolute response standard deviation
#'
#' @param x Numeric vector of concentrations.
#' @param sigma Constant response standard deviation.
#'
#' @return Numeric vector of response standard deviations.
sigma_homo <- function(
  x,
  sigma = 5
) {
  rep(
    sigma,
    length(x)
  )
}

#' Response standard deviation with constant relative standard deviation
#'
#' @param x Numeric vector of concentrations.
#' @param mean_fun Function returning the expected response at `x`.
#' @param rsd Relative standard deviation as a fraction.
#'
#' @return Numeric vector of response standard deviations.
sigma_constant_rsd <- function(
  x,
  mean_fun,
  rsd = 0.10
) {
  abs(mean_fun(x)) * rsd
}

#' Mixed LOQ-like response standard deviation
#'
#' @param x Numeric vector of concentrations.
#' @param mean_fun Function returning the expected response at `x`.
#' @param sigma_floor Low-concentration absolute standard-deviation floor.
#' @param rsd High-concentration relative standard deviation.
#'
#' @return Numeric vector of response standard deviations calculated as the
#'   larger of the absolute noise floor and the constant-RSD component.
sigma_loq_mixed <- function(
  x,
  mean_fun,
  sigma_floor = 4,
  rsd = 0.07
) {
  pmax(
    sigma_floor,
    sigma_constant_rsd(
      x,
      mean_fun = mean_fun,
      rsd = rsd
    )
  )
}
