library(data.table)

#' Simulate calibration or unknown-sample responses
#'
#' @param design `data.table` or `data.frame` with a concentration column `x`.
#' @param mean_fun Function returning the expected response at `x`.
#' @param variance_fun Function returning the response standard deviation at `x`.
#'
#' @return A `data.table` with concentration, observed response, true mean,
#'   response standard deviation, and relative standard deviation.
simulate_dataset <- function(
  design,
  mean_fun,
  variance_fun
) {
  mu <- mean_fun(design$x)
  sigma <- variance_fun(design$x)

  y <- rnorm(
    length(mu),
    mu,
    sigma
  )

  data.table(
    x = design$x,
    y = y,
    mu = mu,
    sigma = sigma,
    rsd = sigma / abs(mu)
  )
}

#' Generate a random test set of unknown samples
#'
#' @param n Number of unknown samples to simulate.
#' @param xmin Lowest concentration to sample.
#' @param xmax Highest concentration to sample.
#' @param mean_fun Function returning the expected response at `x`.
#' @param variance_fun Function returning the response standard deviation at `x`.
#'
#' @return A simulated dataset with uniformly sampled concentrations.
generate_test_set <- function(
  n = 1000,
  xmin,
  xmax,
  mean_fun,
  variance_fun
) {
  x <- runif(
    n,
    xmin,
    xmax
  )

  simulate_dataset(
    design = data.table(x = x),
    mean_fun = mean_fun,
    variance_fun = variance_fun
  )
}
