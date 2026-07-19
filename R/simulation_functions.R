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

#' Simulate curvature-detection power across calibration designs
#'
#' @param design_functions Named list of functions that create designs from
#'   `xmin` and `xmax`.
#' @param mean_fun Function returning the expected response at `x`.
#' @param variance_fun Function returning response standard deviations at `x`.
#' @param n_sim Number of calibration experiments per design.
#' @param weights_fun Optional function that receives a simulated dataset and
#'   returns regression weights.
#' @param alpha Significance level for the curvature test.
#' @param xmin Lowest concentration in the calibration range.
#' @param xmax Highest concentration in the calibration range.
#'
#' @return A `data.table` with the simulated curvature detection rate by design.
simulate_curvature_detection_power <- function(
  design_functions,
  mean_fun,
  variance_fun,
  n_sim = 500,
  weights_fun = NULL,
  alpha = 0.05,
  xmin = 1,
  xmax = 100
) {
  rbindlist(
    lapply(
      names(design_functions),
      function(design_name) {
        detections <- replicate(
          n_sim,
          {
            dt <- simulate_dataset(
              design = design_functions[[design_name]](xmin, xmax),
              mean_fun = mean_fun,
              variance_fun = variance_fun
            )

            metric_curvature_detected(
              dt,
              weights = if (is.null(weights_fun)) NULL else weights_fun(dt),
              alpha = alpha
            )
          }
        )

        data.table(
          design = design_name,
          detection_rate = mean(detections)
        )
      }
    )
  )
}
