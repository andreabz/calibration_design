#' Fit ordinary least squares calibration
#'
#' @param dt Calibration `data.table` or `data.frame` with columns `x` and `y`.
#'
#' @return An `lm` object for `y ~ x`.
fit_ols <- function(dt) {
  lm(
    y ~ x,
    data = dt
  )
}

#' Fit weighted least squares using 1/x weights
#'
#' @param dt Calibration data with columns `x` and `y`.
#'
#' @return An `lm` object for `y ~ x` fitted with `weights_x(dt$x)`.
fit_wls_x <- function(dt) {
  lm(
    y ~ x,
    data = dt,
    weights = weights_x(dt$x)
  )
}

#' Fit weighted least squares using 1/x^2 weights
#'
#' @param dt Calibration data with columns `x` and `y`.
#'
#' @return An `lm` object for `y ~ x` fitted with `weights_x2(dt$x)`.
fit_wls_x2 <- function(dt) {
  lm(
    y ~ x,
    data = dt,
    weights = weights_x2(dt$x)
  )
}

#' Fit weighted least squares using known response standard deviations
#'
#' @param dt Calibration data with columns `x`, `y`, and `sigma`.
#'
#' @return An `lm` object for `y ~ x` fitted with inverse-variance weights.
fit_wls_true <- function(dt) {
  lm(
    y ~ x,
    data = dt,
    weights = 1 / dt$sigma^2
  )
}

#' Fit weighted least squares using replicate-based variance estimates
#'
#' @param dt Calibration data with columns `x` and `y`. Replicated `x` values
#'   are required to estimate empirical variances.
#'
#' @return An `lm` object for `y ~ x`. If no replicated levels are available,
#'   OLS is returned with a warning.
fit_wls_empirical <- function(dt) {
  dt <- data.table::copy(dt)

  dt[,
    weight := if (.N > 1 && var(y) > 0) {
      1 / var(y)
    } else {
      NA_real_
    },
    by = x
  ]

  if (all(is.na(dt$weight))) {
    warning(
      "Not enough replicates to estimate empirical weights. Returning OLS."
    )

    return(
      fit_ols(dt)
    )
  }

  dt[,
    weight := data.table::fifelse(
      is.na(weight),
      median(weight, na.rm = TRUE),
      weight
    )
  ]

  lm(
    y ~ x,
    data = dt,
    weights = weight
  )
}
