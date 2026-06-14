fit_ols <- function(dt) {
  lm(
    y ~ x,
    data = dt
  )
}

fit_wls_x <- function(dt) {
  lm(
    y ~ x,
    data = dt,
    weights = weights_x(dt$x)
  )
}

fit_wls_x2 <- function(dt) {
  lm(
    y ~ x,
    data = dt,
    weights = weights_x2(dt$x)
  )
}

fit_wls_true <- function(dt) {
  lm(
    y ~ x,
    data = dt,
    weights = 1 / dt$sigma^2
  )
}

fit_wls_empirical <- function(dt) {
  dt <- copy(dt)

  dt[,
    weight := if (.N > 1) {
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
    weight := fifelse(
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
