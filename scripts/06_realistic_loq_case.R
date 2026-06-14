sigma_homo <- function(
  x,
  sigma = 5
) {
  rep(sigma, length(x))
}

sigma_linear <- function(
  x,
  sigma0 = 1,
  slope = 0.1
) {
  sigma0 + slope * x
}

sigma_constant_rsd <- function(
  x,
  mean_fun,
  rsd = 0.05
) {
  mean_fun(x) * rsd
}

sigma_loq <- function(
  x,
  mean_fun,
  threshold = 10,
  rsd_low = 0.20,
  rsd_high = 0.05
) {
  mu <- mean_fun(x)

  rsd <- ifelse(
    x < threshold,
    rsd_low,
    rsd_high
  )

  mu * rsd
}
