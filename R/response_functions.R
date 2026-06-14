f_linear <- function(
  x,
  intercept = 0,
  slope = 1
) {
  intercept + slope * x
}

f_plateau <- function(
  x,
  vmax = 100,
  km = 30
) {
  vmax * x / (km + x)
}
