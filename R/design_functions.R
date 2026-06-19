library(data.table)

#' Create a calibration design with 15 unreplicated levels
#'
#' @param xmin Lowest concentration in the calibration range.
#' @param xmax Highest concentration in the calibration range.
#'
#' @return A `data.table` with one column, `x`, containing 15 equally spaced
#'   calibration concentrations.
design_15x1 <- function(
  xmin,
  xmax
) {
  data.table(
    x = seq(
      xmin,
      xmax,
      length.out = 15
    )
  )
}

#' Create a calibration design with five levels in triplicate
#'
#' @param xmin Lowest concentration in the calibration range.
#' @param xmax Highest concentration in the calibration range.
#'
#' @return A `data.table` with one column, `x`, containing five equally spaced
#'   calibration concentrations, each repeated three times.
design_5x3 <- function(
  xmin,
  xmax
) {
  levels <- seq(
    xmin,
    xmax,
    length.out = 5
  )

  data.table(
    x = rep(
      levels,
      each = 3
    )
  )
}

#' Create a calibration design with three levels in quintuplicate
#'
#' @param xmin Lowest concentration in the calibration range.
#' @param xmax Highest concentration in the calibration range.
#'
#' @return A `data.table` with one column, `x`, containing the low, middle, and
#'   high calibration concentrations, each repeated five times.
design_3x5 <- function(
  xmin,
  xmax
) {
  levels <- c(
    xmin,
    (xmin + xmax) / 2,
    xmax
  )

  data.table(
    x = rep(
      levels,
      each = 5
    )
  )
}
