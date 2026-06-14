library(data.table)

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
