weights_x <- function(x) {
  1 / pmax(x, 1e-6)
}

weights_x2 <- function(x) {
  1 / pmax(x, 1e-6)^2
}

weights_empirical <- function(dt) {
  w <- dt[,
    .(
      variance = var(y)
    ),
    by = x
  ]

  w[,
    weight := 1 / variance
  ]

  w
}

weights_true <- function(dt) {
  1 / dt$sigma^2
}
