library(data.table)

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
    rsd = sigma / mu
  )
}

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

run_single_simulation <- function(
  design_fun,
  mean_fun,
  variance_fun,
  fit_fun,
  xmin = 0,
  xmax = 100,
  n_test = 1000
) {
  design <- design_fun(
    xmin,
    xmax
  )

  calibration_data <-
    simulate_dataset(
      design,
      mean_fun,
      variance_fun
    )

  model <-
    fit_fun(
      calibration_data
    )

  test_data <-
    generate_test_set(
      n = n_test,
      xmin = xmin,
      xmax = xmax,
      mean_fun = mean_fun,
      variance_fun = variance_fun
    )

  pred_table <-
    predict_calibration_model(
      model,
      test_data
    )

  pred_table
}
