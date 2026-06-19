library(data.table)

invert_linear_model <- function(
    model,
    y
) {

  coef <- coefficients(model)

  intercept <- coef[1]
  slope <- coef[2]

  (y - intercept) / slope

}

predict_y_interval <- function(
    model,
    newdata,
    level = 0.95
) {

  predict(
    model,
    newdata = newdata,
    interval = "prediction",
    level = level
  )

}

predict_calibration_model <- function(
    model,
    test_data,
    level = 0.95
) {

  # prediction interval in response space
  y_interval <- predict_y_interval(
    model,
    data.frame(
      x = test_data$x
    ),
    level
  )

  # predicted concentration
  x_pred <- invert_calibration(
    model,
    test_data$y
  )

  # convert y limits back to x
  x_lpl <- invert_calibration(
    model,
    y_interval[, "lwr"]
  )


  x_upl <- invert_calibration(
    model,
    y_interval[, "upr"]
  )

  data.table(
    x_true = test_data$x,
    y_true = test_data$mu,
    y_obs = test_data$y,
    y_pred = y_interval[, "fit"],
    y_lpl = y_interval[, "lwr"],
    y_upl = y_interval[, "upr"],
    x_pred = x_pred,
    x_lpl = x_lpl,
    x_upl = x_upl,
    error = x_pred - test_data$x,
    relative_error = (x_pred - test_data$x) / test_data$x
  )

}

summarise_prediction_profile <- function(
    pred_table,
    n_bins = 20
) {

  dt <- copy(pred_table)

  dt[,
    bin := cut(
      x_true,
      breaks = n_bins
    )
  ]

  dt[,
    .(
      mean_true = mean(x_true),
      mean_pred = mean(x_pred),
      bias = mean(error),
      rmse = sqrt(
        mean(error^2)
      ),
      mean_relative_error =
        mean(abs(relative_error)),
      coverage =
        mean(
          x_true >= x_lpl &
          x_true <= x_upl
        )
    ),
    by = bin
  ][order(mean_true)]
}