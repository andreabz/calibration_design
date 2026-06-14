library(data.table)

predict_calibration_model <- function(
  model,
  test_data,
  level = 0.95
) {
  pred <- predict(
    model,
    newdata = test_data,
    interval = "prediction",
    level = level,
    se.fit = TRUE
  )

  data.table(
    x = test_data$x,

    y_true = test_data$mu,

    y_obs = test_data$y,

    y_pred = pred$fit[, "fit"],

    se_fit = pred$se.fit,

    lwr = pred$fit[, "lwr"],

    upr = pred$fit[, "upr"]
  )
}

summarise_prediction_profile <- function(
  pred_table,
  n_bins = 20
) {
  dt <- copy(pred_table)

  dt[,
    bin := cut(
      x,
      breaks = n_bins
    )
  ]

  dt[,
    .(
      mean_true = mean(y_true),
      mean_pred = mean(y_pred),

      bias = mean(
        y_pred - y_true
      ),

      rmse = sqrt(
        mean(
          (y_pred - y_true)^2
        )
      ),

      pi_width = mean(
        upr - lwr
      )
    ),
    by = bin
  ]
}
