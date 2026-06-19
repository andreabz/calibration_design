library(data.table)

#' Prediction interval for future response observations
#'
#' @param model Fitted `lm` calibration model.
#' @param newdata Data frame containing prediction concentrations, usually a
#'   column named `x`.
#' @param variance_new Optional response variances for future observations.
#' @param variance_train Optional response variances for the calibration
#'   observations. When supplied, the coefficient covariance is computed from
#'   the known variance model and fitted weights.
#' @param weights_new Optional future-observation weights. Used only when
#'   `variance_new` is not supplied.
#' @param level Prediction interval confidence level.
#'
#' @return A `data.table` with fitted response, lower prediction limit, and
#'   upper prediction limit.
predict_y_interval <- function(
  model,
  newdata,
  variance_new = NULL,
  variance_train = NULL,
  weights_new = NULL,
  level = 0.95
) {
  X <- model.matrix(
    delete.response(
      terms(model)
    ),
    newdata
  )

  fit <- as.vector(
    X %*% coef(model)
  )

  model_weights <- weights(model)

  if (is.null(model_weights)) {
    model_weights <- rep(
      1,
      length(residuals(model))
    )
  }

  if (!is.null(variance_train)) {
    variance_train <- as.numeric(variance_train)

    if (length(variance_train) != length(residuals(model))) {
      stop(
        "variance_train must have one value per model observation."
      )
    }

    X_train <- model.matrix(model)

    xtwx_inv <- solve(
      crossprod(
        X_train,
        model_weights * X_train
      )
    )

    wx_train <- model_weights * X_train

    beta_vcov <- xtwx_inv %*%
      crossprod(
        wx_train,
        variance_train * wx_train
      ) %*%
      xtwx_inv
  } else {
    beta_vcov <- vcov(model)
  }

  var_fit <- rowSums(
    (X %*% beta_vcov) *
      X
  )

  sigma_ref2 <-
    sum(
      model_weights * residuals(model)^2
    ) /
      df.residual(model)

  if (!is.null(variance_new)) {
    var_new <- as.numeric(variance_new)
  } else if (is.null(weights_new)) {
    var_new <- sigma_ref2
  } else {
    var_new <- sigma_ref2 /
      as.numeric(weights_new)
  }

  if (length(var_new) == 1) {
    var_new <- rep(
      var_new,
      nrow(X)
    )
  }

  if (length(var_new) != nrow(X)) {
    stop(
      "variance_new or weights_new must have one value per prediction row."
    )
  }

  se <- sqrt(
    var_fit + var_new
  )

  tval <- qt(
    1 - (1 - level) / 2,
    df.residual(model)
  )

  data.table(
    fit = fit,
    lwr = fit - tval * se,
    upr = fit + tval * se
  )
}

#' Predict concentrations and prediction intervals for unknown samples
#'
#' @param model Fitted calibration model.
#' @param test_data Simulated unknown-sample data with columns `x`, `y`, and
#'   `mu`.
#' @param variance_fun Optional function returning response standard deviations
#'   at concentrations. If supplied, these standard deviations are squared before
#'   being used as variances.
#' @param level Prediction interval confidence level.
#' @param weight_fun Optional function returning future-observation weights.
#'
#' @return A `data.table` with true concentrations, observed responses,
#'   predicted concentrations, prediction limits, and prediction errors.
predict_calibration_model <- function(
  model,
  test_data,
  variance_fun = NULL,
  level = 0.95,
  weight_fun = NULL
) {
  if (!is.null(weight_fun)) {
    weights_new <- weight_fun(
      test_data$x
    )
    variance_new <- NULL
    variance_train <- NULL
  } else if (!is.null(variance_fun)) {
    sigma_new <- variance_fun(
      test_data$x
    )
    sigma_train <- variance_fun(
      model$model$x
    )
    variance_new <- sigma_new^2
    variance_train <- sigma_train^2
    weights_new <- 1 / variance_new
  } else {
    weights_new <- NULL
    variance_new <- NULL
    variance_train <- NULL
  }

  y_interval <- predict_y_interval(
    model,
    newdata = data.frame(
      x = test_data$x
    ),
    variance_new = variance_new,
    variance_train = variance_train,
    weights_new = weights_new,
    level = level
  )

  x_pred <- invert_calibration(
    model,
    test_data$y
  )

  if (!is.null(weight_fun)) {
    weights_pred <- weight_fun(
      x_pred
    )
    variance_pred <- NULL
  } else if (!is.null(variance_fun)) {
    sigma_pred <- variance_fun(
      x_pred
    )
    variance_pred <- sigma_pred^2
    weights_pred <- 1 / variance_pred
  } else {
    weights_pred <- NULL
    variance_pred <- NULL
  }

  x_interval_y <- predict_y_interval(
    model,
    newdata = data.frame(
      x = x_pred
    ),
    variance_new = variance_pred,
    variance_train = variance_train,
    weights_new = weights_pred,
    level = level
  )

  x_lpl <- invert_calibration(
    model,
    x_interval_y$lwr
  )

  x_upl <- invert_calibration(
    model,
    x_interval_y$upr
  )

  data.table(
    x_true = test_data$x,
    y_true = test_data$mu,
    y_obs = test_data$y,
    y_pred = y_interval$fit,
    y_lpl = y_interval$lwr,
    y_upl = y_interval$upr,
    x_pred = x_pred,
    x_lpl = pmin(x_lpl, x_upl),
    x_upl = pmax(x_lpl, x_upl),
    error = x_pred - test_data$x,
    relative_error = (x_pred - test_data$x) / test_data$x
  )
}

#' Summarise prediction performance by concentration bin
#'
#' @param pred_table Prediction table returned by `predict_calibration_model()`.
#' @param n_bins Number of concentration bins.
#'
#' @return A `data.table` with average prediction performance by concentration
#'   bin.
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
      mean_relative_error = mean(abs(relative_error)),
      coverage = mean(
        x_true >= x_lpl &
          x_true <= x_upl
      ),
      mean_interval_width = mean(
        x_upl - x_lpl
      )
    ),
    by = bin
  ][order(mean_true)]
}
