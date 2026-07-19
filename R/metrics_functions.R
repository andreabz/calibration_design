library(data.table)

#' Root mean squared concentration error
#'
#' @param dt_pred Prediction table returned by `predict_calibration_model()`.
#'
#' @return Numeric scalar RMSE in concentration units.
metric_rmse <- function(dt_pred) {
  sqrt(
    mean(
      (dt_pred$x_pred - dt_pred$x_true)^2
    )
  )
}

#' Mean absolute concentration error
#'
#' @param dt_pred Prediction table returned by `predict_calibration_model()`.
#'
#' @return Numeric scalar MAE in concentration units.
metric_mae <- function(dt_pred) {
  mean(
    abs(
      dt_pred$x_pred - dt_pred$x_true
    )
  )
}

#' Mean signed concentration error
#'
#' @param dt_pred Prediction table returned by `predict_calibration_model()`.
#'
#' @return Numeric scalar bias in concentration units.
metric_bias <- function(dt_pred) {
  mean(
    dt_pred$x_pred - dt_pred$x_true
  )
}

#' Prediction interval coverage in concentration space
#'
#' @param dt_pred Prediction table returned by `predict_calibration_model()`.
#'
#' @return Fraction of true concentrations inside the prediction interval.
metric_coverage <- function(dt_pred) {
  mean(
    dt_pred$x_true >= dt_pred$x_lpl &
      dt_pred$x_true <= dt_pred$x_upl
  )
}

#' Mean concentration prediction interval width
#'
#' @param dt_pred Prediction table returned by `predict_calibration_model()`.
#'
#' @return Numeric scalar average interval width.
metric_mean_pi_width <- function(dt_pred) {
  mean(
    dt_pred$x_upl - dt_pred$x_lpl
  )
}

#' Detect heteroscedastic residual pattern
#'
#' @param model Fitted linear model.
#' @param alpha Significance level for the residual-pattern test.
#'
#' @return Logical scalar indicating whether absolute residuals are correlated
#'   with fitted values.
metric_hetero_detected <- function(
  model,
  alpha = 0.05
) {
  test <- cor.test(
    abs(residuals(model)),
    fitted(model)
  )

  test$p.value < alpha
}

#' Test a linear calibration model for quadratic curvature
#'
#' @param dt Calibration data with columns `x` and `y`.
#' @param weights Optional non-negative regression weights.
#' @param alpha Significance level for the nested-model comparison.
#'
#' @return `TRUE` when adding a quadratic term gives a p-value below `alpha`.
metric_curvature_detected <- function(
  dt,
  weights = NULL,
  alpha = 0.05
) {
  if (is.null(weights)) {
    linear_model <- lm(y ~ x, data = dt)
    quadratic_model <- lm(y ~ x + I(x^2), data = dt)
  } else {
    linear_model <- lm(y ~ x, data = dt, weights = weights)
    quadratic_model <- lm(y ~ x + I(x^2), data = dt, weights = weights)
  }

  p_value <- anova(linear_model, quadratic_model)$`Pr(>F)`[2]

  isTRUE(p_value < alpha)
}

#' Summarise a metric across designs and models
#'
#' @param prediction_tables Nested list of prediction tables by design and model.
#' @param metric_fun Function applied to each prediction table.
#'
#' @return A `data.table` with columns `design`, `model`, and `value`.
summarise_metrics <- function(
  prediction_tables,
  metric_fun
) {
  rbindlist(
    lapply(
      names(prediction_tables),
      function(design_name) {
        data.table(
          design = design_name,
          model = names(
            prediction_tables[[design_name]]
          ),
          value = sapply(
            prediction_tables[[design_name]],
            metric_fun
          )
        )
      }
    )
  )
}
