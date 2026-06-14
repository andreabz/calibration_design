metric_rmse <- function(dt_pred) {
  sqrt(
    mean(
      (dt_pred$y_pred - dt_pred$y_true)^2
    )
  )
}

metric_mae <- function(dt_pred) {
  mean(
    abs(
      dt_pred$y_pred -
        dt_pred$y_true
    )
  )
}

metric_bias <- function(dt_pred) {
  mean(
    dt_pred$y_pred -
      dt_pred$y_true
  )
}

metric_bias_pct <- function(dt_pred) {
  mean(
    100 *
      (dt_pred$y_pred -
        dt_pred$y_true) /
      dt_pred$y_true
  )
}

metric_rmse_pct <- function(dt_pred) {
  sqrt(
    mean(
      (100 *
        (dt_pred$y_pred -
          dt_pred$y_true) /
        dt_pred$y_true)^2
    )
  )
}

metric_coverage <- function(dt_pred) {
  mean(
    dt_pred$y_true >= dt_pred$lwr &
      dt_pred$y_true <= dt_pred$upr
  )
}

metric_mean_pi_width <- function(dt_pred) {
  mean(
    dt_pred$upr -
      dt_pred$lwr
  )
}

metric_mean_pi_width_pct <- function(dt_pred) {
  mean(
    100 *
      (dt_pred$upr -
        dt_pred$lwr) /
      dt_pred$y_pred
  )
}

metric_mean_upl <- function(dt_pred) {
  mean(
    dt_pred$upr
  )
}

metric_mean_upl_distance <- function(dt_pred) {
  mean(
    dt_pred$upr -
      dt_pred$y_pred
  )
}

metric_mean_upl_pct <- function(dt_pred) {
  mean(
    100 *
      (dt_pred$upr -
        dt_pred$y_pred) /
      dt_pred$y_pred
  )
}

metric_curvature_detected <- function(
  dt,
  alpha = 0.05
) {
  m1 <- lm(
    y ~ x,
    data = dt
  )

  m2 <- lm(
    y ~ poly(
      x,
      2,
      raw = TRUE
    ),
    data = dt
  )

  p <- anova(
    m1,
    m2
  )$`Pr(>F)`[2]

  p < alpha
}

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
