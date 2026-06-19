library(ggplot2)
library(data.table)

plot_design <- function(
  dt,
  mean_fun = NULL
) {
  p <- ggplot(
    dt,
    aes(
      x = x,
      y = y
    )
  ) +
    geom_point(
      size = 2
    ) +
    labs(
      x = "Concentration",
      y = "Response"
    ) +
    theme_bw()

  if (!is.null(mean_fun)) {
    xgrid <- data.table(
      x = seq(
        min(dt$x),
        max(dt$x),
        length.out = 200
      )
    )

    xgrid[,
      y := mean_fun(x)
    ]

    p <- p +
      geom_line(
        data = xgrid,
        aes(
          x,
          y
        ),
        linewidth = 1
      )
  }

  p
}

plot_calibration_fit <- function(
  pred_table
) {
  ggplot(
    pred_table,
    aes(
      x = x
    )
  ) +

    geom_point(
      aes(
        y = y_obs
      ),
      alpha = 0.6
    ) +

    geom_line(
      aes(
        y = y_true
      ),
      linewidth = 1
    ) +

    geom_line(
      aes(
        y = y_pred
      ),
      linetype = 2,
      linewidth = 1
    ) +

    geom_ribbon(
      aes(
        ymin = lwr,
        ymax = upr
      ),
      alpha = 0.2
    ) +

    labs(
      x = "Concentration",
      y = "Response"
    ) +

    theme_bw()
}

plot_residuals <- function(
  model
) {
  dt <- data.table(
    fitted = fitted(model),
    residuals = residuals(model)
  )

  ggplot(
    dt,
    aes(
      x = fitted,
      y = residuals
    )
  ) +

    geom_point() +

    geom_hline(
      yintercept = 0,
      linetype = 2
    ) +

    labs(
      x = "Estimated value",
      y = "Residual"
    ) +

    theme_bw()
}

plot_abs_residuals <- function(
  model
) {
  dt <- data.table(
    fitted = fitted(model),
    abs_residuals = abs(residuals(model))
  )

  ggplot(
    dt,
    aes(
      x = fitted,
      y = abs_residuals
    )
  ) +

    geom_point() +

    geom_smooth(
      method = "loess",
      se = FALSE
    ) +

    labs(
      x = "Estimated value",
      y = "|Residual|"
    ) +

    theme_bw()
}

plot_prediction_error <- function(
  pred_table
) {
  dt <- copy(pred_table)

  dt[,
    error_pct := 100 *
      (x_pred - x_true) /
      x_true
  ]

  ggplot(
    dt,
    aes(
      x = x_true,
      y = error_pct
    )
  ) +

    geom_point(
      alpha = 0.4
    ) +

    geom_hline(
      yintercept = 0,
      linetype = 2
    ) +

    geom_smooth(
      se = FALSE
    ) +

    labs(
      x = "Concentration",
      y = "Error (%)"
    ) +

    theme_bw()
}

plot_upl_profile <- function(
  pred_table
) {
  dt <- copy(pred_table)

  dt[,
    upl_pct := 100 *
      (x_upl - x_pred) /
      x_pred
  ]

  ggplot(
    dt,
    aes(
      x = x_true,
      y = upl_pct
    )
  ) +

    geom_line(
      linewidth = 1
    ) +

    labs(
      x = "Concentration",
      y = "Upper Prediction Limit (%)"
    ) +

    theme_bw()
}

plot_design_comparison <- function(
  dt
) {
  ggplot(
    dt,
    aes(
      x = x,
      y = 1
    )
  ) +

    geom_point(
      size = 3
    ) +

    facet_wrap(
      ~design,
      scales = "free_x"
    ) +

    labs(
      x = "Concentration",
      y = NULL
    ) +

    theme_bw() +

    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

plot_weights <- function(
  dt
) {
  ggplot(
    dt,
    aes(
      x = x,
      y = weight,
      colour = method
    )
  ) +

    geom_line(
      linewidth = 1
    ) +

    scale_y_log10() +

    theme_bw()
}

plot_inverse_calibration <- function(
    dt
) {

  ggplot(
    dt,
    aes(
      x = x_true,
      y = x_pred
    )
  ) +

    geom_point() +

    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = 2
    ) +

    labs(
      x = "True concentration",
      y = "Predicted concentration"
    ) +

    theme_bw()

}