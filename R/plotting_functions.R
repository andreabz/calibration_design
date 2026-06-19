library(data.table)
library(ggplot2)

#' Plot calibration observations with an optional true mean response
#'
#' @param dt Calibration dataset with columns `x` and `y`.
#' @param mean_fun Optional function returning the expected response at `x`.
#'
#' @return A `ggplot` object.
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
          x = x,
          y = y
        ),
        linewidth = 1
      )
  }

  p
}

#' Plot residuals against fitted response
#'
#' @param model Fitted model with residuals and fitted values.
#'
#' @return A `ggplot` object.
plot_residuals <- function(model) {
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
      x = "Estimated response",
      y = "Residual"
    ) +
    theme_bw()
}

#' Plot relative concentration prediction error
#'
#' @param pred_table Prediction table returned by `predict_calibration_model()`.
#'
#' @return A `ggplot` object.
plot_prediction_error <- function(pred_table) {
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

#' Plot the location of calibration measurements by design
#'
#' @param dt Dataset with columns `x` and `design`.
#'
#' @return A `ggplot` object.
plot_design_comparison <- function(dt) {
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

#' Plot calibration weight functions
#'
#' @param dt Dataset with columns `x`, `weight`, and `method`.
#'
#' @return A `ggplot` object with log-scaled weights.
plot_weights <- function(dt) {
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
    labs(
      x = "Concentration",
      y = "Weight",
      colour = "Weighting rule"
    ) +
    theme_bw()
}
