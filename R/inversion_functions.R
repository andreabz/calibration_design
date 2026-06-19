library(data.table)

invert_calibration <- function(
    model,
    y,
    x_range = NULL
) {

  # detect linear model
  is_linear <- inherits(
    model,
    "lm"
  ) &&
    length(coefficients(model)) == 2

  if(is.null(x_range)) {

    x_range <- range(
      model$model$x
    )

  }

  if(is_linear) {

    coef <- coefficients(model)

    intercept <- coef[1]
    slope <- coef[2]

    return(
      (y - intercept) / slope
    )

  }

  # generic inverse calibration
  vapply(
    y,
    function(y_value) {

      f <- function(x) {

        predict(
          model,
          newdata = data.frame(
            x = x
          )
        ) - y_value

      }

      uniroot(
        f,
        interval = x_range
      )$root

    },
    numeric(1)
  )
}