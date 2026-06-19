#' Invert a calibration model
#'
#' @param model Fitted calibration model. Linear `lm` models are inverted
#'   analytically; other model objects are inverted numerically with `uniroot()`.
#' @param y Numeric vector of response values to convert into concentrations.
#' @param x_range Optional numeric vector of length two giving the search range
#'   for numerical inversion. If omitted, the range of calibration `x` values is
#'   used.
#'
#' @return Numeric vector of estimated concentrations.
invert_calibration <- function(
  model,
  y,
  x_range = NULL
) {
  is_linear <- inherits(
    model,
    "lm"
  ) &&
    length(coefficients(model)) == 2

  if (is.null(x_range)) {
    x_range <- range(
      model$model$x
    )
  }

  if (is_linear) {
    coef <- coefficients(model)

    return(
      (y - coef[1]) / coef[2]
    )
  }

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
