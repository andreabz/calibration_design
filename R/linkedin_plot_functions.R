library(data.table)
library(ggplot2)

#' LinkedIn-friendly plot theme
#'
#' @return A `ggplot2` theme with large text and minimal chart furniture for a
#'   square social-media graphic.
theme_linkedin <- function() {
  theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 24),
      plot.subtitle = element_text(size = 15, colour = "#4B5563"),
      plot.caption = element_text(size = 10, colour = "#6B7280"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

#' Plot how 15 calibration measurements can be allocated
#'
#' @param design_data Data with columns `x` and `design`.
#'
#' @return A square-friendly `ggplot` object for a LinkedIn post.
plot_linkedin_design_allocation <- function(design_data) {
  design_data <- copy(design_data)
  design_data[, design := factor(
    design,
    levels = c("15 x 1", "5 x 3", "3 x 5")
  )]

  ggplot(design_data, aes(x = x, y = 1)) +
    geom_point(size = 5, colour = "#0F766E") +
    facet_wrap(~design, ncol = 1) +
    scale_y_continuous(NULL, breaks = NULL) +
    labs(
      title = "15 measurements. Three ways to invest them.",
      subtitle = "More levels reveal curve shape; more replicates reveal repeatability.",
      x = "Concentration",
      caption = "Calibration Design Simulations"
    ) +
    theme_linkedin() +
    theme(strip.text = element_text(face = "bold", size = 18))
}

#' Plot a changing response standard deviation for LinkedIn
#'
#' @param variance_data Data with columns `x` and `sigma`.
#'
#' @return A square-friendly `ggplot` object.
plot_linkedin_variance_pattern <- function(variance_data) {
  ggplot(variance_data, aes(x = x, y = sigma)) +
    geom_line(linewidth = 1.5, colour = "#2563EB") +
    labs(
      title = "A straight calibration can still have changing error.",
      subtitle = "With constant RSD, absolute response scatter grows with concentration.",
      x = "Concentration",
      y = "Response standard deviation",
      caption = "Calibration Design Simulations"
    ) +
    theme_linkedin()
}

#' Plot competing weighting assumptions for LinkedIn
#'
#' @param weight_data Data with columns `x`, `weight`, and `method`.
#'
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#'
#' @return A square-friendly `ggplot` object.
plot_linkedin_weights <- function(
  weight_data,
  title,
  subtitle
) {
  plot_data <- copy(weight_data)
  plot_data[, relative_weight := weight / max(weight), by = method]

  ggplot(plot_data, aes(x = x, y = relative_weight, colour = method)) +
    geom_line(linewidth = 1.4) +
    scale_y_log10() +
    scale_colour_manual(values = c(
      "True inverse variance" = "#0F766E",
      "True mixed variance" = "#0F766E",
      "1/x" = "#D97706",
      "1/x2" = "#DC2626"
    )) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Concentration",
      y = "Relative weight (log scale)",
      caption = "Calibration Design Simulations"
    ) +
    theme_linkedin()
}

#' Plot mild downward curvature against a linear reference for LinkedIn
#'
#' @param curve_data Data with columns `x`, `y_curved`, and
#'   `y_linear_reference`.
#'
#' @return A square-friendly `ggplot` object.
plot_linkedin_curvature <- function(curve_data) {
  long_data <- rbindlist(list(
    data.table(
      x = curve_data$x,
      response = curve_data$y_curved,
      model = "Mild downward curvature"
    ),
    data.table(
      x = curve_data$x,
      response = curve_data$y_linear_reference,
      model = "Linear reference"
    )
  ))

  ggplot(long_data, aes(x = x, y = response, colour = model, linetype = model)) +
    geom_line(linewidth = 1.5) +
    scale_colour_manual(values = c(
      "Mild downward curvature" = "#DC2626",
      "Linear reference" = "#374151"
    )) +
    scale_linetype_manual(values = c(
      "Mild downward curvature" = "solid",
      "Linear reference" = "dashed"
    )) +
    labs(
      title = "A small bend can hide inside a good-looking line.",
      subtitle = "The analytical question is whether prediction remains suitable in the intended range.",
      x = "Concentration",
      y = "Response",
      caption = "Calibration Design Simulations"
    ) +
    theme_linkedin()
}
