library(here)
library(data.table)
library(ggplot2)

source(here("R", "design_functions.R"))
source(here("R", "response_functions.R"))
source(here("R", "variance_functions.R"))
source(here("R", "weight_functions.R"))
source(here("R", "linkedin_plot_functions.R"))

output_dir <- here("posts", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

designs <- list(
  "15 x 1" = design_15x1(1, 100),
  "5 x 3" = design_5x3(1, 100),
  "3 x 5" = design_3x5(1, 100)
)

design_data <- rbindlist(Map(
  function(dt, name) {
    dt[, design := name]
    dt
  },
  designs,
  names(designs)
))

grid <- data.table(x = seq(1, 100, length.out = 300))

variance_data <- copy(grid)
variance_data[, sigma := sigma_linear_constant_rsd(x)]

weights_rsd <- rbindlist(list(
  data.table(x = grid$x, method = "True inverse variance", weight = 1 / sigma_linear_constant_rsd(grid$x)^2),
  data.table(x = grid$x, method = "1/x", weight = weights_x(grid$x)),
  data.table(x = grid$x, method = "1/x2", weight = weights_x2(grid$x))
))

weights_loq <- rbindlist(list(
  data.table(x = grid$x, method = "True mixed variance", weight = 1 / sigma_linear_loq_mixed(grid$x)^2),
  data.table(x = grid$x, method = "1/x", weight = weights_x(grid$x)),
  data.table(x = grid$x, method = "1/x2", weight = weights_x2(grid$x))
))

curve_data <- copy(grid)
curve_data[, y_curved := f_mild_downward_curvature(x)]
curve_data[, y_linear_reference := f_linear(x)]

ggsave(
  filename = file.path(output_dir, "01_calibration_design.png"),
  plot = plot_linkedin_design_allocation(design_data), width = 10, height = 10,
  dpi = 150, bg = "white"
)
ggsave(
  filename = file.path(output_dir, "02_heteroscedasticity.png"),
  plot = plot_linkedin_variance_pattern(variance_data), width = 10, height = 10,
  dpi = 150, bg = "white"
)
ggsave(
  filename = file.path(output_dir, "03_weighted_least_squares.png"),
  plot = plot_linkedin_weights(
    weights_rsd,
    title = "Weights are variance assumptions.",
    subtitle = "Under constant RSD, inverse-variance weights follow 1/x²."
  ),
  width = 10, height = 10, dpi = 150, bg = "white"
)
ggsave(
  filename = file.path(output_dir, "04_loq_weight_mismatch.png"),
  plot = plot_linkedin_weights(
    weights_loq,
    title = "At the LOQ, 1/x² can over-weight.",
    subtitle = "A low-end noise floor flattens the real inverse-variance curve."
  ),
  width = 10, height = 10, dpi = 150, bg = "white"
)
ggsave(
  filename = file.path(output_dir, "05_nonlinearity_detection.png"),
  plot = plot_linkedin_curvature(curve_data), width = 10, height = 10,
  dpi = 150, bg = "white"
)
