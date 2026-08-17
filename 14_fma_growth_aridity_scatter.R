# Project: Fire effects on plant demography in arid lands meta analysis (FMA)
# Author: Jessie Golding and Gemini
# Date: 08/02/2026

# Function purpose: Plot meta-regression bubble plot for Model 1 aridity effect

#################################### Intro #####################################

# Function name: fma_plot_aridity_regression
# Description: Generates a meta-regression scatter plot (bubble plot) showing 
# individual effect sizes sized by inverse variance weights, fitted regression 
# slope, 95% confidence intervals, and 95% prediction intervals along an 
# aridity gradient.

################################# Arguments ####################################

# model_1:
#        An rma.mv model object fitted for Model 1 (e.g., res_growth$m1_aridity).
# raw_data:
#        Data frame containing raw effect sizes and variance columns used to 
#        fit model_1.
# aridity_col:
#        Character string specifying the centered aridity column name in raw_data 
#        (default is "aridity_centered").
# response_label:
#        Character string used as a descriptive label for the plot title 
#        (default is "Demographic Response").

################################## Output ######################################

# A ggplot object displaying raw data points sized by inverse variance weights, 
# fitted meta-regression line, 95% confidence interval ribbon, and 95% 
# prediction interval ribbon.

################################# Function #####################################
fma_plot_aridity_regression <- function(model_1, raw_data, aridity_col = "aridity_centered", response_label = "Demographic Response") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("metafor", quietly = TRUE)) stop("Package 'metafor' is required.")
  
  # Calculate inverse variance weights for bubble sizing
  raw_data$inv_var <- 1 / raw_data$g_var
  
  # Generate continuous prediction grid across the aridity range
  grid_x <- seq(min(raw_data[[aridity_col]], na.rm = TRUE), 
                max(raw_data[[aridity_col]], na.rm = TRUE), length.out = 100)
  
  preds <- metafor::predict.rma(model_1, newmods = grid_x, pi.type = "rest")
  
  pred_df <- data.frame(
    x = grid_x,
    pred = preds$pred,
    ci_lb = preds$ci.lb,
    ci_ub = preds$ci.ub,
    pi_lb = preds$pi.lb,
    pi_ub = preds$pi.ub
  )
  
  p <- ggplot2::ggplot() +
    # Prediction Interval (light background band)
    ggplot2::geom_ribbon(data = pred_df, ggplot2::aes(x = x, ymin = pi_lb, ymax = pi_ub), fill = "#E69F00", alpha = 0.15) +
    # Confidence Interval (darker band)
    ggplot2::geom_ribbon(data = pred_df, ggplot2::aes(x = x, ymin = ci_lb, ymax = ci_ub), fill = "#D55E00", alpha = 0.30) +
    # Reference Line at null effect (0)
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    # Regression Line
    ggplot2::geom_line(data = pred_df, ggplot2::aes(x = x, y = pred), color = "#D55E00", linewidth = 1.2) +
    # Weighted Data Points (Bubbles)
    ggplot2::geom_point(data = raw_data, ggplot2::aes_string(x = aridity_col, y = "g", size = "inv_var"), 
                        shape = 21, fill = "gray60", color = "black", alpha = 0.6) +
    ggplot2::scale_size_continuous(name = "Study Weight (1/V)") +
    ggplot2::labs(
      x = "Centered Aridity Index",
      y = "Hedges' g (Fire Effect Size)",
      title = paste0("Fire Effect vs. Aridity: ", response_label),
      subtitle = "Shaded regions: 95% CI (dark) and 95% Prediction Interval (light)"
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text = ggplot2::element_text(color = "black")
    )
  
  return(p)
}