# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 08/02/2026

# Function purpose: Plot moderator coefficient estimates across demographic outcomes

#################################### Intro #####################################

# Function name: fma_plot_moderator_coefficients
# Description: Generates a faceted coefficient forest plot comparing parameter 
# estimates and 95% confidence intervals between Candidate Model 1 (Aridity) 
# and Model 2 (Full Model) across demographic response categories.

################################# Arguments ####################################

# coef_df:
#        Data frame of coefficient summary statistics containing predictor 
#        estimates and 95% CIs across candidate models (e.g., 
#        master_summary$coefficients_table).

################################## Output ######################################

# A ggplot object displaying point estimates and 95% CI error bars faceted 
# by demographic response category, color-coded and shaped by model type.

################################# Function #####################################
fma_plot_moderator_coefficients <- function(coef_df) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  
  # Define custom colors and shapes matching the plot
  model_colors <- c(
    "Model 1 (Aridity)" = "#D95F02", # Orange
    "Model 2 (Full)"    = "#7570B3"  # Purple
  )
  
  model_shapes <- c(
    "Model 1 (Aridity)" = 16, # Circle
    "Model 2 (Full)"    = 17  # Triangle
  )
  
  # Updated display order from top to bottom (aligned with case_when outputs)
  moderator_levels <- c(
    "Photosynthetic Pathway [C4]",
    "Origin [Native]",
    "Life History [Perennial]",
    "Aridity (Centered)"
  )
  
  # Grid display order
  response_levels <- c("All", "Fecundity", "Growth", "Survival")
  
  # Step 1: Clean, rename, and map predictors from master_summary$coefficients_table
  plot_df <- coef_df %>%
    # Standardize column names
    dplyr::rename(
      CI_Lower = CI_95_Lower,
      CI_Upper = CI_95_Upper
    ) %>%
    # Drop intercepts
    dplyr::filter(!stringr::str_detect(Predictor, "intrcpt")) %>%
    # Map raw predictor strings to pretty labels
    dplyr::mutate(
      Moderator = dplyr::case_when(
        stringr::str_detect(Predictor, "aridity")                                                ~ "Aridity (Centered)",
        stringr::str_detect(Predictor, "C3_C4|C4") & !stringr::str_detect(Predictor, "unknown") ~ "Photosynthetic Pathway [C4]",
        stringr::str_detect(Predictor, "native_invasive|native") & !stringr::str_detect(Predictor, "unknown") ~ "Origin [Native]",
        stringr::str_detect(Predictor, "annual_perennial|perennial") & !stringr::str_detect(Predictor, "unknown") ~ "Life History [Perennial]",
        TRUE ~ NA_character_
      )
    ) %>%
    # Keep only the target predictors (excludes "unknown" factor levels)
    dplyr::filter(!is.na(Moderator)) %>%
    # Set factor levels for proper plot positioning
    dplyr::mutate(
      Moderator = factor(Moderator, levels = rev(moderator_levels)),
      Response_Variable = factor(Response_Variable, levels = response_levels),
      Model = factor(Model, levels = c("Model 1 (Aridity)", "Model 2 (Full)"))
    )
  
  # Step 2: Build coefficient plot
  p <- ggplot2::ggplot(
    plot_df, 
    ggplot2::aes(
      x = Estimate, 
      y = Moderator, 
      color = Model, 
      shape = Model,
      group = Model
    )
  ) +
    # Vertical reference line at 0
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.8) +
    
    # Error bars (95% CI)
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = CI_Lower, xmax = CI_Upper),
      height = 0.25,
      linewidth = 0.8,
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    
    # Point estimates
    ggplot2::geom_point(
      size = 3.5,
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    
    # 2x2 Facet layout with independent x-axis scales
    ggplot2::facet_wrap(~ Response_Variable, scales = "free_x", ncol = 2) +
    
    # Color and Shape scales
    ggplot2::scale_color_manual(values = model_colors, name = "Model") +
    ggplot2::scale_shape_manual(values = model_shapes, name = "Model") +
    
    # Titles and Labels
    ggplot2::labs(
      title = "Moderator Coefficients across Demographic Outcomes",
      subtitle = "Comparing parameter signals in Model 1 (Aridity) vs. Model 2 (Full Model)",
      x = expression(paste("Moderator Parameter Estimate (", beta, ") ± 95% CI")),
      y = NULL
    ) +
    
    # Custom Styling
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 12, margin = ggplot2::margin(b = 10)),
      strip.background = ggplot2::element_rect(fill = "#EFEFEF", color = "#000000"),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      axis.text = ggplot2::element_text(color = "#000000", size = 10),
      axis.title.x = ggplot2::element_text(face = "plain", size = 11, margin = ggplot2::margin(t = 10)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E5E5E5", linewidth = 0.4),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = 11),
      legend.text = ggplot2::element_text(size = 10),
      legend.box.margin = ggplot2::margin(t = 10)
    )
  
  return(p)
}