# Project: Fire effects on plant demography in arid lands meta analysis (FMA)
# Author: Jessie Golding and Gemini
# Date: 08/02/2026

# Function purpose: Plot baseline pooled effect sizes across demographic responses

#################################### Intro #####################################

# Function name: fma_plot_baseline_forest
# Description: Generates a publication-quality forest plot of overall baseline 
# pooled effect sizes (Hedges' g) and 95% confidence intervals from Model 0 
# intercepts across demographic categories, scaled by sample size (k).

################################# Arguments ####################################

# primary_null:
#        The combined baseline null data frame containing pooled intercept 
#        estimates, standard errors, and effect counts (e.g., 
#        primary_summary$baseline_null_table).

################################## Output ######################################

# A ggplot object displaying horizontal error bars for 95% CIs and point sizes 
# scaled to the number of effect sizes (k) per demographic response.

################################# Function #####################################

fma_plot_baseline_forest <- function(primary_null) {

  # Parse standard numeric columns if needed
  df <- primary_null %>% 
    dplyr::mutate(
      Effect = as.numeric(Null_Effect_Estimate),
      k = as.numeric(k_Effects),
      p = as.numeric(p_value),
      # Re-extract numeric lower/upper bounds from CI string or recalculate
      SE_num = as.numeric(SE),
      ci_lb = Effect - 1.96 * SE_num,
      ci_ub = Effect + 1.96 * SE_num,
      Label = paste0(Response_Variable, " (k = ", k, ")")
    )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Effect, y = stats::reorder(Label, k))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lb, xmax = ci_ub), height = 0.2, color = "darkseagreen", linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(size = k), color = "darkseagreen") +
    ggplot2::scale_size_continuous(range = c(4, 8), guide = "none") +
    ggplot2::labs(
      x = "Pooled Effect Size (Hedges' g)",
      y = NULL,
      title = "Pooled Fire Demographic Responses",
      subtitle = "Intercept for each 3-level meta-analyses (95% CI)"
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.y = ggplot2::element_text(face = "bold", color = "black"),
      axis.text.x = ggplot2::element_text(color = "black")
    )
  
  return(p)
}