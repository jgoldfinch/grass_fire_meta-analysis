# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 08/02/2026

# Function purpose: Plot I2 variance component breakdown across demographic outcomes

#################################### Intro #####################################

# Function name: fma_plot_i2_breakdown
# Description: Generates a stacked bar chart showing the relative contributions 
# of within-study (Level 2) versus between-study (Level 3) heterogeneity 
# (I² percentage) across demographic response categories.

################################# Arguments ####################################

# master_null:
#        The combined baseline null data frame containing Level 2 and Level 3 
#        variance percentage metrics (e.g., master_summary$baseline_null_table).

################################## Output ######################################

# A ggplot object displaying stacked bar charts of total heterogeneity (%) 
# partitioned into within-study and between-study components per demographic 
# response variable.

################################# Function #####################################
fma_plot_i2_breakdown <- function(master_null) {
  
  df_long <- master_null %>% 
    dplyr::select(Response_Variable, I2_Level2_Within_Study, I2_Level3_Between_Study) %>% 
    dplyr::mutate(
      Level2_Within = as.numeric(I2_Level2_Within_Study),
      Level3_Between = as.numeric(I2_Level3_Between_Study)
    ) %>% 
    dplyr::select(Response_Variable, Level2_Within, Level3_Between) %>% 
    tidyr::pivot_longer(
      cols = c(Level2_Within, Level3_Between),
      names_to = "Variance_Level",
      values_to = "I2_Percent"
    ) %>% 
    dplyr::mutate(
      Variance_Level = dplyr::case_when(
        Variance_Level == "Level2_Within" ~ "Level 2 (Within-Study)",
        Variance_Level == "Level3_Between" ~ "Level 3 (Between-Study)"
      )
    )
  
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Response_Variable, y = I2_Percent, fill = Variance_Level)) +
    ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.6) +
    ggplot2::scale_fill_manual(values = c("Level 3 (Between-Study)" = "#D95F02", "Level 2 (Within-Study)" = "#7570B3")) +
    ggplot2::labs(
      x = NULL,
      y = "Heterogeneity Share (I² %)",
      fill = "Variance distribution",
      title = "Partitioning of Heterogeneity Across Demographic Outcomes",
      subtitle = "Relative contributions of within-study vs. between-study variance"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "top",
      axis.text = ggplot2::element_text(color = "black")
    )
  
  return(p)
}