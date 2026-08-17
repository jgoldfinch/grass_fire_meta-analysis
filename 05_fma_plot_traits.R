# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 07/28/2026

# Function purpose: Plot trait distributions (tile plot)

#################################### Intro #####################################

# Function name: fma_plot_traits
# Description: function to count occurrences of annual/perennial by C3/C4 
# faceted by native/invasive status and plot as a heat tile map.

################################# Arguments ####################################

# data:
#       Data frame/tibble (e.g., d_3.1_g2, d_3.1_s2, d_3.1_f2).
# plot_title:
#       Optional character string for the plot main title (e.g., "Growth").

################################## Output ######################################

# p - A ggplot object displaying the heatmap tiles.

################################# Function #####################################
fma_plot_traits <- function(data, plot_title = NULL) {
  
  p <- data %>%
    dplyr::count(annual_perennial, C3_C4, native_invasive) %>%
    ggplot2::ggplot(ggplot2::aes(x = annual_perennial, y = C3_C4, fill = n)) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~ native_invasive) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(
      x = "Annual / Perennial",
      y = "C3 / C4",
      fill = "Count",
      title = plot_title
    ) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x=element_blank(), axis.title.y=element_blank())
  
  return(p)
}