# Project: Fire effects on plant demography in arid lands meta-analysis (abbr. = FMA)
# Author: Jessie Golding, with help from Claude AI and Gemini
# Date: 08/17/2026

# Primary File
# Run all data loading, cleaning, analysis, and output creation from here

################################################################################
## 00. Setup

# Set working directory where you have files downloaded from GitHub stored
setwd("C:/Users/jessi/OneDrive/Documents/Publications/In_progress/fire_meta_analysis")

# Load functions
source("./01_fma_load_packages.R") # load packages for analysis
source("./02_fma_data_load_clean.R") # clean and format data
source("./03_fma_convert_spatial.R") # convert data to spatial points
source("./04_fma_extract_aridity.R") # extract aridity, categorize, and plot
source("./05_fma_plot_traits.R") # plot combinations of traits that exist in the data
source("./06_fma_model_compare.R") # compare null model and full model results
source("./07_fma_format_result_tables.R") # create tables of results
source("./08_fma_compile_primary_tables.R") # create full tables of results
source("./09_fma_format_supp_table.R") # format results for supplement table
source("./10_fma_format_supp_table_data.R") # format data summary for supplement table
source("./11_fma_plot_null_intercepts.R") # plot null model intercepts with forest plot
source("./12_fma_plot_I2.R") # plot I2 variance
source("./13_fma_plot_coef.R") # plot coefficients for model 1 (aridity only) and model 2 (full)
source("./14_fma_growth_aridity_scatter.R") # scatter plot of growth vs. aridity

################################################################################
## 01. Format and clean data
 
# Load data
d <-read_csv("raw_data_08_17_2026.csv")

# Run function to clean data, calculate SMD, Cohen's d, and Hedges' g
fma_load_clean_data(d)

################################################################################
## 02. Turn data frame into spatial object and use a map to plot locations

d_1.0_sp <- fma_spatial_conversion(d_1.0)

################################################################################
## 03. Extract aridity values - https://www.nature.com/articles/s41597-022-01493-1
## Note that I used the annual aridity index (ai_v31_yr.tif) multiplied
## the raster by 0.0001 to convert it according to the authors' README file

# Specify where aridity raster is stored
ai_1.0_path <- "./ai_1.0.tif"

# Run function: extracts raster data, pops up the plot, and returns d_3.0
d_3.0 <- fma_extract_aridity(
  data_sp     = d_1.0_sp, 
  data_orig   = d_1.0, 
  raster_path = ai_1.0_path,
  show_plot   = TRUE
)

################################################################################
## 04. Format for analysis and examine covariate combinations

# Rename columns 
d_3.1 <-d_3.0 %>% rename(trt_n = treatment_group_sample_size_n, 
                         ctrl_n = control_group_sample_size_n,
                         smd = SMD, smd_var = SMD_var, smd_se = SMD_se)
d_3.1$effect_id <- seq_len(nrow(d_3.1))

# Define strings that should be treated as missing/unknown
na_strings <- c(
  "NA", "na", "N/A", "n/a", "not_provided", "not provided", "unknown", "",
  "C3_C4", "c3_c4", "annual_perennial", "native_invasive"
)

# Create final data set for analysis
d_3.1 <- d_3.1 %>%
  # 1. Trim whitespace and convert to character
  dplyr::mutate(
    C3_C4            = stringr::str_trim(as.character(C3_C4)),
    annual_perennial = stringr::str_trim(as.character(annual_perennial)),
    native_invasive  = stringr::str_trim(as.character(native_invasive))
  ) %>%
  # 2. Recode NAs, placeholders, and literal column names directly to "unknown"
  dplyr::mutate(
    C3_C4  = dplyr::if_else(C3_C4 %in% na_strings | is.na(C3_C4), "unknown", C3_C4),
    annual_perennial = dplyr::if_else(annual_perennial %in% na_strings | is.na(annual_perennial), "unknown", annual_perennial),
    native_invasive  = dplyr::if_else(native_invasive %in% na_strings | is.na(native_invasive), "unknown", native_invasive)
  ) %>%
  # 3. Convert to factors
  dplyr::mutate(
    C3_C4            = as.factor(C3_C4),
    annual_perennial = as.factor(annual_perennial),
    native_invasive  = as.factor(native_invasive)
  )

# Inspect factor levels
cat("C3_C4 levels:\n"); print(table(d_3.1$C3_C4, useNA = "always"))
cat("\nannual_perennial levels:\n"); print(table(d_3.1$annual_perennial, useNA = "always"))
cat("\nnative_invasive levels:\n"); print(table(d_3.1$native_invasive, useNA = "always"))

# Create separate data frames for each demographic category
# Growth 
d_3.1_g2 <-d_3.1 %>% filter(demg_resp_condensed == "growth_biomass") %>%
  dplyr::select(studyid, effect_id, g, g_var, trt_n, ctrl_n, aridity_centered, 
                native_invasive, C3_C4, annual_perennial)

# Survival
d_3.1_s2 <-d_3.1 %>% filter(demg_resp_condensed == "survival") %>%
  dplyr::select(studyid, effect_id, g, g_var, trt_n, ctrl_n, aridity_centered, 
                native_invasive, C3_C4, annual_perennial)

# Fecundity
d_3.1_f2 <-d_3.1 %>% filter(demg_resp_condensed == "fecundity") %>%
  dplyr::select(studyid, effect_id, g, g_var, trt_n, ctrl_n, aridity_centered, 
                native_invasive, C3_C4, annual_perennial)

# Combined with all demographic parameters, with filters applied
d_combined <- dplyr::bind_rows(
  d_3.1_g2  %>% dplyr::mutate(demographic = "growth"),
  d_3.1_s2  %>% dplyr::mutate(demographic = "survival"),
  d_3.1_f2  %>% dplyr::mutate(demographic = "fecundity")
) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(effect_id_global = row_number(.))

# Create a combined effect ID that is unique across all rows
d_combined$effect_id_global <- seq_len(nrow(d_combined))

################################################################################
# 5. Examine what combinations of covariates exist to see if we want to test for 
# potential interactions

## Plot covariate combos ##
# Growth
p_growth <- fma_plot_traits(d_3.1_g2, plot_title = "Growth")
print(p_growth)

# Survival
p_survival <- fma_plot_traits(d_3.1_s2, plot_title = "Survival")
print(p_survival)

# Fecundity
p_fecundity <- fma_plot_traits(d_3.1_f2, plot_title = "Fecundity")
print(p_fecundity)

################################################################################
## 06. Meta-analysis

res_growth <- fma_compare_models(
  d               = d_3.1_g2,
  variable        = "growth_biomass",
  aridity_formula = "~ aridity_centered",
  full_formula    = "~ aridity_centered + factor(C3_C4) + factor(native_invasive) + factor(annual_perennial)"
)

# View summary table of comparisons
res_growth$comparison

res_surv <- fma_compare_models(
  d               = d_3.1_s2,
  variable        = "survival",
  aridity_formula = "~ aridity_centered",
  full_formula    = "~ aridity_centered + factor(C3_C4) + factor(native_invasive) + factor(annual_perennial)"
)

# View summary table of comparisons
res_surv$comparison

res_fec <- fma_compare_models(
  d               = d_3.1_f2,
  variable        = "survival",
  aridity_formula = "~ aridity_centered",
  full_formula    = "~ aridity_centered + factor(C3_C4) + factor(native_invasive) + factor(annual_perennial)"
)

# View summary table of comparisons
res_fec$comparison

res_all <- fma_compare_models(
  d               = d_combined,
  variable        = "all",
  aridity_formula = "~ aridity_centered",
  full_formula    = "~ aridity_centered + factor(C3_C4) + factor(native_invasive) + factor(annual_perennial) + factor(demographic)"
)

# View summary table of comparisons
res_all$comparison

################################################################################
## 07. Compile results

# Format results into tables
fmt_growth <- fma_format_tables(res_growth, model_label = "Growth")
fmt_surv <- fma_format_tables(res_surv, model_label = "Survival")
fmt_fec <- fma_format_tables(res_fec, model_label = "Fecundity")
fmt_all <- fma_format_tables(res_all, model_label = "All")

# Compile into a single primary summary object
primary_summary <- fma_compile_primary_tables(list(
  "Growth" = fmt_growth,
  "Survival" = fmt_surv,
  "Fecundity" = fmt_fec,
  "All" = fmt_all
))

# Look at outputs
# Model Comparisons across all 3 rates:
print(primary_summary$comparison_table)

# Pooled Effect Sizes (Model 0) across all 3 rates:
print(primary_summary$baseline_null_table)

# Render interactive HTML/GT tables in RStudio Viewer:
if (!is.null(primary_summary$gt_comparison)) {
  print(primary_summary$gt_comparison)
  print(primary_summary$gt_baseline_null)
}

# Format the coefficient summary table for publication in the supplement
supp_table <- fma_format_supplement_table(primary_summary$coefficients_table)

# Format the data range summary table for publication in the supplement
demg_summary <- fma_summarize_dataset(d_3.1)

# Check how many unknown covariate data points there were for supplement reporting

d_combined %>%
  # Filter rows where at least one trait covariate is marked as "unknown"
  filter(
    C3_C4 == "unknown" | 
      native_invasive == "unknown" | 
      annual_perennial == "unknown"
  ) %>%
  # Summarize total effect sizes and unique study IDs
  summarise(
    n_effects_with_unknown = n(),
    n_studies_with_unknown = n_distinct(studyid)
  )

# Write final files including raw and formatted for publication
# Raw
write.csv(primary_summary$comparison_table, "Table1_model_comparisons.csv", row.names = FALSE)
write.csv(primary_summary$baseline_null_table, "Table2_baseline_effects.csv", row.names = FALSE)

# Formatted for publication
write.csv(demg_summary, "Table2_supp.csv", row.names = FALSE)
write.csv(supp_table, "Table3_supp.csv", row.names = FALSE)

################################################################################
## 08. Plot results

# Figure 2 - Intercepts from null model
fig_forest <- fma_plot_baseline_forest(primary_summary$baseline_null_table)
print(fig_forest)

# Figure 3 - Heterogeneity partitioning as measured by I2 values
fig_i2 <- fma_plot_i2_breakdown(primary_summary$baseline_null_table)
print(fig_i2)

# Figure 4 - Coefficients
fig_coefs <-fma_plot_moderator_coefficients(primary_summary$coefficients_table)
print(fig_coefs)

# Aridity scatter 
fig_aridity_growth <- fma_plot_aridity_regression(
  model_1        = res_growth$m1_aridity,
  raw_data       = d_3.1,
  aridity_col    = "aridity_centered",
  response_label = "Growth / Biomass"
)
print(fig_aridity_growth)

# Save high-resolution PNGs for  manuscript and supplement
ggplot2::ggsave("Fig2_null_model_intercept.png", fig_forest, width = 7, height = 4.5, dpi = 300)
ggplot2::ggsave("Fig3_I2.png", fig_i2, width = 7, height = 4.5, dpi = 300)
ggplot2::ggsave("Fig4_coef.png", fig_coefs, width = 10, height = 6.5, dpi = 300)
ggplot2::ggsave("FigX_aridity_growth.png", fig_aridity_growth, width = 7.5, height = 5, dpi = 300)

################################################################################
## References

# Used this guide for calculation and analysis references:
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html

# Function for loading packages modified from:
# https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

# Aridity derived from this publication:
# https://www.nature.com/articles/s41597-022-01493-1
