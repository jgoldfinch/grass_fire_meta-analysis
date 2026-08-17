# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 08/02/2026

# Function purpose: Compile and format multiple demographic results into primary tables

#################################### Intro #####################################

# Function name: fma_compile_primary_tables
# Description: Compiles formatted output tables from multiple demographic 
# response models (e.g., Growth, Survival, Fecundity) into primary summary 
# data frames and publication-ready gt tables.

################################# Arguments ####################################

# formatted_list:
#        A named list of outputs from `fma_format_tables()`. 
#        E.g., list(Growth = formatted_growth, Survival = formatted_survival, Fecundity = formatted_fecundity).

################################## Output ######################################

# A named list containing:
#   - primary_comparison_table: Combined model fit and variance explained data frame
#   - primary_coefficients_table: Combined moderator estimates and 95% CIs
#   - primary_null_table: Combined baseline null estimates
#   - gt_tables: Formatted 'gt' table objects for presentation (if 'gt' is installed)

################################# Function #####################################
fma_compile_primary_tables <- function(formatted_list) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  
  # Ensure input is a named list
  if (is.null(names(formatted_list)) || any(names(formatted_list) == "")) {
    stop("Please provide a named list of formatted outputs. E.g., list('Growth/Biomass' = formatted_growth, 'Survival' = formatted_survival)")
  }
  
  # --- 1. Combine Comparison Tables ---
  primary_comp <- dplyr::bind_rows(
    lapply(names(formatted_list), function(nm) {
      df <- formatted_list[[nm]]$model_comparison
      df$Response_Variable <- nm
      return(df)
    })
  )
  
  # --- 2. Combine Baseline Null Tables ---
  primary_null <- dplyr::bind_rows(
    lapply(names(formatted_list), function(nm) {
      df <- formatted_list[[nm]]$baseline_null
      df$Response_Variable <- nm
      return(df)
    })
  )
  
  # --- 3. Combine Coefficient Tables ---
  primary_coefs <- dplyr::bind_rows(
    lapply(names(formatted_list), function(nm) {
      df <- formatted_list[[nm]]$coefficients
      df$Response_Variable <- nm
      return(df)
    })
  )
  
  # --- ADDED: ROUND ALL NUMERIC COLUMNS TO 2 DECIMAL PLACES IN DATA FRAMES ---
  # (Note: 'k_Effects' is preserved as an integer)
  primary_comp <- primary_comp %>% 
    dplyr::mutate(dplyr::across(where(is.numeric) & !c(k_Effects), ~ round(.x, 2)))
  
  primary_null <- primary_null %>% 
    dplyr::mutate(dplyr::across(where(is.numeric) & !c(k_Effects), ~ round(.x, 2)))
  
  primary_coefs <- primary_coefs %>% 
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  
  # --- 4. Optional GT Formatting for Pretty Display in RStudio / HTML / Word ---
  gt_comp <- NULL
  gt_null <- NULL
  
  if (requireNamespace("gt", quietly = TRUE)) {
    
    # Format Table 1 with gt
    gt_comp <- primary_comp %>% 
      gt::gt(groupname_col = "Response_Variable") %>% 
      gt::tab_header(
        title = "Table 1. Model Comparison across Demographic Outcomes",
        subtitle = "Comparing Model 0 (Null), Model 1 (Aridity), and Model 2 (Full Model)"
      ) %>% 
      gt::cols_label(
        k_Effects = "k",
        AIC_M0_Null = "Model 0 AIC",
        AIC_M1_Aridity = "Model 1 AIC",
        AIC_M2_Full = "Model 2 AIC",
        Delta_AIC_M1_vs_M0 = "ΔAIC (M1-M0)",
        Delta_AIC_M2_vs_M1 = "ΔAIC (M2-M1)",
        Delta_AIC_M2_vs_M0 = "ΔAIC (M2-M0)",
        LRT_M1_vs_M0_p = "p (M1 vs M0)",
        LRT_M2_vs_M1_p = "p (M2 vs M1)",
        LRT_M2_vs_M0_p = "p (M2 vs M0)",
        R2_Between_M1_vs_M0_pct = "R² M1 (%)",
        R2_Between_M2_vs_M1_pct = "R² M2-M1 (%)",
        R2_Between_M2_vs_M0_pct = "R² Full (%)"
      ) %>% 
      gt::fmt_number(columns = dplyr::where(is.numeric) & !c(k_Effects), decimals = 2) %>% # ADDED
      gt::tab_options(table.font.size = gt::px(12))
    
    # Format Table 2 with gt
    gt_null <- primary_null %>% 
      gt::gt() %>% 
      gt::tab_header(
        title = "Table 2. Baseline Pooled Fire Effects (Model 0) & Heterogeneity",
        subtitle = "3-Level Meta-Analysis Intercept-Only Models"
      ) %>% 
      gt::cols_label(
        Response_Variable = "Demographic Outcome",
        k_Effects = "k",
        Null_Effect_Estimate = "Hedges' g",
        SE = "SE",
        CI_95_Formatted = "95% CI",
        p_value = "p-value",
        I2_Total_Percent = "Total I² (%)",
        I2_Level2_Within_Study = "Level 2 / Within (%)",
        I2_Level3_Between_Study = "Level 3 / Between (%)"
      ) %>% 
      gt::fmt_number(columns = dplyr::where(is.numeric) & !c(k_Effects), decimals = 2) %>% # ADDED
      gt::tab_options(table.font.size = gt::px(12))
  }
  
  return(list(
    comparison_table = primary_comp,
    baseline_null_table = primary_null,
    coefficients_table = primary_coefs,
    gt_comparison = gt_comp,
    gt_baseline_null = gt_null
  ))
}