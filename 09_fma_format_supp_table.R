# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 08/17/2026

# Function purpose: Format summary coefficient table for supplementary materials

#################################### Intro #####################################

# Function name: fma_format_supplement_table
# Description: Cleans, labels, and formats model coefficient estimates from 
# primary_summary$coefficients_table with explicit reference levels, combined 
# 95% confidence intervals, formatted numeric columns, and significance 
# indicators for pasting directly into supplementary tables.

################################# Arguments ####################################

# coef_df:
#        Data frame of coefficient summary statistics (e.g., 
#        primary_summary$coefficients_table).

################################## Output ######################################

# formatted_df - A tibble formatted for publication/supplementary layout with
# columns: Demographic Response, Model, Predictor, Estimate [95% CI], SE, 
# z-score, and p-value.

################################# Function #####################################
fma_format_supplement_table <- function(coef_df) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  
  # Define custom formatting for predictors and their explicit reference levels
  formatted_df <- coef_df %>%
    dplyr::mutate(
      # 1. Clean Model Names
      Model_Formatted = dplyr::case_when(
        stringr::str_detect(Model, "Model 1") ~ "Aridity only (Model 1)",
        stringr::str_detect(Model, "Model 2") ~ "Full model (Model 2)",
        TRUE ~ Model
      ),
      
      # 2. Clean Predictor Labels & Reference Levels
      Predictor_Formatted = dplyr::case_when(
        stringr::str_detect(Predictor, "intrcpt") ~ 
          "Mean effect of fire (intercept; \u03B20)",
        
        stringr::str_detect(Predictor, "aridity") ~ 
          "Aridity (\u03B21)",
        
        stringr::str_detect(Predictor, "C3_C4") & stringr::str_detect(Predictor, "C4") ~ 
          "Photosynthetic pathway (\u03B22; reference level C3)",
        stringr::str_detect(Predictor, "C3_C4") & stringr::str_detect(Predictor, "unknown") ~ 
          "Photosynthetic pathway: unknown (\u03B23; reference level C3)",
        
        stringr::str_detect(Predictor, "native_invasive") & stringr::str_detect(Predictor, "native") ~ 
          "Native/invasive (\u03B24; reference level invasive)",
        stringr::str_detect(Predictor, "native_invasive") & stringr::str_detect(Predictor, "unknown") ~ 
          "Native/invasive: unknown (\u03B25; reference level invasive)",
        
        stringr::str_detect(Predictor, "annual_perennial") & stringr::str_detect(Predictor, "perennial") ~ 
          "Annual/perennial (\u03B26; reference level annual)",
        stringr::str_detect(Predictor, "annual_perennial") & stringr::str_detect(Predictor, "unknown") ~ 
          "Annual/perennial: unknown (\u03B27; reference level annual)",
        
        TRUE ~ Predictor
      ),
      
      # 3. Combine Estimate and 95% CI into a single column
      Estimate_CI = sprintf("%.2f [%.2f, %.2f]", Estimate, CI_95_Lower, CI_95_Upper),
      
      # 4. Format Numeric Columns for direct pasting
      SE_Formatted      = sprintf("%.2f", SE),
      z_val_Formatted   = sprintf("%.2f", z_val),
      
      # Add significance asterisk if p < 0.05
      p_val_num         = as.numeric(p_val),
      p_val_Formatted   = dplyr::if_else(
        !is.na(p_val_num) & p_val_num < 0.05,
        paste0(sprintf("%.3f", p_val_num), "*"),
        sprintf("%.3f", p_val_num)
      )
    ) %>%
    
    # Select and order columns matching your supplement structure
    dplyr::select(
      `Demographic Response` = Response_Variable,
      `Model`                = Model_Formatted,
      `Predictor`            = Predictor_Formatted,
      `Estimate [95% CI]`   = Estimate_CI,
      `SE`                   = SE_Formatted,
      `z-score`              = z_val_Formatted,
      `p-value`              = p_val_Formatted
    )
  
  return(formatted_df)
}