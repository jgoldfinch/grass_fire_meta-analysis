# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 08/02/2026

# Function purpose: Format output from fma_compare_models into publication-ready tables

#################################### Intro #####################################

# Function name: fma_format_3way_tables
# Description: Formats output objects from fma_compare_models_3way into 
# publication-ready summary tables, splitting results into model 
# comparisons, baseline null parameters, and moderator coefficients.

################################# Arguments ####################################

# res_3way:
#        Output list object returned by `fma_compare_models_3way()`.
# model_label:
#        Character string indicating the demographic outcome (e.g., "Growth / Biomass").

################################## Output ######################################

# A named list containing three formatted tibbles:
#   - model_comparison: Goodness-of-fit (AIC), LRT, and R2 summary table
#   - baseline_null: Baseline overall mean effect sizes across outcomes
#   - coefficients: Parameter estimates, SE, test statistics, and 95% CIs

################################# Function #####################################
fma_format_tables <- function(res_3way, model_label = "Demographic Response") {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("metafor", quietly = TRUE)) stop("Package 'metafor' is required.")
  
  # --- Vectorized Helpers ---
  fmt_vec <- function(val, digits = 3) {
    if (is.null(val)) return("NA")
    sapply(val, function(x) {
      if (is.null(x) || is.na(x)) "NA" else sprintf(paste0("%.", digits, "f"), x)
    })
  }
  
  fmt_p_vec <- function(p_val) {
    if (is.null(p_val)) return("NA")
    sapply(p_val, function(p) {
      if (is.null(p) || is.na(p)) return("NA")
      if (p < 0.001) return("< 0.001")
      sprintf("%.3f", as.numeric(p))
    })
  }
  
  # Extract components from the 3-way result list
  m0 <- res_3way$m0_null
  m1 <- res_3way$m1_aridity
  m2 <- res_3way$m2_full
  comp <- res_3way$comparison
  
  # Calculate I2 for Model 0 (Heterogeneity across level 2 within-study & level 3 between-study)
  # Sampling variance mean approximation for rma.mv I2
  wi <- 1 / m0$vi
  v_avg <- (m0$k - 1) / (sum(wi) - (sum(wi^2) / sum(wi)))
  i2_level2 <- (m0$sigma2[1] / (m0$sigma2[1] + m0$sigma2[2] + v_avg)) * 100
  i2_level3 <- (m0$sigma2[2] / (m0$sigma2[1] + m0$sigma2[2] + v_avg)) * 100
  i2_total  <- i2_level2 + i2_level3
  
  # =========================================================================
  # TABLE 1: Model Fit & Model Comparison Summary
  # =========================================================================
  
  t1_model_comp <- tibble::tibble(
    Response_Variable       = model_label,
    k_Effects               = m0$k,
    
    # AIC Values
    AIC_M0_Null             = fmt_vec(comp$AIC_Model0_Null, 2),
    AIC_M1_Aridity          = fmt_vec(comp$AIC_Model1_Aridity, 2),
    AIC_M2_Full             = fmt_vec(comp$AIC_Model2_Full, 2),
    
    # Deltas
    Delta_AIC_M1_vs_M0      = fmt_vec(comp$Delta_AIC_M1_vs_M0, 2),
    Delta_AIC_M2_vs_M1      = fmt_vec(comp$Delta_AIC_M2_vs_M1, 2),
    Delta_AIC_M2_vs_M0      = fmt_vec(comp$Delta_AIC_M2_vs_M0, 2),
    
    # Likelihood Ratio Tests
    LRT_M1_vs_M0_p          = fmt_p_vec(comp$LRT_M1_vs_M0_p),
    LRT_M2_vs_M1_p          = fmt_p_vec(comp$LRT_M2_vs_M1_p),
    LRT_M2_vs_M0_p          = fmt_p_vec(comp$LRT_M2_vs_M0_p),
    
    # Variance Explained (R2)
    R2_Between_M1_vs_M0_pct = fmt_vec(comp$R2_Between_M1_vs_M0_pct, 1),
    R2_Between_M2_vs_M1_pct = fmt_vec(comp$R2_Between_M2_vs_M1_pct, 1),
    R2_Between_M2_vs_M0_pct = fmt_vec(comp$R2_Between_M2_vs_M0_pct, 1)
  )
  
  # =========================================================================
  # TABLE 2: Baseline Null Model (Model 0) Pooled Effect & Heterogeneity
  # =========================================================================
  
  t2_baseline_null <- tibble::tibble(
    Response_Variable       = model_label,
    k_Effects               = m0$k,
    Null_Effect_Estimate   = fmt_vec(m0$b[1, 1], 3),
    SE                      = fmt_vec(m0$se[1], 3),
    CI_95_Formatted         = paste0("[", fmt_vec(m0$ci.lb[1], 3), ", ", fmt_vec(m0$ci.ub[1], 3), "]"),
    p_value                 = fmt_p_vec(m0$pval[1]),
    I2_Total_Percent        = fmt_vec(i2_total, 1),
    I2_Level2_Within_Study  = fmt_vec(i2_level2, 1),
    I2_Level3_Between_Study = fmt_vec(i2_level3, 1)
  )
  
  # =========================================================================
  # TABLE 3: Coefficients from Full Model (Model 2) & Aridity Model (Model 1)
  # =========================================================================
  
  # Helper to format coefficient table from a model
  extract_coef_table <- function(mod, model_name) {
    coef_names <- rownames(mod$b)
    tibble::tibble(
      Response_Variable = model_label,
      Model             = model_name,
      Predictor         = coef_names,
      Estimate          = round(as.numeric(mod$b), 3),
      SE                = round(as.numeric(mod$se), 3),
      z_val             = round(as.numeric(mod$zval), 3),
      p_val             = fmt_p_vec(mod$pval),
      CI_95_Lower       = round(as.numeric(mod$ci.lb), 3),
      CI_95_Upper       = round(as.numeric(mod$ci.ub), 3),
      CI_95_Formatted   = paste0("[", fmt_vec(mod$ci.lb, 3), ", ", fmt_vec(mod$ci.ub, 3), "]")
    )
  }
  
  t3_coefs_m1 <- extract_coef_table(m1, "Model 1 (Aridity)")
  t3_coefs_m2 <- extract_coef_table(m2, "Model 2 (Full)")
  
  t3_all_coefs <- dplyr::bind_rows(t3_coefs_m1, t3_coefs_m2)
  
  return(list(
    model_comparison = t1_model_comp,
    baseline_null    = t2_baseline_null,
    coefficients     = t3_all_coefs
  ))
}