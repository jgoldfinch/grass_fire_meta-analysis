# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 08/17/2026

# Function purpose: Compare meta-analysis models 
#                   (Null vs. Aridity vs. Full Model)

#################################### Intro #####################################

# Function name: fma_compare_models
# Description: Fits three nested multi-level meta-analysis models (Null, 
# Aridity-only, and Full model with plant traits) using both REML and ML. 
# Calculates goodness-of-fit (AIC), Likelihood Ratio Tests (LRT), and 
# variance explained (R2) across models.

################################# Arguments ####################################

# d:
#        Data frame containing effect size data and moderator variables.
# variable:
#        String indicating the demographic response outcome (e.g., "Growth", "Survival").
# aridity_formula:
#        String model formula for Model 1 (e.g., "~ aridity_centered").
# full_formula:
#        String model formula for Model 2 including aridity and plant traits.

################################## Output ######################################

# A named list containing:
#   - m0_null: Fitted REML Null model
#   - m1_aridity: Fitted REML Model 1 (Aridity)
#   - m2_full: Fitted REML Model 2 (Full Model)
#   - comparison: Summary tibble with AIC deltas, LRT statistics, and R2 values

################################# Function #####################################
fma_compare_models <- function(d, 
                                    variable = "growth_biomass", 
                                    aridity_formula = "~ aridity_centered", 
                                    full_formula = "~ aridity_centered + factor(C3_C4) + factor(native_invasive) + factor(annual_perennial)") {
  
  if (!requireNamespace("metafor", quietly = TRUE)) stop("Package 'metafor' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  
  # Ensure complete cases across all variables used in Model 2 to keep dataset k identical across comparisons
  full_vars <- all.vars(as.formula(full_formula))
  req_cols  <- c("g", "g_var", "studyid", "effect_id", full_vars)
  
  d_clean <- d %>% 
    dplyr::filter(dplyr::if_all(dplyr::all_of(req_cols), ~ !is.na(.)))
  
  k_count <- nrow(d_clean)
  
  # =========================================================================
  # 1. FIT REML MODELS (For Unbiased Variance Components & Parameters)
  # =========================================================================
  
  m0_reml <- metafor::rma.mv(
    yi = g, V = g_var, random = ~ 1 | studyid/effect_id,
    data = d_clean, method = "REML"
  )
  
  m1_reml <- metafor::rma.mv(
    yi = g, V = g_var, mods = as.formula(aridity_formula),
    random = ~ 1 | studyid/effect_id, data = d_clean, method = "REML"
  )
  
  m2_reml <- metafor::rma.mv(
    yi = g, V = g_var, mods = as.formula(full_formula),
    random = ~ 1 | studyid/effect_id, data = d_clean, method = "REML"
  )
  
  # Extract REML variance components (sigma2[1] = within, sigma2[2] = between)
  s2_within_0  <- m0_reml$sigma2[1]; s2_between_0 <- m0_reml$sigma2[2]
  s2_within_1  <- m1_reml$sigma2[1]; s2_between_1 <- m1_reml$sigma2[2]
  s2_within_2  <- m2_reml$sigma2[1]; s2_between_2 <- m2_reml$sigma2[2]
  
  # Calculate R2 (Variance Explained) relative to Model 0
  # Model 1 vs 0
  r2_between_m1 <- max(0, (s2_between_0 - s2_between_1) / s2_between_0) * 100
  r2_total_m1   <- max(0, ((s2_between_0 + s2_within_0) - (s2_between_1 + s2_within_1)) / (s2_between_0 + s2_within_0)) * 100
  
  # Model 2 vs 0
  r2_between_m2 <- max(0, (s2_between_0 - s2_between_2) / s2_between_0) * 100
  r2_total_m2   <- max(0, ((s2_between_0 + s2_within_0) - (s2_between_2 + s2_within_2)) / (s2_between_0 + s2_within_0)) * 100
  
  # Model 2 vs Model 1 (Incremental R2 added by Traits over Aridity)
  r2_between_m1_m2 <- max(0, (s2_between_1 - s2_between_2) / s2_between_1) * 100
  
  # =========================================================================
  # 2. FIT ML MODELS (Safely Handled)
  # =========================================================================
  
  m0_ml <- tryCatch(metafor::rma.mv(yi = yi_val, V = vi_val, random = rand_formula, data = d_clean, method = "ML"), error = function(e) NULL)
  m1_ml <- tryCatch(metafor::rma.mv(yi = yi_val, V = vi_val, mods = as.formula(aridity_formula), random = rand_formula, data = d_clean, method = "ML"), error = function(e) NULL)
  m2_ml <- tryCatch(metafor::rma.mv(yi = yi_val, V = vi_val, mods = as.formula(full_formula), random = rand_formula, data = d_clean, method = "ML"), error = function(e) NULL)
  
  # Safe check: verify models fit and optimizer code is strictly 0 (Success)
  m0_ok <- !is.null(m0_ml) && isTRUE(m0_ml$optinfo$val == 0)
  m1_ok <- !is.null(m1_ml) && isTRUE(m1_ml$optinfo$val == 0)
  m2_ok <- !is.null(m2_ml) && isTRUE(m2_ml$optinfo$val == 0)
  
  ml_success <- m0_ok && m1_ok && m2_ok
  
  if (ml_success) {
    aic0 <- m0_ml$fit.stats$ML[1]
    aic1 <- m1_ml$fit.stats$ML[1]
    aic2 <- m2_ml$fit.stats$ML[1]
    
    lrt_01 <- tryCatch(metafor::anova.rma.mv(m0_ml, m1_ml), error = function(e) list(LRT = NA, pval = NA))
    lrt_12 <- tryCatch(metafor::anova.rma.mv(m1_ml, m2_ml), error = function(e) list(LRT = NA, pval = NA))
    lrt_02 <- tryCatch(metafor::anova.rma.mv(m0_ml, m2_ml), error = function(e) list(LRT = NA, pval = NA))
  } else {
    warning("ML convergence failed or produced errors for one or more models. Returning REML-based AICs and skipping LRTs.")
    aic0 <- m0_reml$fit.stats$REML[1]
    aic1 <- m1_reml$fit.stats$REML[1]
    aic2 <- m2_reml$fit.stats$REML[1]
    
    lrt_01 <- lrt_12 <- lrt_02 <- list(LRT = NA, pval = NA)
  }
  
  # =========================================================================
  # 3. BUILD COMPARISON SUMMARY TABLE
  # =========================================================================
  
  comp_df <- tibble::tibble(
    Response_Variable       = variable,
    k_Effects               = k_count,
    
    # AIC Values & Deltas
    AIC_Model0_Null         = round(aic0, 2),
    AIC_Model1_Aridity      = round(aic1, 2),
    AIC_Model2_Full         = round(aic2, 2),
    
    Delta_AIC_M1_vs_M0      = round(aic1 - aic0, 2),
    Delta_AIC_M2_vs_M1      = round(aic2 - aic1, 2),
    Delta_AIC_M2_vs_M0      = round(aic2 - aic0, 2),
    
    # Likelihood Ratio Tests
    LRT_M1_vs_M0_chi2       = round(as.numeric(lrt_01$LRT), 2),
    LRT_M1_vs_M0_p          = ifelse(is.na(lrt_01$pval), "NA", sprintf("%.3f", lrt_01$pval)),
    
    LRT_M2_vs_M1_chi2       = round(as.numeric(lrt_12$LRT), 2),
    LRT_M2_vs_M1_p          = ifelse(is.na(lrt_12$pval), "NA", sprintf("%.3f", lrt_12$pval)),
    
    LRT_M2_vs_M0_chi2       = round(as.numeric(lrt_02$LRT), 2),
    LRT_M2_vs_M0_p          = ifelse(is.na(lrt_02$pval), "NA", sprintf("%.3f", lrt_02$pval)),
    
    # Variance Explained (R2)
    R2_Between_M1_vs_M0_pct = round(r2_between_m1, 1),
    R2_Total_M1_vs_M0_pct   = round(r2_total_m1, 1),
    
    R2_Between_M2_vs_M1_pct = round(r2_between_m1_m2, 1),
    
    R2_Between_M2_vs_M0_pct = round(r2_between_m2, 1),
    R2_Total_M2_vs_M0_pct   = round(r2_total_m2, 1)
  )
  
  return(list(
    m0_null    = m0_reml,
    m1_aridity = m1_reml,
    m2_full    = m2_reml,
    comparison = comp_df
  ))
}