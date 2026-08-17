# Project: FMA
# Author: Jessie Golding and Claude AI
# Date: 08/14/2026

# Function purpose: load and clean data

#################################### Intro #####################################

# Function name: fma_load_clean_data
# Description:  function to load and clean data for FMA project

################################# Arguments ####################################

# d:
#       Data file (csv) with raw data for meta-analsysi

################################## Output ######################################

# d_1.0 - rowws_df saved to global environment that ontains cleaned data 
# responses plus calculated standardized mean effects, Hedges' g, correction factor

################################## Function #####################################
fma_load_clean_data <- function(d) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("esc", quietly = TRUE)) stop("Package 'esc' is required.")
  
  # Step 1: Initial preparation and conversion to numeric
  d_prep <- d %>%
    dplyr::select(number, authors, title, fire_intensity, fire_severity, fire_season,
                  year_of_fire, years_since_last_fire, historical_fire_regime, fire_treatment,
                  sp_or_group, species1, species2, functional_group, C3_C4, annual_perennial, native_invasive,
                  demg_resp_condensed, specific_demographic_response, treatment_group_mean,
                  treatment_group_variance_standardized_SD,
                  treatment_group_variance_standardized_SE, treatment_group_sample_size_n, control_group_mean,
                  control_group_variance_standardized_SD, control_group_variance_standardized_SE,
                  control_group_sample_size_n, years_of_study,
                  country, latitude, longitude, mean_annual_precip, soil_type,
                  habitat_type, mean_annual_high_temp, mean_annual_low_temp) %>%
    dplyr::rename(studyid = number) %>%
    dplyr::mutate(
      t_mean = as.numeric(treatment_group_mean),
      c_mean = as.numeric(control_group_mean),
      t_sd   = as.numeric(treatment_group_variance_standardized_SD),
      c_sd   = as.numeric(control_group_variance_standardized_SD),
      t_n    = as.numeric(treatment_group_sample_size_n),
      c_n    = as.numeric(control_group_sample_size_n),
      
      # Flags for zero/missing values
      t_mean_zero = is.na(t_mean) | t_mean == 0,
      c_mean_zero = is.na(c_mean) | c_mean == 0,
      t_sd_zero   = is.na(t_sd)   | t_sd == 0,
      c_sd_zero   = is.na(c_sd)   | c_sd == 0,
      
      # Master flag to identify rows that must be filtered out
      is_excluded = t_mean_zero | c_mean_zero | t_sd_zero | c_sd_zero | is.na(t_n) | is.na(c_n) | t_n <= 0 | c_n <= 0
    )
  
  # Step 2: Extract and print summary table of filtered rows
  filtered_data <- d_prep %>% dplyr::filter(is_excluded)
  num_filtered  <- nrow(filtered_data)
  
  cat("\n=========================================================\n")
  cat(" FILTERED STUDIES SUMMARY (ZERO / MISSING MEAN OR SD)\n")
  cat("=========================================================\n")
  cat("Total effect sizes filtered out:", num_filtered, "out of", nrow(d_prep), "\n\n")
  
  if (num_filtered > 0) {
    cat("Excluded observations and details:\n")
    summary_table <- filtered_data %>%
      dplyr::select(studyid, sp_or_group, demg_resp_condensed, t_mean, t_sd, t_n, c_mean, c_sd, c_n) %>%
      dplyr::mutate(
        Exclusion_Reason = dplyr::case_when(
          (t_sd == 0 | is.na(t_sd)) & (c_sd == 0 | is.na(c_sd)) ~ "Zero/NA SD in Both Groups",
          t_sd == 0 | is.na(t_sd)                               ~ "Zero/NA Treatment SD",
          c_sd == 0 | is.na(c_sd)                               ~ "Zero/NA Control SD",
          t_mean == 0 | is.na(t_mean)                           ~ "Zero/NA Treatment Mean",
          c_mean == 0 | is.na(c_mean)                           ~ "Zero/NA Control Mean",
          TRUE                                                  ~ "Invalid Sample Size"
        )
      )
    print(as.data.frame(summary_table), row.names = FALSE)
    
    unique_studies <- unique(filtered_data$studyid)
    cat("\nUnique Study IDs affected (", length(unique_studies), " total): ",
        paste(unique_studies, collapse = ", "), "\n", sep = "")
  } else {
    cat("No effect sizes were excluded for zero or missing values.\n")
  }
  cat("=========================================================\n\n")
  
  # Step 3: Retain only valid rows
  d_valid <- d_prep %>% dplyr::filter(!is_excluded)
  
  # Step 4: Compute effect sizes (Hedges' g)
  d_clean <- d_valid %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      esc_result = list(esc::esc_mean_sd(
        grp1m   = t_mean,
        grp1sd  = t_sd,
        grp1n   = t_n,
        grp2m   = c_mean,
        grp2sd  = c_sd,
        grp2n   = c_n,
        es.type = "d"
      )),
      
      SMD     = esc_result$es,
      SMD_var = esc_result$var,
      SMD_se  = esc_result$se,
      
      df = (t_n + c_n) - 2,
      J  = 1 - (3 / (4 * df - 1)),
      
      g     = SMD * J,
      g_var = SMD_var * J^2,
      g_se  = SMD_se  * J,
      
      total_n = t_n + c_n
    ) %>%
    dplyr::select(-esc_result, -t_mean, -c_mean, -t_sd, -c_sd, -t_n, -c_n,
                  -t_mean_zero, -c_mean_zero, -t_sd_zero, -c_sd_zero, -is_excluded)
  
  # Step 5: Save to Global Environment
  biodata <- list("d_1.0" = d_clean)
  list2env(biodata, .GlobalEnv)
  
  message(paste0("Data successfully loaded! Retained ", nrow(d_clean), " total valid effect sizes in 'd_1.0'."))
}