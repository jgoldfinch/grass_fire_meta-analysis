# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 08/17/2026

# Function purpose: Summarize study counts, effect sizes, sample sizes, and 
# covariates across dataset groupings

#################################### Intro #####################################

# Function name: fma_summarize_dataset
# Description: Generates a comprehensive dataset summary table broken down by 
# a specified grouping variable (e.g., demographic categories). Calculates 
# unique study counts, total effect sizes, average effects per study, sample 
# sizes, Hedge's g ranges, and optional numeric covariate summaries.

################################# Arguments ####################################

# df:
#        Data frame containing the meta-analysis dataset (must include studyid, 
#        trt_n, ctrl_n, and g).
# group_var:
#        Character string specifying the column name to group by (default is 
#        "demg_resp_condensed").
# extra_numeric_vars:
#        Optional character vector of additional numeric column names to 
#        summarize as Mean [min, max].
# include_all_row:
#        Logical indicating whether to include a top row summarizing all 
#        combined observations (default is TRUE).

################################## Output ######################################

# out_table - A data frame containing summary statistics formatted with 
# Mean [min, max] ranges for sample sizes, effect sizes, and specified 
# covariates across group levels and total dataset.

################################# Function #####################################

fma_summarize_dataset <- function(df, 
                                  group_var = "demg_resp_condensed", 
                                  extra_numeric_vars = NULL,
                                  include_all_row = TRUE) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  
  # Helper function to collapse any subset into a single 1-row summary
  summarize_subframe <- function(sub_df, label_name) {
    res <- data.frame(
      Group_Label                       = as.character(label_name),
      `Number of studies`               = dplyr::n_distinct(sub_df$studyid),
      `Unique measured effects`          = nrow(sub_df),
      `Mean number of effects per study`  = sprintf("%.1f", nrow(sub_df) / dplyr::n_distinct(sub_df$studyid)),
      
      # Updated Treatment Sample Size: Mean [min, max]
      `Treatment sample size Mean [min, max]` = sprintf(
        "%.1f [%d, %d]",
        mean(sub_df$trt_n, na.rm = TRUE),
        min(sub_df$trt_n, na.rm = TRUE),
        max(sub_df$trt_n, na.rm = TRUE)
      ),
      
      # Updated Control Sample Size: Mean [min, max]
      `Control sample size Mean [min, max]`   = sprintf(
        "%.1f [%d, %d]",
        mean(sub_df$ctrl_n, na.rm = TRUE),
        min(sub_df$ctrl_n, na.rm = TRUE),
        max(sub_df$ctrl_n, na.rm = TRUE)
      ),
      
      # Hedge's g: Mean [min, max]
      `Hedge's g Mean [min, max]`         = sprintf(
        "%.2f [%.1f, %.1f]", 
        mean(sub_df$g, na.rm = TRUE), 
        min(sub_df$g, na.rm = TRUE), 
        max(sub_df$g, na.rm = TRUE)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Process optional extra numeric variables if supplied
    if (!is.null(extra_numeric_vars)) {
      for (var_name in extra_numeric_vars) {
        if (var_name %in% colnames(sub_df)) {
          vals <- sub_df[[var_name]]
          col_label <- paste0(var_name, " Mean [min, max]")
          res[[col_label]] <- sprintf(
            "%.2f [%.1f, %.1f]", 
            mean(vals, na.rm = TRUE), 
            min(vals, na.rm = TRUE), 
            max(vals, na.rm = TRUE)
          )
        }
      }
    }
    
    return(res)
  }
  
  # Clean grouping column values
  df_clean <- df
  df_clean$Group_Var_Clean <- stringr::str_to_title(stringr::str_trim(as.character(df_clean[[group_var]])))
  
  # 1. Split data by unique group levels and collapse each into 1 row
  split_list <- split(df_clean, df_clean$Group_Var_Clean)
  group_summaries <- do.call(rbind, lapply(names(split_list), function(g_name) {
    summarize_subframe(split_list[[g_name]], g_name)
  }))
  
  # 2. Add "All demographic responses" row if requested
  if (include_all_row) {
    all_summary <- summarize_subframe(df_clean, "All demographic responses")
    out_table <- rbind(all_summary, group_summaries)
  } else {
    out_table <- group_summaries
  }
  
  # Set header column label
  header_label <- if (group_var == "demg_resp_condensed") "Demographic category" else group_var
  colnames(out_table)[colnames(out_table) == "Group_Label"] <- header_label
  rownames(out_table) <- NULL
  
  return(out_table)
}