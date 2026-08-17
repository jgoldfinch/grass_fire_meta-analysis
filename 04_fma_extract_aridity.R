# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 08/14/2026

# Function purpose: Extract raster aridity values, categorize climate zones, 
#                   center/scale aridity values, plot summary, and merge.

#################################### Intro #####################################

# Function name: fma_extract_aridity
# Description: Function to extract raster aridity index values at spatial point 
# locations, classify points into UNEP aridity categories, generate a study-level 
# summary histogram, compute mean-centered and standardized z-score aridity variables, 
# and output the updated data frame.

################################# Arguments ####################################

# data_sp:
#       Spatial points object (`sf` object, e.g., d_1.0_sp).
# data_orig:
#       Original tabular data frame (e.g., d_1.0).
# raster_path:
#       File path to the aridity raster (.tif).
# show_plot:
#       Logical (TRUE/FALSE). If TRUE, displays the aridity summary histogram. 
#       Default TRUE.

################################## Output ######################################

# d_3.0 - A tibble/data frame containing all original data plus raw aridity, 
#         aridity categories, `aridity_centered`, and `aridity_scaled`.

################################# Function #####################################
fma_extract_aridity <- function(data_sp, 
                                data_orig, 
                                raster_path, 
                                join_keys = c("studyid", "latitude", "longitude"),
                                show_plot = TRUE) {
  
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  
  # --- 1. Load raster ---
  if (!file.exists(raster_path)) stop("Raster file not found at path: ", raster_path)
  ai_raster <- terra::rast(raster_path)
  
  # --- 2. Extract raster values at spatial points ---
  data_sp_reproj <- sf::st_transform(data_sp, terra::crs(ai_raster))
  extracted_df   <- terra::extract(ai_raster, terra::vect(data_sp_reproj))
  
  aridity_vec <- extracted_df[[2]] 
  
  # --- 2b. Fallback: Fix 0 or NA values (Coastal/Ocean Mask Mismatches) ---
  bad_indices <- which(is.na(aridity_vec) | aridity_vec == 0)
  
  if (length(bad_indices) > 0) {
    message(paste0("Found ", length(bad_indices), " point(s) landing on ocean/water mask. Extracting nearest land pixel..."))
    
    # Crop raster locally around problematic points to keep spatial lookup fast
    bad_pts_sf <- data_sp_reproj[bad_indices, ]
    pt_ext     <- terra::ext(terra::vect(bad_pts_sf)) + 1 # 1-degree buffer
    raster_sub <- terra::crop(ai_raster, pt_ext)
    
    # Extract land points (> 0)
    land_cells <- terra::as.points(raster_sub, values = TRUE, na.rm = TRUE)
    land_sf    <- sf::st_as_sf(land_cells)
    raster_col <- names(land_sf)[1]
    land_sf    <- land_sf %>% dplyr::filter(.data[[raster_col]] > 0)
    
    # Find geographically nearest land cell
    nearest_idx  <- sf::st_nearest_feature(bad_pts_sf, land_sf)
    nearest_vals <- land_sf[[raster_col]][nearest_idx]
    
    # Replace zeros/NAs with nearest land values
    aridity_vec[bad_indices] <- nearest_vals
  }
  
  # --- 3. Attach aridity and classify climate categories safely ---
  data_sp_classified <- data_sp
  data_sp_classified$aridity <- aridity_vec
  
  data_sp_classified <- data_sp_classified %>%
    dplyr::mutate(
      aridity_category = dplyr::case_when(
        aridity > 0    & aridity <= 0.03 ~ "Hyper arid",
        aridity > 0.03 & aridity <= 0.20 ~ "Arid",
        aridity > 0.20 & aridity <= 0.50 ~ "Semi-arid",
        aridity > 0.50 & aridity <= 0.65 ~ "Dry sub-humid",
        aridity > 0.65                   ~ "Humid",
        TRUE                             ~ NA_character_
      ),
      aridity_category = factor(
        aridity_category, 
        levels = c("Hyper arid", "Arid", "Semi-arid", "Dry sub-humid", "Humid")
      )
    )
  
  # --- 4. Plotting & Summary Histogram ---
  if (show_plot) {
    cat_colors <- c(
      "Hyper arid"    = "#CF7563",
      "Arid"          = "#E09053", 
      "Semi-arid"     = "#FAE039", 
      "Dry sub-humid" = "#D2FA32", 
      "Humid"         = "#3D5894"
    )
    
    plot_data <- data_sp_classified %>%
      dplyr::group_by(studyid) %>%
      dplyr::summarise(mean_aridity = mean(aridity, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        mean_cat = dplyr::case_when(
          mean_aridity <= 0.03                        ~ "Hyper arid",
          mean_aridity > 0.03 & mean_aridity <= 0.20 ~ "Arid",
          mean_aridity > 0.20 & mean_aridity <= 0.50 ~ "Semi-arid",
          mean_aridity > 0.50 & mean_aridity <= 0.65 ~ "Dry sub-humid",
          mean_aridity > 0.65                        ~ "Humid"
        ),
        mean_cat = factor(mean_cat, levels = names(cat_colors))
      )
    
    hist_p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = mean_aridity, fill = mean_cat)) +
      ggplot2::geom_histogram(color = "#000000", binwidth = 0.04) +
      ggplot2::scale_fill_manual(values = cat_colors, name = "Aridity category", drop = FALSE) +
      ggplot2::labs(
        x = "Annual mean aridity (per study)",
        y = "Count of studies",
        title = "Study Distribution Across Aridity Zones"
      ) +
      ggplot2::theme_classic()
    
    print(hist_p)
    ggplot2::ggsave("FigX_aridity_histogram.png", hist_p, width = 7, height = 4.5, dpi = 300)
  }
  
  # --- 5. Merge lookup back to data_orig ---
  data_sp_coords <- data_sp_classified
  
  if ("latitude" %in% join_keys || "longitude" %in% join_keys) {
    coords <- sf::st_coordinates(data_sp_coords)
    if (!"longitude" %in% colnames(data_sp_coords)) data_sp_coords$longitude <- coords[, 1]
    if (!"latitude" %in% colnames(data_sp_coords))  data_sp_coords$latitude  <- coords[, 2]
  }
  
  aridity_lookup <- data_sp_coords %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(join_keys), aridity, aridity_category) %>%
    dplyr::distinct()
  
  d_out <- data_orig %>%
    dplyr::left_join(aridity_lookup, by = join_keys)
  
  # --- 6. Center and Scale Aridity (Preserving NA safely) ---
  valid_aridity <- d_out$aridity[!is.na(d_out$aridity)]
  
  if (length(valid_aridity) == 0) {
    warning("ALL extracted aridity values are NA!")
    d_out$aridity_centered <- NA_real_
    d_out$aridity_scaled   <- NA_real_
  } else {
    mean_val <- mean(d_out$aridity, na.rm = TRUE)
    sd_val   <- sd(d_out$aridity, na.rm = TRUE)
    
    d_out <- d_out %>%
      dplyr::mutate(
        aridity_centered = aridity - mean_val,
        aridity_scaled   = dplyr::if_else(
          is.na(aridity), 
          NA_real_, 
          if (!is.na(sd_val) && sd_val > 0) (aridity - mean_val) / sd_val else 0
        )
      )
  }
  
  return(d_out)
}
