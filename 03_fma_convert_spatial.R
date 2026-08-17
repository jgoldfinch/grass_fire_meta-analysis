# Project: FMA
# Author: Jessie Golding and Gemini
# Date: 07/28/2026

# Function purpose: Convert spatial coordinates to SF object and inspect/count 
# unique locations

#################################### Intro #####################################

# Function name: fma_spatial_conversion
# Description: function to convert demographic data with lat/lon into an `sf` 
# spatial object, optionally plot an interactive mapview map, and print the 
# count of unique geographic points.

################################# Arguments ####################################

# data:
#       Data frame/tibble containing latitude, longitude, country, and studyid.
# lon_col:
#       Name of the longitude column (default is "longitude").
# lat_col:
#       Name of the latitude column (default is "latitude").
# show_map:
#       Logical (TRUE/FALSE). If TRUE, displays an interactive mapview map. 
#       Default TRUE.

################################## Output ######################################

# d_sp - An `sf` spatial point object saved to global environment.

################################# Function #####################################
fma_spatial_conversion <- function(data, 
                                   lon_col = "longitude", 
                                   lat_col = "latitude", 
                                   show_map = TRUE) {
  
  # --- 1. Filter out missing coordinates to avoid sf errors ---
  data_clean <- data %>%
    dplyr::filter(!is.na(.data[[lon_col]]), !is.na(.data[[lat_col]]))
  
  # --- 2. Convert to Spatial sf object (WGS84 EPSG:4326) ---
  d_sp <- data_clean %>%
    dplyr::select(studyid, country, dplyr::all_of(c(lat_col, lon_col))) %>%
    sf::st_as_sf(
      coords = c(lon_col, lat_col),
      crs = 4326  # Standard WGS84 shorthand
    )
  
  # --- 3. Count unique coordinate locations ---
  n_unique_pts <- sf::st_coordinates(d_sp) %>% 
    as.data.frame() %>% 
    dplyr::distinct() %>% 
    nrow()
  
  cat("--------------------------------------------------\n")
  cat("Total spatial records:", nrow(d_sp), "\n")
  cat("Number of unique geographic locations:", n_unique_pts, "\n")
  cat("--------------------------------------------------\n")
  
  # --- 4. Interactive mapview check (Optional) ---
  if (show_map) {
    if (requireNamespace("mapview", quietly = TRUE)) {
      print(mapview::mapview(d_sp))
    } else {
      warning("Package 'mapview' is not installed. Skipping interactive plot.")
    }
  }
  
  return(d_sp)
}