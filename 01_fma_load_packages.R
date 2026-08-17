# Project: FMA
# Author: Jessie Golding
# Date: 05/22/2025

# Function purpose: load packages (check if installed and if not install them)

################################################################################
## Load required packages
# Function from https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
## First specify the packages of interest
packages = c("tidyverse", "here", "ggplot2", "raster", "sf", "mapview",
             "ggspatial", "prettymapr", "rnaturalearthdata", "rnaturalearth",
             "effsize", "metafor", "esc", "meta", "mice", "mitml", "terra")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
