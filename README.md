


<img width="960" height="768" alt="image" src="https://github.com/jgoldfinch/grass_fire_meta-analysis/blob/main/cullan-smith-BdTtvBRhOng-unsplash.jpg" />

*Photo credit: Cullen Smith, Unsplash*

# grass_fire_meta-analysis
A repository to recreate the analysis in the Golding et al. (in prep) publication examining the influence of fire on grass demographics, specifically in the context of aridity.

## Programs needed to run:
* R

## Code files needed to run:
* 00_fma_primary.R
* 01_fma_load_packages.R
* 02_fma_data_load_clean.R
* 03_fma_convert_spatial.R
* 04_fma_extract_aridity.R
* 05_fma_plot_traits.R
* 06_fma_model_compare.R
* 07_fma_format_result_tables.R
* 08_fma_compile_primary_tables.R
* 09_fma_format_supp_table.R
* 10_fma_format_supp_table_data.R
* 11_fma_plot_null_intercepts.R
* 12_fma_plot_I2.R
* 13_fma_plot_coef.R
* 14_fma_growth_aridity_scatter.R

## Data files needed to run:
* raw_data_final.csv (raw meta-analysis data)
*      Hosted on Zenodo; DOI 10.5281/zenodo.22880585

* ai_1.1.tif (aridity raster data)
*      Hosted on Zenodo; DOI 10.5281/zenodo.22880585

## Complete the following steps in this order:

1) Download all R code files and data files into a single location.

2) Open the *00_fma_primary.R* file (all R code will be run from this file and line numbers below refer to this file).

3) Set working directory (L12) to where you have downloaded R files and data files and load functions needed for analysis (L15-28).

4) Load, format, and clean the data (L34-37).

5) Turn the data into a spatial object and extract, scale, and append aridity values to the main data (L50-58).

6) Format data for analysis (L64-127).

7) Examine covariate combinations (L135-144).

8) Conduct meta-analysis and perform 3 model (null - model 0, aridity only - model 1, and full - model 2) comparison (L149-187).

9) Compile results from meta-analysis and write to file (L193-247).

10) Plot results and save high resolution graphs (L252-277).
