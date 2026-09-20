# ============================================================
# run_all.R
# Runs the full pipeline in order.
# 02_explore_codebook.R is excluded -- personal exploration only.

# ============================================================

source("scripts/00_setup.R")
source("scripts/01_load_eicv7.R")
source("scripts/03_build_luc.R")
source("scripts/04_build_analysis_data.R")
source("scripts/04b_descriptives.R")
source("scripts/04c_collinearity_check.R")
source("scripts/05_main_models.R")
source("scripts/06_household_crop_concentration.R")
source("scripts/07_figures_maps.R")

message("All scripts executed successfully. Check the output folder for results and figures.")