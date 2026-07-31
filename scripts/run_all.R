# ============================================================
# run_all.R
# Runs the full pipeline in order. 02 is  excluded it is just for exploration
# ============================================================

source("scripts/00_setup.R")
source("scripts/01_load_eicv7.R")
source("scripts/03_build_luc.R")
source("scripts/04_build_analysis_data.R")
source("scripts/05_main_models.R")
source("scripts/06_robustness_appendix.R")
source("scripts/07_figures_maps.R")