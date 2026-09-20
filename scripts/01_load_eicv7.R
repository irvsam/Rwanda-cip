# ============================================================
# 01_load_eicv7.R
# Load raw EICV7 files actually used downstream.
#
# Only expenditure_C (S8A3) is used, for the own-production
# mechanism test in 05_main_models.R. 
# ============================================================

source("scripts/00_setup.R")

hh_data       <- read_dta(file.path(data_path, "EICV7/CS_S01_S5_S7_Household.dta"))
poverty_data  <- read_dta(file.path(data_path, "EICV7/CS_EICV7_poverty_file.dta"))
expenditure_C <- read_dta(file.path(data_path, "EICV7/CS_S8A3_Expenditure.dta"))