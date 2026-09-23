# ============================================================
# 01_load_eicv7.R
# Load the raw EICV7 files
#
#   poverty_data  : base of the master file (outcome, adult
#                   equivalents, price deflator, household size,
#                   weights, geography)
#   expenditure_C : S8A3, for the own-production mechanism test
#                   in 05_main_models.R
# ============================================================

source("scripts/00_setup.R")

poverty_data  <- read_dta(file.path(data_path, "EICV7/CS_EICV7_poverty_file.dta"))
expenditure_C <- read_dta(file.path(data_path, "EICV7/CS_S8A3_Expenditure.dta"))