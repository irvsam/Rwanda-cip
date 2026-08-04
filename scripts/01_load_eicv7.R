# ============================================================
# 01_load_eicv7.R
# Load raw EICV7 files.
# ============================================================

source("scripts/00_setup.R")

hh_data       <- read_dta(file.path(data_path, "EICV7/CS_S01_S5_S7_Household.dta"))
poverty_data  <- read_dta(file.path(data_path, "EICV7/CS_EICV7_poverty_file.dta"))
savings_data  <- read_dta(file.path(data_path, "EICV7/CS_S10C_Savings.dta"))
credits_data  <- read_dta(file.path(data_path, "EICV7/CS_S10A1_A2_credits.dta"))
expenditure_A <- read_dta(file.path(data_path, "EICV7/CS_S8A1_Expenditure.dta"))
expenditure_B <- read_dta(file.path(data_path, "EICV7/CS_S8A2_Expenditure.dta"))
expenditure_C <- read_dta(file.path(data_path, "EICV7/CS_S8A3_Expenditure.dta"))

