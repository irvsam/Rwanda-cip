# ============================================================
# 01a_load_eicv7.R
# Load the raw EICV7 (2023/24, cross-sectional sample) files.
#
#   poverty_data : base of the master file (food aggregate, adult
#                  equivalents, price deflator hh_index, household
#                  size, weights, geography)
#   food         : S8B food consumption module, one row per
#                  household x item (15,054 x 148). Only the columns
#                  used in 04b are read (ID, item, and the consumed /
#                  purchased / own-produced flags for visits 2-5),
#                  which cuts memory use by ~80%.
# ============================================================

if (!exists(".setup_done")) source("scripts/00_setup.R")

poverty_data <- read_dta(file.path(data_path, "EICV7/CS_EICV7_poverty_file.dta"))

food <- read_dta(
  file.path(data_path, "EICV7/CS_S8B_Food_Expenditure_Consumption.dta"),
  col_select = c(hhid, s8bq0, matches("^s8bq(2|6|9)_v[2-5]$"))
)

if (isTRUE(CHECK_LABELS)) {
  print(val_labels(food$s8bq0))      # the 148 item names
  print(val_labels(food$s8bq6_v2))   # consumed: 1 = Yes
  print(val_labels(food$s8bq2_v2))   # purchased: 1 = Yes
  print(val_labels(food$s8bq9_v2))   # own-produced: 1 = Yes
}

message("EICV7 loaded: ", nrow(poverty_data), " households")