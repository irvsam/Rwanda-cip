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

# food takes a while to load because it is a large file (15,054 households × 148 items = 2,227,992 rows)
# so we select only what is
food <- read_dta(
  file.path(data_path, "EICV7/CS_S8B_Food_Expenditure_Consumption.dta"),
  col_select = c(hhid, s8bq0, matches("^s8bq(2|6|9)_v[2-5]$"))
)

# The following are just to see codings for the files
# print(val_labels(food$s8bq0))      # the 148 item names
# print(val_labels(food$s8bq6_v2))   # consumed: which code is yes?
# print(val_labels(food$s8bq2_v2))   # purchased: which code is yes?
# print(val_labels(food$s8bq9_v2))   # own-produced: which code is yes?
# print(val_labels(food$s8bq0))

message("eicv loaded")
