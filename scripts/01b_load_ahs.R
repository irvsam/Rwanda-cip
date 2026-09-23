# ============================================================
# 01b_load_ahs.R
# Load the raw AHS 2024 files used to build the master file.
#
#   ahs_s1  : household members (head characteristics, composition)
#   ahs_s2  : land tenure (total agricultural land, AHS weight)
#   ahs_s34 : crops grown, by plot and season (crop concentration)
#   ahs_s6  : extension services and programmes (proxy validation)
#
# Also checks that AHS households match EICV7 on hhid, and prints
# the value labels behind the codes used in 04. Read these before
# running 04 and correct the codes block there if needed.
# ============================================================

source("scripts/00_setup.R")
if (!exists("poverty_data")) source("scripts/01_load_eicv7.R")

ahs_dir <- file.path(data_path, "AHS 2024")

ahs_s1  <- read_dta(file.path(ahs_dir, "AHS2024_Section1_HOUSEHOLD MEMBERS CHARACTERISTICS.dta"))
ahs_s2  <- read_dta(file.path(ahs_dir, "AHS2024_Section2_LAND_TENURE.dta"))
ahs_s34 <- read_dta(file.path(ahs_dir, "AHS2024_Section3_4_CROP_GROWN__SEEDS_AND_PRODUCTION___AGRICULTURAL_INPUTS_AND_PRACTICES.dta"))
ahs_s6  <- read_dta(file.path(ahs_dir, "AHS2024_Section6_EXTENSION SERVICES AND AGRICULTURAL PROGRAMMES.dta"))

# ---- Join check: do AHS households appear in EICV7? --------
eicv_ids <- unique(as.numeric(zap_labels(poverty_data$hhid)))

join_check <- imap_dfr(
  list(s1 = ahs_s1, s2 = ahs_s2, s34 = ahs_s34, s6 = ahs_s6),
  function(df, name) {
    ids <- unique(as.numeric(zap_labels(df$hhid)))
    tibble(file = name, rows = nrow(df), households = length(ids),
           matched_in_eicv7 = sum(ids %in% eicv_ids))
  }
)
print(join_check)
# Expect roughly 3,700 households per file, nearly all matched.

# ---- Codes to confirm before running 04 --------------------
print(val_labels(ahs_s1$s1q2))    # relationship to head: which is "head"?
print(val_labels(ahs_s1$s1q1))    # sex: which is "female"?
print(val_labels(ahs_s1$s1q14))   # household membership: which codes are current members?
print(val_labels(ahs_s1$s4aq1))   # ever attended school: which is "yes"?
print(val_labels(ahs_s1$s4aq3))   # highest diploma: used to collapse education
print(val_labels(ahs_s34$Season)) # which code is Season A?
print(val_labels(ahs_s2$s2q2a))   # plot land use: what are 96 and 99?
print(val_labels(ahs_s6$s6q10))   # cooperative membership: which is "yes"?
print(val_labels(ahs_s34$s3_q4_1)) # crop codes: do they match the SAS list (101 maize etc.)?