# ============================================================
# 04a_build_analysis_data.R
# One row per household. Builds every variable needed for the
# district-level analysis (05_main_models.R).

# ============================================================

source("scripts/00_setup.R")
if (!exists("hh_data")) source("scripts/01_load_eicv7.R")
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

# TODO: could add household size and household-head sex/age controls 

analysis_data <- hh_data %>%
  mutate(
    district_code = as.numeric(district),
    quintile_f    = as.factor(as.numeric(quintile)),   # H2 stratifier / control
    ur_f          = as.factor(as.numeric(ur)),
    province_f    = as.factor(as.numeric(province))    # province fixed effects
  ) %>%
  left_join(poverty_data %>% select(hhid, food, ae, member), by = "hhid") %>%
  left_join(dist_luc, by = "district_code") %>%
  filter(!is.na(food), !is.na(luc_intensity)) %>%
  mutate(log_food_ae = log(food))

count(analysis_data, quintile_f)

saveRDS(analysis_data, file.path(processed_path, "analysis_data.rds"))

# ---- Refined version: just the columns needed for modeling ----
analysis_data_refined <- analysis_data %>%
  select(
    hhid,
    district_code,
    log_food_ae,
    food,
    luc_intensity,   # key IV (district-level analysis)
    quintile_f,      # control + stratifier for H2
    ur_f,            # control
    province_f       # control (province fixed effects)
  )

saveRDS(analysis_data_refined, file.path(processed_path, "analysis_data_refined.rds"))