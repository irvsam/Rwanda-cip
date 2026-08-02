# ============================================================
# 04_build_analysis_data.R
# One row per household. Builds every variable needed
# ============================================================

source("scripts/00_setup.R")
source("scripts/01_load_eicv7.R")
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

analysis_data <- hh_data %>%
  mutate(
    district_code = as.numeric(district),
    quintile_f    = as.factor(as.numeric(quintile)),   # H2 stratifier / control
    ur_f          = as.factor(as.numeric(ur)),
    province_f    = as.factor(as.numeric(province)),    # province fixed effects
    
    # Key IV: most severe shock in past 12 months is
    # climate-related (s5eq2a categories 1-6: drought, irregular rainfall,
    # heavy rain, flooding, landslides, crop/livestock disease)
    shock = as.numeric(as.numeric(s5eq1) == 1 & as.numeric(s5eq2a) %in% 1:6)
    
    # TODO: add household size and household-head sex/age controls once confirmed
  ) %>%
  left_join(poverty_data %>% select(hhid, food, ae, member), by = "hhid") %>%
  left_join(dist_luc, by = "district_code") %>%
  filter(!is.na(food), !is.na(luc_intensity)) %>%
  mutate(log_food_ae = log(food))   # DV 

cat("Rows:", nrow(analysis_data), "\n")
cat("Missing LUC:", sum(is.na(analysis_data$luc_intensity)), "\n")
count(analysis_data, shock)
count(analysis_data, quintile_f)

saveRDS(analysis_data, file.path(processed_path, "analysis_data.rds"))



# Refining 
analysis_data_refined <- analysis_data %>%
  select(
    hhid,                # household id
    district_code,        # for joining/checking, e.g. against dist_rainfall_shock
    log_food_ae,          # DV
    food,                 # DV, pre-log, useful for descriptives
    shock,                # key IV
    luc_intensity,        # moderator
    quintile_f,           # control + H2 stratifier
    ur_f,                 # control
    province_f             # control (province fixed effects)
    # add hh size / head sex / head age here once those columns exist
  )

str(analysis_data_refined)
nrow(analysis_data_refined)

saveRDS(analysis_data_refined, file.path(processed_path, "analysis_data_refined.rds"))