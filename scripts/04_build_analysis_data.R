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