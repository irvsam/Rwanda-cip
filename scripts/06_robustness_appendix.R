# ============================================================
# 06_robustness_appendix.R
# Exploratory / supplementary analysis
# Kept separate because it uses a different sample (shock-affected
# households only) and different outcome variables (recovery time,
# distress coping) 
# ============================================================

source("scripts/00_setup.R")
source("scripts/01_load_eicv7.R")
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

# --- Sample: households with a climate/weather shock as most severe shock ---
# NOTE: this uses codes 1,2,3,4,5,6,8,9 (broader than the initial 1-6).

hh_clean <- hh_data %>%
  mutate(
    had_shock = as.numeric(s5eq1) == 1,
    agri_weather_shock = as.numeric(s5eq2a) %in% c(1, 2, 3, 4, 5, 6, 8, 9),
    not_recovered = case_when(
      as.numeric(s5eq4a) == 99 ~ 1L,
      !is.na(s5eq4a) ~ 0L,
      TRUE ~ NA_integer_
    ),
    district_code = as.numeric(district),
    quintile_f = as.factor(as.numeric(quintile)),
    ur_f = as.factor(as.numeric(ur)),
    province_f = as.factor(as.numeric(province))
  ) %>%
  filter(had_shock, agri_weather_shock, !is.na(not_recovered)) %>%
  left_join(dist_luc, by = "district_code")

nrow(hh_clean)
count(hh_clean, not_recovered)

# Recovery ~ LUC (alternative outcome to food welfare)
model_recovery <- glm(
  not_recovered ~ luc_intensity + quintile_f + luc_intensity:quintile_f +
    ur_f + province_f,
  data = hh_clean,
  family = binomial(link = "logit")
)
summary(model_recovery)

# --- Distress coping strategies (alternative outcome) ---
distress_codes <- c(2, 3, 4, 8, 9, 11, 13, 14)
# 2=reduced food, 3=reduced health/education, 4=sold productive assets,
# 8=sold house/land, 9=withdrew child from school, 11=sold last female
# animals, 13=entire household migrated, 14=begging

hh_clean2 <- hh_clean %>%
  mutate(
    distress_coping = case_when(
      as.numeric(s5eq3a) %in% distress_codes ~ 1L,
      !is.na(s5eq3a) ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(distress_coping))

count(hh_clean2, distress_coping)

model_distress <- glm(
  distress_coping ~ luc_intensity + quintile_f + luc_intensity:quintile_f +
    ur_f + province_f,
  data = hh_clean2,
  family = binomial(link = "logit")
)
summary(model_distress)

# --- Descriptive breakdowns (useful for Section 5 in prop ---
hh_clean %>%
  group_by(province) %>%
  summarise(n = n(), pct_not_recovered = mean(not_recovered) * 100) %>%
  mutate(province = as_factor(province))

hh_clean %>%
  group_by(quintile_f) %>%
  summarise(n = n(), pct_not_recovered = mean(not_recovered) * 100)

hh_clean %>%
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile) %>%
  summarise(n = n(), mean_luc = mean(luc_intensity), pct_not_recovered = mean(not_recovered) * 100)

# Rural-only check (is Kigali driving the result?)
hh_clean %>%
  filter(as.numeric(ur) == 2) %>%
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile) %>%
  summarise(n = n(), mean_luc = mean(luc_intensity), pct_not_recovered = mean(not_recovered) * 100)

# Drought-only check
hh_clean %>%
  filter(as.numeric(s5eq2a) == 4) %>%
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile) %>%
  summarise(n = n(), mean_luc = mean(luc_intensity), pct_not_recovered = mean(not_recovered) * 100)

# Farming-households-only check (s7aq4 == 1)
hh_clean %>%
  filter(as.numeric(s7aq4) == 1) %>%
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile) %>%
  summarise(n = n(), mean_luc = mean(luc_intensity), pct_not_recovered = mean(not_recovered) * 100)