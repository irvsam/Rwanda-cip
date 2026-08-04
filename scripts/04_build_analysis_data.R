# ============================================================
# 04_build_analysis_data.R
# One row per household. Builds every variable needed
# ============================================================

source("scripts/00_setup.R")
source("scripts/01_load_eicv7.R")
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

# colnames(hh_data)
# lf <- labelled::look_for(hh_data)
# lf %>% as_tibble() %>% print(n = Inf)



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


saveRDS(analysis_data_refined, file.path(processed_path, "analysis_data_refined.rds"))


# ============================================================
# Quick diagnostic: do high-LUC districts have more own production?
# ============================================================


dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

# Build a minimal dataset just for this test
test_data <- hh_data %>%
  mutate(district_code = as.numeric(district)) %>%
  left_join(dist_luc, by = "district_code") %>%
  left_join(poverty_data %>% select(hhid, food), by = "hhid")

# Quick checks
nrow(test_data)
head(test_data$luc_intensity)
head(test_data$food)

# Does own production exist in expenditure files?
colnames(expenditure_C)  # S8A3 has s08a4_4 (own production yes/no)

# Pull own production from expenditure_C
own_prod <- expenditure_C %>%
  select(hhid, s08a4_4, s08a4_5, s08a4_7) %>%
  # Collapse to household level: does HH have ANY own production?
  group_by(hhid) %>%
  summarise(
    has_own_prod = max(as.numeric(s08a4_4), na.rm = TRUE),  # 1 if any item consumed from own production
    n_own_prod_items = sum(as.numeric(s08a4_4) == 1, na.rm = TRUE)
  )

# Join to test data
test_data <- test_data %>%
  left_join(own_prod, by = "hhid")

own_prod <- expenditure_C %>%
  select(hhid, s08a4_4) %>%
  group_by(hhid) %>%
  summarise(
    has_own_prod = max(as.numeric(s08a4_4) == 1, na.rm = TRUE),  # 1 if ANY row says yes
    n_missing = sum(is.na(s08a4_4))
  )

# Check how much data we have
own_prod %>%
  summarise(
    pct_missing_all = mean(n_missing > 0) * 100,
    pct_with_own_prod = mean(has_own_prod == 1) * 100
  )

# Then rerun the tests with this cleaner version
test_data <- test_data %>%
  select(-has_own_prod, -n_own_prod_items) %>%  # drop old ones
  left_join(own_prod, by = "hhid")

# TEST 1 again
test_data %>%
  mutate(high_luc = luc_intensity > 10) %>%
  group_by(high_luc) %>%
  summarise(
    n = n(),
    pct_with_own_prod = mean(has_own_prod == 1, na.rm = TRUE) * 100
  )


# TEST 2: Split by quintile — do poor people in high-LUC have less own production?
test_data %>%
  mutate(
    quintile_f = as.factor(as.numeric(quintile)),
    high_luc = luc_intensity > 10
  ) %>%
  filter(as.numeric(quintile_f) == 1) %>%  # poorest only
  group_by(high_luc) %>%
  summarise(
    n = n(),
    pct_with_own_prod = mean(has_own_prod == 1, na.rm = TRUE) * 100,
    mean_food_spending = mean(food, na.rm = TRUE)
  )
