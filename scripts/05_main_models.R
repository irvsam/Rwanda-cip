# ============================================================
# 05_main_models.R
# District-level analysis: does LUC intensity affect household food
# welfare, controlling for wealth -- and is that effect concentrated
# among poorer households?
#
# This is the SECONDARY / robustness analysis in the final paper
# structure. The PRIMARY analysis (household-level crop concentration
# as IV, LUC intensity as moderator) lives in
# 06_household_crop_concentration.R

# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

# ---- Naive OLS (unclustered) ----
model_main_naive <- lm(
  log_food_ae ~ luc_intensity + quintile_f + ur_f + province_f,
  data = analysis_data
)
summary(model_main_naive)

model_interaction_naive <- lm(
  log_food_ae ~ luc_intensity * quintile_f + ur_f + province_f,
  data = analysis_data
)
summary(model_interaction_naive)

models_by_quintile_naive <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm(log_food_ae ~ luc_intensity + ur_f + province_f, data = .x))

map(models_by_quintile_naive, summary)

modelsummary(
  c(list("Main effect" = model_main_naive, "Interaction" = model_interaction_naive),
    models_by_quintile_naive),
  output = file.path(output_tables_path, "main_results_naive.docx")
)

# ---- Clustered version (the correctly-inferred results) ----
# LUC intensity is measured at the district level, so households
# within a district are not independent observations with respect
# to it; clustering by district_code corrects the standard errors.
model_main <- lm_robust(
  log_food_ae ~ luc_intensity + quintile_f + ur_f + province_f,
  data = analysis_data,
  clusters = district_code
)
summary(model_main)

model_interaction <- lm_robust(
  log_food_ae ~ luc_intensity * quintile_f + ur_f + province_f,
  data = analysis_data,
  clusters = district_code
)
summary(model_interaction)

models_by_quintile <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm_robust(
    log_food_ae ~ luc_intensity + ur_f + province_f,
    data = .x,
    clusters = district_code
  ))

map(models_by_quintile, summary)

modelsummary(
  c(list("Main effect" = model_main, "Interaction" = model_interaction),
    models_by_quintile),
  output = file.path(output_tables_path, "main_results_clustered.docx")
)

# ============================================================
# MECHANISM TEST
# Does LUC intensity affect own-production, an intermediate channel
# plausibly linking district-level consolidation to reduced food
# consumption? One pre-specified test, reported regardless of result.
# ============================================================

if (!exists("expenditure_C")) source("scripts/01_load_eicv7.R")

# Households with zero valid s08a4_4 rows get NA, not -Inf:
# max(..., na.rm = TRUE) on an all-NA/empty vector silently returns
# -Inf, which crashes lm_robust()'s underlying C++ code rather than
# throwing a normal R error.
own_prod <- expenditure_C %>%
  select(hhid, s08a4_4) %>%
  group_by(hhid) %>%
  summarise(
    has_own_prod     = if (all(is.na(s08a4_4))) NA_real_
    else max(as.numeric(s08a4_4) == 1, na.rm = TRUE),
    n_own_prod_items = sum(as.numeric(s08a4_4) == 1, na.rm = TRUE),
    n_missing        = sum(is.na(s08a4_4)),
    .groups = "drop"
  )

sum(is.na(own_prod$has_own_prod))  # how many households this affects

mechanism_data <- analysis_data %>%
  left_join(own_prod, by = "hhid") %>%
  filter(is.finite(has_own_prod))  # defensive: drop any NA/-Inf/NaN

nrow(mechanism_data)

model_mech_binary <- lm_robust(
  has_own_prod ~ luc_intensity + quintile_f + ur_f + province_f,
  data = mechanism_data,
  clusters = district_code
)
summary(model_mech_binary)

model_mech_count <- lm_robust(
  n_own_prod_items ~ luc_intensity + quintile_f + ur_f + province_f,
  data = mechanism_data,
  clusters = district_code
)
summary(model_mech_count)

mech_models_by_quintile <- mechanism_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(mechanism_data$quintile_f))) %>%
  map(~ lm_robust(
    has_own_prod ~ luc_intensity + ur_f + province_f,
    data = .x,
    clusters = district_code
  ))

map(mech_models_by_quintile, summary)

modelsummary(
  c(list("Has own production" = model_mech_binary,
         "N own-production items" = model_mech_count),
    mech_models_by_quintile),
  output = file.path(output_tables_path, "mechanism_results.docx")
)