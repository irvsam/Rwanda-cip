# ============================================================
# 04b_descriptives.R
# Table 1: descriptive statistics for just the variables used
# in the H1/H2 models
# Catches any possible coding errors
# ============================================================

# H1: District-level LUC intensity is negatively associated with household food consumption welfare.
# H2: The negative association between LUC intensity and food consumption welfare is stronger among poorer households (lower consumption quintiles).

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

vars_of_interest <- analysis_data %>%
  transmute(
    `Food consumption per AE (RWF)`      = food,
    `Food consumption per AE (log)`      = log_food_ae,
    `District LUC intensity (%)`         = luc_intensity,
    `Consumption quintile`               = quintile_f,
    `Urban/rural`                        = ur_f,
    `Province`                           = province_f
  )

# Quick console check first
summary(vars_of_interest)

# Table 1 — numeric vars get mean/SD/min/max/N, factors get counts/%
datasummary_skim(
  vars_of_interest,
  output = file.path(output_tables_path, "table1_descriptives.docx")
)
datasummary_skim(vars_of_interest, type = "categorical",
                 output = file.path(output_tables_path, "table1_descriptives_categorical.docx")
)

# Sanity check: 
# is LUC intensity itself balanced across quintiles, or do poorer households happen to cluster in high-LUC districts already (before you even look at consumption)?
analysis_data %>%
  group_by(quintile_f) %>%
  summarise(
    n = n(),
    mean_luc = mean(luc_intensity),
    mean_log_food_ae = mean(log_food_ae)
  )

# Group means H1 is now comparing: food welfare by LUC level
analysis_data %>%
  mutate(high_luc = luc_intensity > median(luc_intensity, na.rm = TRUE)) %>%
  group_by(high_luc) %>%
  summarise(
    n = n(),
    mean_log_food_ae = mean(log_food_ae),
    mean_food = mean(food)
  )


