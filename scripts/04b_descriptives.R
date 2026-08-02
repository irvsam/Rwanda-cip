# ============================================================
# 04b_descriptives.R
# Table 1: descriptive statistics for just the variables used
# in the H1/H2 models
# Catches any possible coding errors
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

vars_of_interest <- analysis_data %>%
  transmute(
    `Food consumption per AE (RWF)`      = food,
    `Food consumption per AE (log)`      = log_food_ae,
    `Climate shock (0/1)`                = shock,
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

# Sanity check ahead of H2: are shock exposure and LUC balanced across
# quintiles, or is one quintile driving everything?
analysis_data %>%
  group_by(quintile_f) %>%
  summarise(
    n = n(),
    pct_shock = mean(shock) * 100,
    mean_luc = mean(luc_intensity),
    mean_log_food_ae = mean(log_food_ae)
  )

# Same, split by shock status — the group means H1 is actually comparing
analysis_data %>%
  group_by(shock) %>%
  summarise(
    n = n(),
    mean_luc = mean(luc_intensity),
    mean_log_food_ae = mean(log_food_ae)
  )


