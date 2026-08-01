# ============================================================
# 05_main_models.R
# Core test: does district-level LUC intensity affect household
# food welfare, controlling for wealth — and is that effect
# concentrated among poorer households? (Shock variable and
# shock interaction dropped.)
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))
str(analysis_data)
head(analysis_data)
colnames(analysis_data)

# Model 1: does LUC intensity predict food welfare, controlling for wealth?
# A negative, significant luc_intensity coefficient = higher-LUC districts
# have lower food consumption welfare, net of wealth quintile.
model_main <- lm(
  log_food_ae ~ luc_intensity + quintile_f + ur_f + province_f,
  data = analysis_data
)
summary(model_main)

# Model 2: is the LUC effect concentrated among poorer households?
# The distributional test. A significant negative interaction on the
# richer quintiles (relative to quintile 1, the reference level) would
# mean the LUC penalty is smaller/absent for wealthier households.
model_interaction <- lm(
  log_food_ae ~ luc_intensity * quintile_f + ur_f + province_f,
  data = analysis_data
)
summary(model_interaction)

# Same test, split by quintile group — easier to read the pattern directly
# than the interaction coefficients above.
models_by_quintile <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm(log_food_ae ~ luc_intensity + ur_f + province_f, data = .x))

map(models_by_quintile, summary)

# Export a regression table for Section 5 (Results)
modelsummary(
  c(
    list("Main effect" = model_main, "Interaction" = model_interaction),
    models_by_quintile
  ),
  output = file.path(output_tables_path, "main_results.docx")
)