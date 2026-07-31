# ============================================================
# 05_main_models.R
# Core test of H1 and H2
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

str(analysis_data)
head(analysis_data)
colnames(analysis_data)

# H1: log(food_ae) = b0 + b1*LUC + b2*shock + b3*(LUC x shock) + controls
# A negative, significant b3 = shock penalty is amplified in high-LUC districts
model_h1 <- lm(
  log_food_ae ~ luc_intensity * shock + ur_f + province_f,
  data = analysis_data
)
summary(model_h1)

# H2: is the LUC x shock effect concentrated among poorer households?
# Estimating separately by quintile group.
models_by_quintile <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm(log_food_ae ~ luc_intensity * shock + ur_f + province_f, data = .x))

map(models_by_quintile, summary)

# Single-model alternative to the quintile split (easier to read, same test)
model_h2_interaction <- lm(
  log_food_ae ~ luc_intensity * shock * quintile_f + ur_f + province_f,
  data = analysis_data
)
summary(model_h2_interaction)

# Export a regression table for Section 5 (Results)
modelsummary(
  c(list("H1: Full sample" = model_h1), models_by_quintile),
  output = file.path(output_tables_path,"main_results.docx")
)