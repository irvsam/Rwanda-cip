# ============================================================
# 06_main_models.R
# SECONDARY ANALYSIS (extension): district LUC intensity and food
# consumption in the full EICV7 sample, without the household
# crop concentration measure.
#
# Reported as context for the primary analysis (06), not as an
# independent test of H1/H2. With 30 districts it is low-powered,
# so a minimum detectable effect is reported alongside the estimate.
#
# Changes from the earlier version:
#   - outcome is now food per adult equivalent, deflated
#   - quintile control and quintile-stratified models removed
#     (the quintile is built from consumption; stratifying on it
#     selects on the outcome)
#   - household size added (only predetermined control available
#     for the full EICV7 sample in the master file)
#   - rural-only and weighted checks added; naive SEs dropped
# ============================================================

source("scripts/00_setup.R")
master <- readRDS(file.path(processed_path, "master.rds"))

# Check the urban/rural coding before trusting the rural subset
if (!exists("poverty_data")) source("scripts/01_load_eicv7.R")
print(val_labels(poverty_data$ur))
RURAL_CODE <- 2   # confirm against the labels printed above

full <- master %>% filter(!is.na(log_food_ae_real), !is.na(luc_intensity))
cat("Extension sample:", nrow(full), "households in",
    n_distinct(full$district_code), "districts\n")

f_ext <- log_food_ae_real ~ luc_intensity + hh_size + ur_f + province_f

fit <- function(f, data, ...) {
  lm_robust(f, data = data, clusters = district_code, ...)
}

ext_models <- list(
  "Full sample"    = fit(f_ext, full),
  "Rural only"     = fit(update(f_ext, . ~ . - ur_f),
                         filter(full, ur == RURAL_CODE)),
  "EICV7 weights"  = fit(f_ext, full, weights = wt_eicv)
)
map(ext_models, summary)

# ---- Minimum detectable effect --------------------------------
# With 80% power and a two-sided 5% test, MDE ~ 2.8 x SE.
# Also expressed across the 10th-90th percentile range of LUC,
# so it can be compared with the primary model's magnitudes.
luc_range <- full %>%
  distinct(district_code, luc_intensity) %>%
  summarise(p10 = quantile(luc_intensity, 0.10),
            p90 = quantile(luc_intensity, 0.90))

se_luc <- ext_models[["Full sample"]]$std.error[["luc_intensity"]]
mde    <- 2.8 * se_luc
cat("\nLUC coefficient:", round(coef(ext_models[["Full sample"]])[["luc_intensity"]], 5),
    " SE:", round(se_luc, 5), "\n")
cat("MDE per percentage point of LUC:", round(mde, 5), "\n")
cat("MDE across the p10-p90 LUC range, in %:",
    round(100 * (exp(mde * (luc_range$p90 - luc_range$p10)) - 1), 1), "\n")

modelsummary(
  ext_models,
  coef_map = c("luc_intensity" = "District LUC intensity (pp)",
               "hh_size"       = "Household size",
               "ur_f2"         = "Rural"),
  gof_map  = c("nobs", "r.squared"),
  stars    = TRUE,
  title    = "District LUC intensity and food consumption, full EICV7 sample",
  notes    = paste("CR2 standard errors clustered by district (30 clusters).",
                   "Outcome: log food consumption per adult equivalent, January 2024 prices.",
                   "All models include province fixed effects."),
  output   = file.path(output_tables_path, "extension_results.tex")
)

# ============================================================
# MECHANISM TEST (optional; not currently reported in the paper)
# Is district LUC intensity associated with own-production?
# ============================================================

if (!exists("expenditure_C")) source("scripts/01_load_eicv7.R")

# Households with no valid s08a4_4 rows get NA, not -Inf
own_prod <- expenditure_C %>%
  transmute(hhid = as.numeric(zap_labels(hhid)), s08a4_4 = as.numeric(s08a4_4)) %>%
  group_by(hhid) %>%
  summarise(
    has_own_prod     = if (all(is.na(s08a4_4))) NA_real_
    else max(s08a4_4 == 1, na.rm = TRUE),
    n_own_prod_items = sum(s08a4_4 == 1, na.rm = TRUE),
    .groups = "drop"
  )

mechanism_data <- full %>%
  left_join(own_prod, by = "hhid") %>%
  filter(is.finite(has_own_prod))

cat("\nMechanism sample:", nrow(mechanism_data), "households\n")

mech_models <- list(
  "Has own production"     = fit(update(f_ext, has_own_prod ~ .), mechanism_data),
  "N own-production items" = fit(update(f_ext, n_own_prod_items ~ .), mechanism_data)
)
map(mech_models, summary)

modelsummary(
  mech_models,
  coef_map = c("luc_intensity" = "District LUC intensity (pp)"),
  gof_map  = c("nobs", "r.squared"),
  stars    = TRUE,
  output   = file.path(output_tables_path, "mechanism_results.tex")
)

saveRDS(list(ext_models = ext_models, mech_models = mech_models, mde = mde),
        file.path(processed_path, "extension_models.rds"))