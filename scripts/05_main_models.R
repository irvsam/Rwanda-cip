# ============================================================
# 05_main_models.R
# Food value per adult equivalent: the most distal outcome in the
# mechanism chain (05b runs the diet outcomes with the identical
# sample and specification).
#
# Outcome: log real food consumption per adult equivalent
#   (food / ae / hh_index, January 2024 prices).
# Specification: HHI x district LUC intensity, predetermined
#   controls including land (CONTROLS in 00_setup.R).
# Inference: CR2 cluster-robust SEs by district.
# ============================================================

if (!exists(".setup_done")) source("scripts/00_setup.R")
master  <- readRDS(file.path(processed_path, "master.rds"))
primary <- make_primary(master)

cat("Primary sample:", nrow(primary), "households in",
    n_distinct(primary$district_code), "districts\n")

y <- "log_food_ae_real"

# ---- Stepwise: what the controls do --------------------------
main_models <- list(
  "(1) Geography"        = fit_cl(make_f(y, CONTROLS_GEO), primary),
  "(2) + Household"      = fit_cl(make_f(y, c(CONTROLS_GEO, CONTROLS_HH)), primary),
  "(3) + Land (primary)" = fit_cl(make_f(y), primary)
)
walk(main_models, ~ print(summary(.x)$coefficients[names(KEY_LABELS)[1:3], ]))

# ---- Robustness ----------------------------------------------
primary_A <- primary %>% mutate(luc_c = luc_A_c)   # Season A moderator

robust_models <- list(
  "Primary"         = main_models[["(3) + Land (primary)"]],
  "AHS weights"     = fit_cl(make_f(y), primary, weights = wt_ahs),
  "Season A LUC"    = fit_cl(make_f(y), primary_A),
  "Nominal outcome" = fit_cl(make_f("log_food_ae_nominal"), primary)
)

# ---- Marginal effect of HHI (% per one-SD increase) ----------
marginal_effects <- slope_at(main_models[["(3) + Land (primary)"]], primary) %>%
  mutate(pct_1sd      = 100 * (exp(effect_1sd) - 1),
         pct_1sd_low  = 100 * (exp(low_1sd) - 1),
         pct_1sd_high = 100 * (exp(high_1sd) - 1))
print(marginal_effects, width = Inf)

# ---- Multilevel cross-check -----------------------------------
ml_slope <- fit_ml(make_f(y), primary)
cat("\nRandom-slope model, singular:", isSingular(ml_slope), "\n")
print(summary(ml_slope)$coefficients[c("crop_hhi_c", "crop_hhi_c:luc_c"), ])

ml_int <- lmer(update(make_f(y), . ~ . + (1 | district_code)), data = primary)
vc  <- as.data.frame(VarCorr(ml_int))
icc <- vc$vcov[vc$grp == "district_code"] / sum(vc$vcov)
cat("ICC (conditional on controls):", round(icc, 3), "\n")

# ---- Tables ---------------------------------------------------
modelsummary(
  main_models, coef_map = ALL_LABELS, gof_map = c("nobs", "r.squared"),
  stars = TRUE, notes = TABLE_NOTES,
  title = "Crop concentration, district LUC intensity and food value",
  output = file.path(output_tables_path, "food_value_steps.tex")
)

modelsummary(
  robust_models, coef_map = KEY_LABELS[1:3], gof_map = c("nobs", "r.squared"),
  stars = TRUE, notes = TABLE_NOTES,
  title = "Robustness: food value",
  output = file.path(output_tables_path, "food_value_robust.tex")
)

saveRDS(
  list(primary = primary, main_models = main_models,
       robust_models = robust_models, marginal_effects = marginal_effects,
       ml_slope = ml_slope, icc = icc),
  file.path(processed_path, "primary_models.rds")
)