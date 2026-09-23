# ============================================================
# 05b_diet_models.R
# Diet outcomes along the mechanism chain, same sample and
# specification as 05 (HHI x district LUC, predetermined controls
# including land, CR2 SEs by district).
#
#   hdds_own       : own-produced food groups  (link 1)
#   purch_share    : purchased share of items  (link 2, market dependence)
#   hdds_purch     : purchased food groups     (link 3, compensation; H2)
#   hdds           : dietary diversity, 12 FAO groups (PRIMARY; H1)
#   hdds_nonstaple : non-staple groups (veg, fruit, meat, eggs, fish, dairy)
#
# HDDS was fixed as the primary outcome from its distribution before
# any modelling (no ceiling: mean 8.1, SD 1.6 in the AHS sample).
# Counts are modelled by OLS so coefficients read as food groups.
# ============================================================

if (!exists(".setup_done")) source("scripts/00_setup.R")
master  <- readRDS(file.path(processed_path, "master.rds"))
primary <- make_primary(master)

# ---- Stepwise for the primary outcome -------------------------
hdds_steps <- list(
  "(1) Geography"        = fit_cl(make_f("hdds", CONTROLS_GEO), primary),
  "(2) + Household"      = fit_cl(make_f("hdds", c(CONTROLS_GEO, CONTROLS_HH)), primary),
  "(3) + Land (primary)" = fit_cl(make_f("hdds"), primary)
)
walk(hdds_steps, ~ print(summary(.x)$coefficients[names(KEY_LABELS)[1:3], ]))

# ---- All diet outcomes, full specification --------------------
outcomes <- c(
  "Own-produced groups"      = "hdds_own",
  "Purchased share of items" = "purch_share",
  "Purchased groups"         = "hdds_purch",
  "Dietary diversity (HDDS)" = "hdds",
  "Non-staple groups"        = "hdds_nonstaple"
)

diet_models <- map(outcomes, ~ fit_cl(make_f(.x), primary))
walk2(diet_models, names(diet_models), function(m, nm) {
  cat("\n", nm, "\n")
  print(summary(m)$coefficients[names(KEY_LABELS)[1:3], ])
})

# ---- Marginal effects (outcome units per one-SD HHI) ----------
diet_marginal <- imap_dfr(diet_models, ~ mutate(slope_at(.x, primary), outcome = .y)) %>%
  relocate(outcome)
print(diet_marginal %>% select(outcome, luc_pctile, luc_intensity,
                               effect_1sd, low_1sd, high_1sd),
      n = Inf, width = Inf)

# ---- Robustness: HDDS (H1) and purchased groups (H2) ----------
primary_A <- primary %>% mutate(luc_c = luc_A_c)   # Season A moderator
cat("\nAHS households with HDDS = 0:", sum(primary$hdds == 0, na.rm = TRUE), "\n")

robust_outcomes <- c(hdds = "Dietary diversity (HDDS)", hdds_purch = "Purchased groups")

diet_robust <- imap(robust_outcomes, function(label, y) {
  list(
    "Primary"        = diet_models[[label]],
    "AHS weights"    = fit_cl(make_f(y), primary, weights = wt_ahs),
    "Season A LUC"   = fit_cl(make_f(y), primary_A),
    "Excl. HDDS = 0" = fit_cl(make_f(y), filter(primary, hdds > 0))
  )
})

diet_ml <- map(names(robust_outcomes), ~ fit_ml(make_f(.x), primary)) %>%
  set_names(names(robust_outcomes))
iwalk(diet_ml, function(m, y) {
  cat("\nRandom-slope model:", y, " singular:", isSingular(m), "\n")
  print(summary(m)$coefficients[c("crop_hhi_c", "crop_hhi_c:luc_c"), ])
})

# ---- Tables ---------------------------------------------------
diet_notes <- paste(TABLE_NOTES,
                    "Food groups counted over the four EICV7 consumption visits.")

modelsummary(
  hdds_steps, coef_map = KEY_LABELS, gof_map = c("nobs", "r.squared"),
  stars = TRUE, notes = diet_notes,
  title = "Crop concentration, district LUC intensity and dietary diversity",
  output = file.path(output_tables_path, "hdds_steps.tex")
)

iwalk(diet_robust, function(models, y) {
  modelsummary(
    models, coef_map = KEY_LABELS[1:3], gof_map = c("nobs", "r.squared"),
    stars = TRUE, notes = diet_notes,
    title = paste("Robustness:", robust_outcomes[[y]]),
    output = file.path(output_tables_path, paste0("robust_", y, ".tex"))
  )
})

# ---- Main results table: the whole chain ----------------------
food_value_model <- readRDS(file.path(processed_path, "primary_models.rds"))$main_models[[
  "(3) + Land (primary)"]]

chain_models <- c(
  set_names(diet_models, c("Own-produced groups", "Purchased share",
                           "Purchased groups", "HDDS", "Non-staple groups")),
  list("Log food value" = food_value_model)
)

modelsummary(
  chain_models, coef_map = KEY_LABELS, gof_map = c("nobs", "r.squared"),
  stars = TRUE,
  notes = paste(diet_notes,
                "Log food value: food consumption per adult equivalent, Jan 2024 prices."),
  title = "Crop concentration and household diets along the mechanism",
  output = file.path(output_tables_path, "chain_results.tex")
)

saveRDS(list(hdds_steps = hdds_steps, diet_models = diet_models,
             diet_marginal = diet_marginal, diet_robust = diet_robust,
             diet_ml = diet_ml, chain_models = chain_models),
        file.path(processed_path, "diet_models.rds"))