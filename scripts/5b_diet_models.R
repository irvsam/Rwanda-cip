# ============================================================
# 05b_diet_models.R
# PRIMARY ANALYSIS, diet outcomes.
#
# Same design as 05_main_model.R (HHI x district LUC, predetermined
# controls including land, CR2 SEs by district), applied to outcomes
# closer to the mechanism:
#
#   hdds           : dietary diversity, 12 FAO groups (PRIMARY outcome,
#                    chosen from the distribution before modelling: no
#                    ceiling, mean 8.1, SD 1.6 in the AHS sample)
#   hdds_nonstaple : non-staple groups (veg, fruit, meat, eggs, fish, dairy)
#   hdds_purch     : groups purchased     (market compensation, H2 test)
#   hdds_own       : groups own-produced  (mechanical first link)
#   purch_share    : share of consumed items purchased (market dependence)
#
# Counts are modelled by OLS so coefficients read as "food groups".
# ============================================================

source("scripts/00_setup.R")
master <- readRDS(file.path(processed_path, "master.rds"))

primary <- master %>%
  filter(in_ahs) %>%
  mutate(
    crop_hhi_c = crop_hhi - mean(crop_hhi),
    luc_c      = luc_intensity - mean(luc_intensity)
  )

controls <- c("ur_f", "province_f", "hh_size", "dep_ratio", "head_age",
              "head_female", "head_educ", "log_land")

make_f <- function(outcome, terms = c("crop_hhi_c * luc_c", controls)) {
  reformulate(terms, response = outcome)
}

fit <- function(f, data = primary, ...) {
  lm_robust(f, data = data, clusters = district_code, ...)
}

# ---- Stepwise for the primary outcome -------------------------
hdds_steps <- list(
  "(1) Geography"        = fit(make_f("hdds", c("crop_hhi_c * luc_c", "ur_f", "province_f"))),
  "(2) + Household"      = fit(make_f("hdds", c("crop_hhi_c * luc_c", setdiff(controls, "log_land")))),
  "(3) + Land (primary)" = fit(make_f("hdds"))
)
map(hdds_steps, summary)

# ---- All diet outcomes, full specification --------------------
outcomes <- c(
  "Dietary diversity (HDDS)"   = "hdds",
  "Non-staple groups"          = "hdds_nonstaple",
  "Purchased groups"           = "hdds_purch",
  "Own-produced groups"        = "hdds_own",
  "Purchased share of items"   = "purch_share"
)

diet_models <- map(outcomes, ~ fit(make_f(.x)))
map(diet_models, summary)

# ---- Marginal effect of HHI at low, median, high LUC ----------
# In outcome units for a one-SD increase in HHI.
luc_pcts <- primary %>%
  distinct(district_code, luc_intensity) %>%
  summarise(p10 = quantile(luc_intensity, 0.10),
            p50 = quantile(luc_intensity, 0.50),
            p90 = quantile(luc_intensity, 0.90)) %>%
  pivot_longer(everything(), names_to = "luc_pctile", values_to = "luc_intensity")

df_clust <- n_distinct(primary$district_code) - 1
sd_hhi   <- sd(primary$crop_hhi)
luc_mean <- mean(primary$luc_intensity)

slope_at <- function(model, hhi = "crop_hhi_c", int = "crop_hhi_c:luc_c") {
  b <- coef(model); V <- vcov(model)
  luc_pcts %>%
    mutate(
      L         = luc_intensity - luc_mean,
      slope     = b[hhi] + L * b[int],
      se        = sqrt(V[hhi, hhi] + L^2 * V[int, int] + 2 * L * V[hhi, int]),
      effect_1sd = slope * sd_hhi,
      low_1sd    = (slope - qt(0.975, df_clust) * se) * sd_hhi,
      high_1sd   = (slope + qt(0.975, df_clust) * se) * sd_hhi
    ) %>%
    select(luc_pctile, luc_intensity, effect_1sd, low_1sd, high_1sd)
}

diet_marginal <- imap_dfr(diet_models, ~ mutate(slope_at(.x), outcome = .y)) %>%
  relocate(outcome)
print(diet_marginal, n = Inf, width = Inf)

# ---- Tables ---------------------------------------------------
key_labels <- c(
  "crop_hhi_c"       = "Crop concentration (HHI, centred)",
  "luc_c"            = "District LUC intensity (pp, centred)",
  "crop_hhi_c:luc_c" = "HHI x LUC intensity",
  "log_land"         = "Log agricultural land (ha)"
)

diet_notes <- paste(
  "CR2 standard errors clustered by district (30 clusters) in parentheses.",
  "All models include province fixed effects, urban/rural, household size,",
  "dependency ratio, head age, sex and education. HHI and LUC centred.",
  "Food groups counted over the four EICV7 consumption visits."
)

modelsummary(
  hdds_steps,
  coef_map = key_labels, gof_map = c("nobs", "r.squared"), stars = TRUE,
  title = "Crop concentration, district LUC intensity and dietary diversity",
  notes = diet_notes,
  output = file.path(output_tables_path, "diet_hdds_steps.tex")
)

modelsummary(
  diet_models,
  coef_map = key_labels, gof_map = c("nobs", "r.squared"), stars = TRUE,
  title = "Diet outcomes: diversity, sourcing and market dependence",
  notes = diet_notes,
  output = file.path(output_tables_path, "diet_outcomes.tex")
)


# ============================================================
# ROBUSTNESS: primary outcome (HDDS) and the H2 compensation test
# (purchased groups), same checks as the food value model
# ============================================================

primary <- primary %>%
  mutate(luc_A_c = luc_intensity_A - mean(luc_intensity_A))
primary_A <- primary %>% mutate(luc_c = luc_A_c)   # Season A moderator

cat("\nAHS households with HDDS = 0:", sum(primary$hdds == 0, na.rm = TRUE), "\n")

robust_outcomes <- c(hdds       = "Dietary diversity (HDDS)",
                     hdds_purch = "Purchased groups")

diet_robust <- imap(robust_outcomes, function(label, y) {
  list(
    "Primary"         = diet_models[[label]],
    "AHS weights"     = fit(make_f(y), weights = wt_ahs),
    "Season A LUC"    = fit(make_f(y), data = primary_A),
    "Excl. HDDS = 0"  = fit(make_f(y), data = filter(primary, hdds > 0))
  )
})
walk(diet_robust, ~ print(modelsummary(.x, coef_map = key_labels[1:3],
                                       gof_map = "nobs", stars = TRUE,
                                       output = "markdown")))

# Multilevel with a random slope for HHI (bobyqa optimiser, which
# usually resolves the marginal convergence warnings seen before)
ml_ctrl <- lmerControl(optimizer = "bobyqa")
diet_ml <- map(names(robust_outcomes), function(y) {
  lmer(update(make_f(y), . ~ . + (1 + crop_hhi_c | district_code)),
       data = primary, control = ml_ctrl)
}) %>% set_names(names(robust_outcomes))

walk2(diet_ml, names(diet_ml), function(m, y) {
  cat("\nRandom-slope model:", y, " singular:", isSingular(m), "\n")
  print(summary(m)$coefficients[c("crop_hhi_c", "crop_hhi_c:luc_c"), ])
})

iwalk(diet_robust, function(models, y) {
  modelsummary(
    models, coef_map = key_labels[1:3], gof_map = c("nobs", "r.squared"),
    stars = TRUE, notes = diet_notes,
    title = paste("Robustness:", robust_outcomes[[y]]),
    output = file.path(output_tables_path, paste0("diet_robust_", y, ".tex"))
  )
})

# ============================================================
# ONE TABLE FOR THE WHOLE CHAIN
# Same sample, same specification; outcomes ordered along the
# mechanism, from the first link to the most distal.
# ============================================================

food_value_model <- readRDS(file.path(processed_path, "primary_models.rds"))$main_models[[
  "(3) + Land (primary)"]]

chain_models <- list(
  "Own-produced groups" = diet_models[["Own-produced groups"]],
  "Purchased share"     = diet_models[["Purchased share of items"]],
  "Purchased groups"    = diet_models[["Purchased groups"]],
  "HDDS"                = diet_models[["Dietary diversity (HDDS)"]],
  "Non-staple groups"   = diet_models[["Non-staple groups"]],
  "Log food value"      = food_value_model
)

modelsummary(
  chain_models,
  coef_map = key_labels, gof_map = c("nobs", "r.squared"), stars = TRUE,
  title = "Crop concentration and household diets along the mechanism",
  notes = paste(diet_notes,
                "Log food value: food consumption per adult equivalent, Jan 2024 prices."),
  output = file.path(output_tables_path, "chain_results.tex")
)

saveRDS(list(hdds_steps = hdds_steps, diet_models = diet_models,
             diet_marginal = diet_marginal, diet_robust = diet_robust,
             diet_ml = diet_ml, chain_models = chain_models),
        file.path(processed_path, "diet_models.rds"))