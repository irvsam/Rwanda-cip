# ============================================================
# 05_main_model.R
# PRIMARY ANALYSIS.
#
# H1: household crop concentration (HHI, Season A) is negatively
#     associated with food consumption per adult equivalent.
# H2: that association is stronger in districts with higher LUC
#     intensity (compounding).
#
# Sample: AHS 2024 sub-panel (in_ahs), linked to EICV7 by hhid.
# All variables are built in 04a_build_analysis_data.R.
#
# Outcome: log real food consumption per adult equivalent
#   (food / ae / hh_index, Jan 2024 prices).
# Controls: predetermined household characteristics and land held.
#   The consumption quintile is NOT used: it is built from total
#   consumption, which contains the outcome.
# Inference: CR2 cluster-robust SEs by district (lm_robust default).
# ============================================================

source("scripts/00_setup.R")


master <- readRDS(file.path(processed_path, "master.rds"))

# ---- Estimation sample, centred on that sample ---------------
primary <- master %>%
  filter(in_ahs) %>%
  mutate(
    crop_hhi_c = crop_hhi - mean(crop_hhi),
    luc_c      = luc_intensity   - mean(luc_intensity),
    luc_A_c    = luc_intensity_A - mean(luc_intensity_A)
  )

cat("Primary sample:", nrow(primary), "households in",
    n_distinct(primary$district_code), "districts\n")

# ---- Specifications ------------------------------------------
# (1) geography only, (2) + household controls, (3) + land = primary
f_base <- log_food_ae_real ~ crop_hhi_c * luc_c + ur_f + province_f
f_hh   <- update(f_base, . ~ . + hh_size + dep_ratio + head_age +
                   head_female + head_educ)
f_main <- update(f_hh, . ~ . + log_land)

fit <- function(f, data = primary, ...) {
  lm_robust(f, data = data, clusters = district_code, ...)
}

main_models <- list(
  "(1) Geography"          = fit(f_base),
  "(2) + Household"        = fit(f_hh),
  "(3) + Land (primary)"   = fit(f_main)
)
map(main_models, summary)

# ---- Robustness ----------------------------------------------
# Season A LUC: swap the moderator in a copy of the data so the
# coefficient rows line up with the primary model in the table.
primary_A <- primary %>% mutate(luc_c = luc_A_c)

robust_models <- list(
  "Primary"           = main_models[["(3) + Land (primary)"]],
  "AHS weights"       = fit(f_main, weights = wt_ahs),
  "Season A LUC"      = fit(f_main, data = primary_A),
  "Nominal outcome"   = fit(update(f_main, log_food_ae_nominal ~ .))
)
map(robust_models, summary)

# ---- Marginal effect of HHI across district LUC intensity -----
# slope(L) = b_hhi + L * b_interaction, with its SE from the
# cluster-robust vcov. Evaluated at the 10th, 50th and 90th
# percentiles of LUC across the 30 districts. t critical value uses
# (clusters - 1) df, a conservative choice with 30 clusters.
luc_pcts <- primary %>%
  distinct(district_code, luc_intensity) %>%
  summarise(p10 = quantile(luc_intensity, 0.10),
            p50 = quantile(luc_intensity, 0.50),
            p90 = quantile(luc_intensity, 0.90)) %>%
  pivot_longer(everything(), names_to = "luc_pctile", values_to = "luc_intensity")

df_clust <- n_distinct(primary$district_code) - 1
sd_hhi   <- sd(primary$crop_hhi)

hhi_slope_at <- function(model, luc_table, luc_mean, hhi = "crop_hhi_c",
                         int = "crop_hhi_c:luc_c") {
  b <- coef(model)
  V <- vcov(model)
  luc_table %>%
    mutate(
      L        = luc_intensity - luc_mean,
      slope    = b[hhi] + L * b[int],
      se       = sqrt(V[hhi, hhi] + L^2 * V[int, int] + 2 * L * V[hhi, int]),
      conf.low  = slope - qt(0.975, df_clust) * se,
      conf.high = slope + qt(0.975, df_clust) * se,
      # % difference in food consumption for a one-SD increase in HHI
      pct_1sd      = 100 * (exp(slope * sd_hhi) - 1),
      pct_1sd_low  = 100 * (exp(conf.low * sd_hhi) - 1),
      pct_1sd_high = 100 * (exp(conf.high * sd_hhi) - 1)
    )
}

marginal_effects <- hhi_slope_at(main_models[["(3) + Land (primary)"]],
                                 luc_pcts, mean(primary$luc_intensity))
cat("\nSD of HHI:", round(sd_hhi, 3), "\n")
print(marginal_effects %>%
        select(luc_pctile, luc_intensity, slope, conf.low, conf.high,
               pct_1sd, pct_1sd_low, pct_1sd_high),
      width = Inf)

# ---- Multilevel cross-check: random intercept and random slope -----
# The random slope for HHI matters: H2 claims the HHI slope varies by
# district, and a random-intercept-only model with a cross-level
# interaction tends to understate the interaction's SE
# (Heisig & Schaeffer, 2019).
ml_int   <- lmer(update(f_main, . ~ . + (1 | district_code)), data = primary)
ml_slope <- lmer(update(f_main, . ~ . + (1 + crop_hhi_c | district_code)),
                 data = primary)

key <- c("crop_hhi_c", "crop_hhi_c:luc_c")
cat("\nRandom intercept:\n");  print(summary(ml_int)$coefficients[key, ])
cat("\nRandom slope:\n");      print(summary(ml_slope)$coefficients[key, ])
cat("\nRandom slope model singular?", isSingular(ml_slope), "\n")
print(VarCorr(ml_slope))

vc  <- as.data.frame(VarCorr(ml_int))
icc <- vc$vcov[vc$grp == "district_code"] / sum(vc$vcov)
cat("ICC (conditional on controls):", round(icc, 3), "\n")

# ---- Tables for the paper ------------------------------------
coef_labels <- c(
  "crop_hhi_c"       = "Crop concentration (HHI, centred)",
  "luc_c"            = "District LUC intensity (pp, centred)",
  "crop_hhi_c:luc_c" = "HHI x LUC intensity",
  "log_land"         = "Log agricultural land (ha)",
  "hh_size"          = "Household size",
  "dep_ratio"        = "Dependency ratio",
  "head_age"         = "Head age",
  "head_female"      = "Female head",
  "head_educAttended, no certificate" = "Head: attended, no certificate",
  "head_educPrimary"           = "Head: primary",
  "head_educSecondary or TVET" = "Head: secondary or TVET",
  "head_educTertiary"          = "Head: tertiary",
  "ur_f2"            = "Rural"
)

table_notes <- paste(
  "CR2 standard errors clustered by district (30 clusters) in parentheses.",
  "Outcome: log food consumption per adult equivalent, January 2024 prices.",
  "All models include province fixed effects. HHI and LUC are centred on",
  "their sample means. Reference education category: never attended."
)

modelsummary(
  main_models,
  coef_map = coef_labels,
  gof_map  = c("nobs", "r.squared"),
  stars    = TRUE,
  title    = "Household crop concentration, district LUC intensity and food consumption",
  notes    = table_notes,
  output   = file.path(output_tables_path, "primary_results.tex")
)

modelsummary(
  robust_models,
  coef_map = coef_labels[c("crop_hhi_c", "luc_c", "crop_hhi_c:luc_c")],
  gof_map  = c("nobs", "r.squared"),
  stars    = TRUE,
  title    = "Robustness of the primary specification",
  notes    = paste(table_notes,
                   "All columns include the full control set of the primary model."),
  output   = file.path(output_tables_path, "primary_robustness.tex")
)

# ---- Save for figures (07) -----------------------------------
saveRDS(
  list(primary = primary, main_models = main_models,
       robust_models = robust_models, marginal_effects = marginal_effects,
       ml_int = ml_int, ml_slope = ml_slope, icc = icc),
  file.path(processed_path, "primary_models.rds")
)