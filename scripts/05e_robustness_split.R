# 05e_robustness_split.R ---------------------
# Robustness checks for the primary (split) design, on the two diet
# quality outcomes and on purchased groups (the H2 mechanism link):
#   AHS survey weights, Season A only (HHI parts and LUC), excluding the
#   five households with HDDS = 0, a multilevel model with random slopes
#   for both parts, and village (EICV7 cluster) fixed effects.
# Village FE are also run for the whole chain.

if (!exists(".setup_done")) source("scripts/00_setup.R")
master    <- readRDS(file.path(processed_path, "master.rds"))
primary   <- make_primary(master)
primary_A <- make_season_A(primary)

robust_outcomes <- c(
  hdds_nonstaple = "Non-staple groups",
  hdds_asf       = "Animal-source groups",
  hdds_purch     = "Purchased groups"
)


top_district <- primary %>% slice_max(luc_intensity, n = 1) %>%
  pull(district_code) %>% unique()

# ---- OLS checks -----------------------------------------------
split_robust <- imap(robust_outcomes, function(label, y) {
  list(
    "Primary"        = fit_cl(make_f_split(y), primary),
    "AHS weights"    = fit_cl(make_f_split(y), primary, weights = wt_ahs),
    "Season A only"  = fit_cl(make_f_split(y), primary_A),
    "Excl. HDDS = 0" = fit_cl(make_f_split(y), filter(primary, hdds > 0)),
    "Excl. top-LUC district" = fit_cl(make_f_split(y), filter(primary, district_code != top_district))
  )
})

iwalk(split_robust, function(models, y) {
  cat("\nRobustness:", robust_outcomes[[y]], "\n")
  print(modelsummary(models, coef_map = SPLIT_LABELS[-6], gof_map = "nobs",
                     stars = TRUE, output = "markdown"))
})

# ---- Multilevel: random slopes for both parts -------------------
# May be singular with 30 districts; if so, the df are not reliable
split_ml <- map(names(robust_outcomes), ~ fit_ml_split(make_f_split(.x), primary)) %>%
  set_names(names(robust_outcomes))

iwalk(split_ml, function(m, y) {
  cat("\nRandom-slope model:", robust_outcomes[[y]], " singular:", isSingular(m), "\n")
  print(summary(m)$coefficients[SPLIT_TERMS[-3], ])
})

# ---- Village fixed effects, whole chain --------------------------
vfe_outcomes <- c(
  "Own-produced groups"  = "hdds_own",
  "Purchased share"      = "purch_share",
  "Purchased groups"     = "hdds_purch",
  "Non-staple groups"    = "hdds_nonstaple",
  "Animal-source groups" = "hdds_asf",
  "HDDS"                 = "hdds",
  "Log food value"       = "log_food_ae_real"
)

split_vfe <- map(vfe_outcomes, ~ fit_cl(make_f_split_vfe(.x), primary))

walk2(split_vfe, names(split_vfe), function(m, nm) {
  cat("\n Village FE:", nm, "\n")
  print(summary(m)$coefficients[SPLIT_TERMS[-3], ])
})

# ---- Tables ---------------------------------------------------
robust_notes <- paste(SPLIT_NOTES,
                      "Food groups counted over the four EICV7 consumption visits.",
                      "Season A only: both parts of HHI and LUC intensity measured in Season A.",
                      "Excl. top-LUC district: drops the district with the highest LUC intensity")


iwalk(split_robust, function(models, y) {
  modelsummary(
    models, coef_map = SPLIT_LABELS[-6], gof_map = c("nobs", "r.squared"),
    stars = TRUE, notes = robust_notes,
    title = paste("Robustness:", robust_outcomes[[y]]),
    output = file.path(output_tables_path, paste0("robust_split_", y, ".tex"))
  )
})

VFE_SPLIT_NOTES <- paste(
  "CR2 standard errors clustered by district (30 clusters) in parentheses.",
  "All models include village (EICV7 cluster) fixed effects, household size,",
  "dependency ratio, head age, sex and education, and log land held.",
  "Province, urban/rural and the district LUC main effect are absorbed by the village fixed effects.",
  "Both parts of HHI and LUC intensity are centred on their sample means."
)

modelsummary(
  split_vfe, coef_map = SPLIT_LABELS[c(1, 2, 4, 5)], gof_map = c("nobs", "r.squared"),
  stars = TRUE, notes = VFE_SPLIT_NOTES,
  title = "Village fixed effects: priority-crop concentration and household diets",
  output = file.path(output_tables_path, "village_fe_split.tex")
)

saveRDS(list(split_robust = split_robust, split_ml = split_ml, split_vfe = split_vfe),
        file.path(processed_path, "split_robust.rds"))