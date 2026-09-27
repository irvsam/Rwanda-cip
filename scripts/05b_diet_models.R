# 05b_diet_models.R ---------------------
# Diet outcomes along the mechanism chain, same sample and
# specification as 05 (HHI x district LUC, predetermined controls
# including land, CR2 SEs by district).
#
#   hdds_own       : own-produced food groups  (link 1)
#   purch_share    : purchased share of items  (link 2, market dependence)
#   hdds_purch     : purchased food groups     (link 3, compensation; H2)
#   hdds           : dietary diversity, 12 FAO groups (PRIMARY; H1)
#   hdds_nonstaple : non-staple groups (veg, fruit, meat, eggs, fish, dairy)
#   hdds_asf       : animal-source groups (meat, eggs, fish, dairy), 0-4
#
# HDDS was fixed as the primary outcome from its distribution before
# any modelling (no ceiling: mean 8.1, SD 1.6 in the AHS sample).
# Counts are modelled by OLS so coefficients read as food groups.
# The per-group models at the end are descriptive: one linear
# probability model per food group, to show which groups drop.

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
  "Non-staple groups"        = "hdds_nonstaple",
  "Animal-source groups"     = "hdds_asf"
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
primary_A <- primary %>%
  filter(!is.na(crop_hhi_A)) %>%
  mutate(crop_hhi_c = crop_hhi_A - mean(crop_hhi_A),
         luc_c      = luc_intensity_A - mean(luc_intensity_A))
cat("\nAHS households with HDDS = 0:", sum(primary$hdds == 0, na.rm = TRUE), "\n")

robust_outcomes <- c(hdds = "Dietary diversity (HDDS)", hdds_purch = "Purchased groups")

diet_robust <- imap(robust_outcomes, function(label, y) {
  list(
    "Primary"        = diet_models[[label]],
    "AHS weights"    = fit_cl(make_f(y), primary, weights = wt_ahs),
    "Season A only"  = fit_cl(make_f(y), primary_A),
    "Excl. HDDS = 0" = fit_cl(make_f(y), filter(primary, hdds > 0))
  )
})

iwalk(diet_robust, function(models, y) {
  cat("\nRobustness:", robust_outcomes[[y]], "\n")
  print(modelsummary(models, coef_map = KEY_LABELS[1:3], gof_map = "nobs",
                     stars = TRUE, output = "markdown"))
})

diet_ml <- map(names(robust_outcomes), ~ fit_ml(make_f(.x), primary)) %>%
  set_names(names(robust_outcomes))
iwalk(diet_ml, function(m, y) {
  cat("\nRandom-slope model:", y, " singular:", isSingular(m), "\n")
  print(summary(m)$coefficients[c("crop_hhi_c", "crop_hhi_c:luc_c"), ])
})

# ---- Which food groups drop? (descriptive) ---------------------
# One linear probability model per group: did the household eat
# the group at all over the four visits? Same specification as above.
hh_groups <- readRDS(file.path(processed_path, "hh_groups.rds"))
sd_hhi    <- sd(primary$crop_hhi)

group_models <- hh_groups %>%
  inner_join(primary, by = "hhid") %>%
  group_by(group) %>%
  group_map(function(d, key) {
    co <- summary(fit_cl(make_f("eaten"), d))$coefficients
    tibble(group        = key$group,
           share_eating = mean(d$eaten),
           estimate     = co["crop_hhi_c", "Estimate"],
           std_error    = co["crop_hhi_c", "Std. Error"],
           p_value      = co["crop_hhi_c", "Pr(>|t|)"],
           ci_low       = co["crop_hhi_c", "CI Lower"],
           ci_high      = co["crop_hhi_c", "CI Upper"],
           int_estimate = co["crop_hhi_c:luc_c", "Estimate"],
           int_p_value  = co["crop_hhi_c:luc_c", "Pr(>|t|)"])
  }) %>%
  bind_rows() %>%
  # percentage points per one-SD increase in HHI
  mutate(across(c(estimate, ci_low, ci_high), ~ 100 * .x * sd_hhi, .names = "{.col}_pp_1sd"))

cat("\nFood groups: change in probability of eating, pp per one-SD HHI\n")
print(group_models %>%
        select(group, share_eating, estimate_pp_1sd, ci_low_pp_1sd, ci_high_pp_1sd,
               p_value, int_estimate, int_p_value) %>%
        arrange(estimate_pp_1sd),
      n = Inf, width = Inf)

fig_groups <- group_models %>%
  mutate(group = fct_reorder(group, estimate_pp_1sd)) %>%
  ggplot(aes(x = estimate_pp_1sd, y = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = ci_low_pp_1sd, xmax = ci_high_pp_1sd), height = 0.2) +
  geom_point(size = 2) +
  labs(x = "Change in probability of eating the group (pp per one-SD increase in HHI, 95% CI)",
       y = NULL) +
  theme_classic(base_family = "serif")

ggsave(file.path(output_figures_path, "fig_food_groups.png"), fig_groups,
       width = 7, height = 4.5, dpi = 300)

# ---- Tables ---------------------------------------------------
diet_notes <- paste(TABLE_NOTES,
                    "Food groups counted over the four EICV7 consumption visits.")

modelsummary(
  hdds_steps, coef_map = KEY_LABELS, gof_map = c("nobs", "r.squared"),
  stars = TRUE,
  notes = paste(STEP_NOTES, "Food groups counted over the four EICV7 consumption visits."),
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
# Kept to six columns so it fits the page; the animal-source outcome
# is reported with the non-staple count in the diet quality table.
food_value_model <- readRDS(file.path(processed_path, "primary_models.rds"))$main_models[[
  "(3) + Land (primary)"]]

chain_models <- c(
  set_names(diet_models[1:5], c("Own-produced groups", "Purchased share",
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

# ---- Diet quality table ----------------------------------------
modelsummary(
  list("Non-staple groups (0-6)"    = diet_models[["Non-staple groups"]],
       "Animal-source groups (0-4)" = diet_models[["Animal-source groups"]]),
  coef_map = KEY_LABELS, gof_map = c("nobs", "r.squared"),
  stars = TRUE,
  notes = paste(diet_notes,
                "Animal-source groups: meat, eggs, fish, and milk and milk products."),
  title = "Crop concentration and diet quality",
  output = file.path(output_tables_path, "diet_quality.tex")
)

saveRDS(list(hdds_steps = hdds_steps, diet_models = diet_models,
             diet_marginal = diet_marginal, diet_robust = diet_robust,
             diet_ml = diet_ml, chain_models = chain_models,
             group_models = group_models),
        file.path(processed_path, "diet_models.rds"))