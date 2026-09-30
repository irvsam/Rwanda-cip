# 05d_priority_split.R ---------------------
# PRIMARY DESIGN. Crop concentration split into a priority-crop part and
# a non-priority part, each with its own district LUC interaction.
# Same sample, controls and CR2 inference as 05 and 05b (which now serve
# as the plain-HHI comparison).
#
# Primary outcome: diet quality
#   hdds_nonstaple : non-staple groups (veg, fruit, meat, eggs, fish, dairy), 0-6
#   hdds_asf       : animal-source groups (meat, eggs, fish, dairy), 0-4 (stricter check)
# Mechanism chain: hdds_own (link 1), purch_share (link 2), hdds_purch (link 3)
# Comparison: hdds (link to Del Prete et al., 2019), log food value (distal)
#
# Two versions of each model (identical fit):
#   split_models : priority slope and non-priority slope
#   diff_models  : priority MINUS non-priority (tests whether they differ)

if (!exists(".setup_done")) source("scripts/00_setup.R")
master  <- readRDS(file.path(processed_path, "master.rds"))
primary <- make_primary(master)

cat("Primary sample:", nrow(primary), "households in",
    n_distinct(primary$district_code), "districts\n")

primary %>%
  summarise(across(c(hhi_prio, hhi_nonprio), list(mean = mean, sd = sd)),
            r = cor(hhi_prio, hhi_nonprio)) %>%
  print(width = Inf)

split_outcomes <- c(
  "Own-produced groups"  = "hdds_own",
  "Purchased share"      = "purch_share",
  "Purchased groups"     = "hdds_purch",
  "Non-staple groups"    = "hdds_nonstaple",
  "Animal-source groups" = "hdds_asf",
  "HDDS"                 = "hdds",
  "Log food value"       = "log_food_ae_real"
)

# ---- Models ---------------------------------------------------
split_models <- map(split_outcomes, ~ fit_cl(make_f_split(.x), primary))
diff_models  <- map(split_outcomes, ~ fit_cl(make_f_diff(.x),  primary))

# term names must match the labels exactly (see SPLIT_TERMS in 00_setup)
stopifnot(all(names(SPLIT_LABELS)[names(SPLIT_LABELS) != "log_land"] %in%
                names(coef(split_models[[1]]))),
          all(names(DIFF_LABELS) %in% names(coef(diff_models[[1]]))))

walk2(split_models, names(split_models), function(m, nm) {
  cat("\n", nm, "(split)\n")
  print(summary(m)$coefficients[SPLIT_TERMS[-3], ])
})

walk2(diff_models, names(diff_models), function(m, nm) {
  cat("\n", nm, "(difference: priority minus non-priority)\n")
  print(summary(m)$coefficients[names(DIFF_LABELS), ])
})

# ---- Stepwise for the primary outcome --------------------------
nonstaple_steps <- list(
  "(1) Geography"        = fit_cl(make_f_split("hdds_nonstaple", CONTROLS_GEO), primary),
  "(2) + Household"      = fit_cl(make_f_split("hdds_nonstaple",
                                               c(CONTROLS_GEO, CONTROLS_HH)), primary),
  "(3) + Land (primary)" = split_models[["Non-staple groups"]]
)
walk2(nonstaple_steps, names(nonstaple_steps), function(m, nm) {
  cat("\n Non-staple stepwise", nm, "\n")
  print(summary(m)$coefficients[SPLIT_TERMS[-3], ])
})

# ---- Marginal effects: each part at p10 / p50 / p90 LUC -----------
# Per one-SD increase in that part; CR2 with Satterthwaite df
split_marginal <- map_dfr(split_outcomes, ~ slope_at_split(.x, primary)) %>%
  mutate(outcome = names(split_outcomes)[match(outcome, split_outcomes)])

print(split_marginal %>%
        select(outcome, part, luc_pctile, luc_intensity, effect_1sd, low_1sd, high_1sd, df),
      n = Inf, width = Inf)

# ---- Tables ---------------------------------------------------
diet_notes <- paste(SPLIT_NOTES,
                    "Food groups counted over the four EICV7 consumption visits.")

# Main table: mechanism chain and diet quality
modelsummary(
  split_models[c("Own-produced groups", "Purchased share", "Purchased groups",
                 "Non-staple groups", "Animal-source groups")],
  coef_map = SPLIT_LABELS, gof_map = c("nobs", "r.squared"),
  stars = TRUE, notes = diet_notes,
  title = "Priority-crop concentration and household diets along the mechanism",
  output = file.path(output_tables_path, "main_split.tex")
)

# Comparison outcomes: HDDS and food value
modelsummary(
  split_models[c("HDDS", "Log food value")],
  coef_map = SPLIT_LABELS, gof_map = c("nobs", "r.squared"),
  stars = TRUE,
  notes = paste(diet_notes,
                "Log food value: food consumption per adult equivalent, Jan 2024 prices."),
  title = "Priority-crop concentration, dietary diversity and food value",
  output = file.path(output_tables_path, "comparison_split.tex")
)

# Difference tests (same models, reparametrised)
modelsummary(
  diff_models, coef_map = DIFF_LABELS, gof_map = "nobs",
  stars = TRUE,
  notes = paste("Each column re-estimates the model in the main tables with HHI and its priority part",
                "in place of the two parts; the coefficients shown are the priority slope minus the",
                "non-priority slope, and the same difference for the LUC interaction.",
                "CR2 standard errors clustered by district (30 clusters) in parentheses."),
  title = "Difference between priority and non-priority concentration",
  output = file.path(output_tables_path, "diff_split.tex")
)
# Stepwise (appendix)
modelsummary(
  nonstaple_steps, coef_map = SPLIT_LABELS, gof_map = c("nobs", "r.squared"),
  stars = TRUE,
  notes = paste(
    "CR2 standard errors clustered by district (30 clusters) in parentheses.",
    "(1) urban/rural and province fixed effects; (2) adds household size, dependency",
    "ratio and head age, sex and education; (3) adds log land held.",
    "Crop concentration (HHI, Seasons A and B) is split into the part from CIP priority crops",
    "and the part from all other crops; the two sum to the HHI.",
    "Both parts and LUC intensity are centred on their sample means.",
    "Reference education category: never attended.",
    "Food groups counted over the four EICV7 consumption visits."
  ),
  title = "Priority-crop concentration and non-staple food groups, stepwise",
  output = file.path(output_tables_path, "nonstaple_steps.tex")
)

saveRDS(list(split_models = split_models, diff_models = diff_models,
             nonstaple_steps = nonstaple_steps, split_marginal = split_marginal),
        file.path(processed_path, "split_models.rds"))