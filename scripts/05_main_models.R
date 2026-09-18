# ============================================================
# 05_main_models.R
# Core test: does district-level LUC intensity affect household
# food welfare, controlling for wealth — and is that effect
# concentrated among poorer households? (Shock variable and
# shock interaction dropped.)
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

# Model 1: does LUC intensity predict food welfare, controlling for wealth?
# A negative, significant luc_intensity coefficient = higher-LUC districts
# have lower food consumption welfare, net of wealth quintile.

model_main <- lm(
  log_food_ae ~ luc_intensity + quintile_f + ur_f + province_f,
  data = analysis_data
)
summary(model_main)

# ============================================================
# Exploring the main effect (H1)
# ============================================================

analysis_data %>%
  filter(as.numeric(quintile_f) == 1) %>%  # poorest only
  ggplot(aes(x = luc_intensity, y = log_food_ae)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Quintile 1: LUC intensity vs. log food consumption",
       x = "District LUC intensity (%)",
       y = "Log food per adult equiv")

# If LUC goes from 8% to 10% (a 2 percentage-point increase):
exp(-0.00542 * 2) - 1  # What % does food consumption change?

# A 2 percentage-point increase in LUC intensity is associated with a
# ~1.08% decrease in food consumption per adult equivalent, holding
# quintile, urban/rural and province fixed. Statistically detectable,
# but a small effect size overall (whole-sample average effect).
# Whether this is bigger for poorer households is the H2 question,
# tested below via the interaction model and split-sample regressions.

analysis_data %>%
  arrange(desc(luc_intensity)) %>%
  head(10)  # What are the most high-LUC districts?

# ---- Whole-sample comparison: high- vs low-LUC districts, ALL quintiles ----
analysis_data %>%
  filter(luc_intensity > 15) %>%
  summarise(n = n(), mean_log_food = mean(log_food_ae))

analysis_data %>%
  filter(luc_intensity < 5) %>%
  summarise(n = n(), mean_log_food = mean(log_food_ae))

# Whole-sample: mean log food ~14.0 in low-LUC districts vs ~13.8 in
# high-LUC districts, a gap of ~0.2 log points.
# exp(0.2) - 1 ≈ 0.22 → food consumption is ~22% higher in low-LUC
# districts than high-LUC districts, across the FULL sample (not
# quintile-specific). This is the raw H1 pattern before controls.

# ---- Same comparison, restricted to the poorest quintile (Q1 only) ----
# This is the one that actually speaks to H2 — is the gap bigger here
# than in the whole-sample version above?
analysis_data %>%
  filter(as.numeric(quintile_f) == 1, luc_intensity > 15) %>%
  summarise(n = n(), mean_log_food = mean(log_food_ae))

analysis_data %>%
  filter(as.numeric(quintile_f) == 1, luc_intensity < 5) %>%
  summarise(n = n(), mean_log_food = mean(log_food_ae))

# Compare the resulting gap (and its exp(gap)-1 % conversion) against
# the whole-sample gap above. If Q1's gap is noticeably larger, that's
# raw descriptive support for H2, ahead of the formal interaction test.

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




# ============================================================
# clustered version

# Same models as before, but standard errors are now clustered
# by district_code — necessary because luc_intensity is measured
# at the district level, so households within a district are not
# independent observations w.r.t. that variable.
# ============================================================

library(estimatr)  # install.packages("estimatr") if needed

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

# Model 1: main effect, clustered SEs
model_main <- lm_robust(
  log_food_ae ~ luc_intensity + quintile_f + ur_f + province_f,
  data = analysis_data,
  clusters = district_code
)
summary(model_main)

# Model 2: interaction, clustered SEs
model_interaction <- lm_robust(
  log_food_ae ~ luc_intensity * quintile_f + ur_f + province_f,
  data = analysis_data,
  clusters = district_code
)
summary(model_interaction)

# Split-sample models, clustered SEs
models_by_quintile <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm_robust(
    log_food_ae ~ luc_intensity + ur_f + province_f,
    data = .x,
    clusters = district_code
  ))

map(models_by_quintile, summary)

# modelsummary() supports lm_robust objects directly
modelsummary(
  c(
    list("Main effect" = model_main, "Interaction" = model_interaction),
    models_by_quintile
  ),
  output = file.path(output_tables_path, "main_results_clustered.docx")
)




# ============================================================
# MECHANISM TEST
# Does LUC intensity affect an intermediate channel (own-production
# / self-provisioning), even if the headline consumption regression
# is underpowered with only 30 district clusters? This is a
# mechanism/pathway check, not a fishing expedition — one
# pre-specified intermediate outcome, reported regardless of result.
# ============================================================

library(estimatr)

source("scripts/00_setup.R")
source("scripts/01_load_eicv7.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

# ---- Build own-production variable ----
# Fix: households with zero valid s08a4_4 rows get NA, not -Inf.
# max(..., na.rm = TRUE) on an all-NA/empty vector silently returns
# -Inf, which can crash lm_robust()'s underlying C++ code rather than
# throwing a normal R error.
own_prod <- expenditure_C %>%
  select(hhid, s08a4_4) %>%
  group_by(hhid) %>%
  summarise(
    has_own_prod = if (all(is.na(s08a4_4))) NA_real_
    else max(as.numeric(s08a4_4) == 1, na.rm = TRUE),
    n_own_prod_items = sum(as.numeric(s08a4_4) == 1, na.rm = TRUE),
    n_missing        = sum(is.na(s08a4_4))
  )

# Check how many households this affects
sum(is.na(own_prod$has_own_prod))

# Merge into the analysis dataset
mechanism_data <- analysis_data %>%
  left_join(own_prod, by = "hhid") %>%
  filter(is.finite(has_own_prod))  # defensive: drop any NA/-Inf/NaN

nrow(mechanism_data)  # sanity check against analysis_data's n

# ---- Model A: does LUC predict whether a household has ANY own production? ----
model_mech_binary <- lm_robust(
  has_own_prod ~ luc_intensity + quintile_f + ur_f + province_f,
  data = mechanism_data,
  clusters = district_code
)
summary(model_mech_binary)

# ---- Model B: does LUC predict HOW MANY own-production items a household reports? ----
model_mech_count <- lm_robust(
  n_own_prod_items ~ luc_intensity + quintile_f + ur_f + province_f,
  data = mechanism_data,
  clusters = district_code
)
summary(model_mech_count)

# ---- Same question, split by quintile ----
mech_models_by_quintile <- mechanism_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(mechanism_data$quintile_f))) %>%
  map(~ lm_robust(
    has_own_prod ~ luc_intensity + ur_f + province_f,
    data = .x,
    clusters = district_code
  ))

map(mech_models_by_quintile, summary)

# ---- Export for the paper ----
modelsummary(
  c(
    list("Has own production" = model_mech_binary,
         "N own-production items" = model_mech_count),
    mech_models_by_quintile
  ),
  output = file.path(output_tables_path, "mechanism_results.docx")
)


# TODO: work on visualisations because they are not 100% perfect yet


# =============================================  visualising ==============================================
source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))
rwa_map <- readRDS(file.path(processed_path, "rwa_map.rds"))
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

# Refit the split-sample models (or load them if you saved them)
models_by_quintile <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm(log_food_ae ~ luc_intensity + ur_f + province_f, data = .x))

# Create a prediction grid: for each district's LUC intensity,
# predict food consumption for each quintile, holding ur_f and 
# province_f at their modal/mean values
pred_grid <- expand_grid(
  luc_intensity = seq(min(dist_luc$luc_intensity), 
                      max(dist_luc$luc_intensity), 
                      length.out = 30),
  ur_f = factor(1),           # urban = 1 (or use modal value)
  province_f = factor(1)      # province 1 (or use modal value)
)

# Generate predictions for each quintile
preds <- bind_rows(
  pred_grid %>% 
    mutate(
      quintile = "Q1",
      predicted_log_food = predict(models_by_quintile[[1]], newdata = pred_grid),
      predicted_food = exp(predicted_log_food)
    ),
  pred_grid %>% 
    mutate(
      quintile = "Q2",
      predicted_log_food = predict(models_by_quintile[[2]], newdata = pred_grid),
      predicted_food = exp(predicted_log_food)
    ),
  pred_grid %>% 
    mutate(
      quintile = "Q3",
      predicted_log_food = predict(models_by_quintile[[3]], newdata = pred_grid),
      predicted_food = exp(predicted_log_food)
    ),
  pred_grid %>% 
    mutate(
      quintile = "Q4",
      predicted_log_food = predict(models_by_quintile[[4]], newdata = pred_grid),
      predicted_food = exp(predicted_log_food)
    ),
  pred_grid %>% 
    mutate(
      quintile = "Q5",
      predicted_log_food = predict(models_by_quintile[[5]], newdata = pred_grid),
      predicted_food = exp(predicted_log_food)
    )
)

# Plot: predicted food consumption by LUC intensity, colored by quintile
ggplot(preds, aes(x = luc_intensity, y = predicted_food, color = quintile)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("Q1" = "#440154", "Q2" = "#31688e", "Q3" = "#35b779", 
               "Q4" = "#fde724", "Q5" = "#ff0000"),
    name = "Wealth quintile"
  ) +
  labs(
    title = "Predicted food consumption by LUC intensity and wealth",
    subtitle = "From regression models controlling for urban/rural and province",
    x = "District LUC intensity (%)",
    y = "Predicted food consumption per adult equiv (RWF)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(file.path(output_figures_path, "regression_predictions_by_quintile.png"),
       width = 10, height = 6, dpi = 300)