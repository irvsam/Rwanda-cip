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

# Exploring this data a bit

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

# so a 2% percentage-point increase in LUC intensity is associated with a ~1.08% decrease in food consumption per adult equivalent. 
# NB this is pretty significant, but the effect size is small. The interaction test below will show whether this effect is concentrated among poorer households.

analysis_data %>%
  arrange(desc(luc_intensity)) %>%
  head(10)  # What are the most high-LUC districts?

analysis_data %>%
  filter(luc_intensity > 15) %>%
  summarise(n = n(), mean_log_food = mean(log_food_ae))

analysis_data %>%
  filter(luc_intensity < 5) %>%
  summarise(n = n(), mean_log_food = mean(log_food_ae))


# so in lower LUC districts ave food consumption is 14 and in higher LUC districts it's 13.8, which is a difference of 0.2 in log food consumption per adult equivalent.
# Converting back: exp(0.2) - 1 = 0.22, meaning food consumption is about 22% higher in low-LUC districts than high-LUC districts for the poorest quintile.


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