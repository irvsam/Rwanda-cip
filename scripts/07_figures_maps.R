# ============================================================
# 07_figures_maps.R
# Visual output for Results section
# ============================================================

source("scripts/00_setup.R")
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))
rwa_map  <- readRDS(file.path(processed_path, "rwa_map.rds"))

map_data <- rwa_map %>%
  left_join(dist_luc %>% mutate(district_code = as.character(district_code)),
            by = c("CC_2" = "district_code"))

p_luc_map <- ggplot(data = map_data) +
  geom_sf(aes(fill = luc_intensity), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Avg Annual % Land in LUC",
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "Rwanda: Annual Land Use Consolidation Intensity (2023/24)",
    subtitle = "Population-weighted estimates from SAS Seasons A, B & C",
    caption = "Source: NISR SAS 2023/24 Microdata"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_blank())

p_luc_map
ggsave(file.path(output_figures_path, "luc_intensity_map.png"), p_luc_map,
       width = 8, height = 6, dpi = 300)
print(p_luc_map)


# ===== MAP 2: Mean food consumption by district (Q1 poorest only) =====

analysis_data <- readRDS(file.path(processed_path, "analysis_data_refined.rds"))


q1_food_by_dist <- analysis_data %>%
  filter(as.numeric(quintile_f) == 1) %>%
  group_by(district_code) %>%
  summarise(
    mean_food = mean(food, na.rm = TRUE),
    n = n()
  )

map_food_q1 <- rwa_map %>%
  left_join(q1_food_by_dist %>% mutate(district_code = as.character(district_code)),
            by = c("CC_2" = "district_code"))

p2_food_q1 <- ggplot(map_food_q1) +
  geom_sf(aes(fill = mean_food / 1e6), color = "white", size = 0.2) +  # convert to millions
  scale_fill_viridis_c(
    option = "viridis",
    name = "Food spending\n(millions RWF)",
    direction = 1
  ) +
  labs(
    title = "Rwanda: Food Consumption by District (Poorest Quintile)",
    subtitle = "Mean food spending per adult equivalent (Q1 households only)",
    caption = "Source: EICV7 2023/24"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

ggsave(file.path(output_figures_path, "food_q1_by_district.png"),
       p2_food_q1, width = 10, height = 8, dpi = 300)

print(p2_food_q1)

# ============================================================
# 10_results_visualizations.R
# Publication-quality figures for main findings
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data_refined.rds"))

# ===== FIGURE 1: Regression coefficients by quintile =====
# Shows the core finding: LUC penalty is concentrated in Q1-Q2

models_by_quintile <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm(log_food_ae ~ luc_intensity + ur_f + province_f, data = .x))

# Extract coefficients
coef_data <- tibble(
  quintile = c("Q1\n(Poorest)", "Q2", "Q3\n(Middle)", "Q4", "Q5\n(Richest)"),
  estimate = c(
    coef(models_by_quintile[[1]])["luc_intensity"],
    coef(models_by_quintile[[2]])["luc_intensity"],
    coef(models_by_quintile[[3]])["luc_intensity"],
    coef(models_by_quintile[[4]])["luc_intensity"],
    coef(models_by_quintile[[5]])["luc_intensity"]
  ),
  se = c(
    summary(models_by_quintile[[1]])$coefficients["luc_intensity", "Std. Error"],
    summary(models_by_quintile[[2]])$coefficients["luc_intensity", "Std. Error"],
    summary(models_by_quintile[[3]])$coefficients["luc_intensity", "Std. Error"],
    summary(models_by_quintile[[4]])$coefficients["luc_intensity", "Std. Error"],
    summary(models_by_quintile[[5]])$coefficients["luc_intensity", "Std. Error"]
  ),
  pvalue = c(
    summary(models_by_quintile[[1]])$coefficients["luc_intensity", "Pr(>|t|)"],
    summary(models_by_quintile[[2]])$coefficients["luc_intensity", "Pr(>|t|)"],
    summary(models_by_quintile[[3]])$coefficients["luc_intensity", "Pr(>|t|)"],
    summary(models_by_quintile[[4]])$coefficients["luc_intensity", "Pr(>|t|)"],
    summary(models_by_quintile[[5]])$coefficients["luc_intensity", "Pr(>|t|)"]
  )
) %>%
  mutate(
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se,
    significant = pvalue < 0.05
  )

p1_coef <- ggplot(coef_data, aes(x = reorder(quintile, -estimate), y = estimate, color = significant)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  scale_color_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#95a5a6"),
    name = "Significant\n(p < 0.05)",
    labels = c("TRUE" = "Yes", "FALSE" = "No")
  ) +
  labs(
    title = "LUC Intensity Effect on Food Consumption by Wealth Quintile",
    subtitle = "Coefficient ± 95% CI. Negative = lower food consumption.",
    x = "Wealth Quintile",
    y = "Coefficient (log scale)",
    caption = "Source: EICV7 2023/24, controlling for urban/rural and province"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.title = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(output_figures_path, "10_coefficients_by_quintile.png"),
       p1_coef, width = 10, height = 6, dpi = 300)

print(p1_coef)



# ===== FIGURE 2: Predicted food consumption by LUC and quintile (% change from baseline) =====
# Shows what the regression predicts as percentage change from 0% LUC baseline

pred_grid <- expand_grid(
  luc_intensity = seq(min(analysis_data$luc_intensity, na.rm = TRUE),
                      max(analysis_data$luc_intensity, na.rm = TRUE),
                      length.out = 50),
  quintile = 1:5,
  ur_f = factor(1),
  province_f = factor(1)
)

# Generate predictions for each quintile
preds <- tibble()
for (q in 1:5) {
  pred_data <- pred_grid %>% filter(quintile == q) %>%
    mutate(quintile_f = factor(q))
  
  pred_vals <- predict(models_by_quintile[[q]],
                       newdata = pred_data %>% select(-quintile),
                       se.fit = FALSE)
  
  # Get baseline prediction at 0% LUC
  baseline_data <- pred_data %>% slice(1) %>% mutate(luc_intensity = 0)
  baseline_val <- predict(models_by_quintile[[q]],
                          newdata = baseline_data %>% select(-quintile),
                          se.fit = FALSE)[1]
  
  preds <- bind_rows(preds,
                     pred_data %>%
                       mutate(
                         quintile_label = c("Q1 (Poorest)", "Q2", "Q3 (Middle)", "Q4", "Q5 (Richest)")[q],
                         predicted_log_food = pred_vals,
                         predicted_food = exp(pred_vals),
                         baseline_food = exp(baseline_val),
                         pct_change = ((exp(pred_vals) - exp(baseline_val)) / exp(baseline_val)) * 100
                       )
  )
}

p2_pred <- ggplot(preds, aes(x = luc_intensity, y = pct_change, color = quintile_label, linetype = quintile_label)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  scale_color_manual(
    values = c(
      "Q1 (Poorest)" = "#e74c3c",
      "Q2" = "#e59866",
      "Q3 (Middle)" = "#f9e79f",
      "Q4" = "#a9dfbf",
      "Q5 (Richest)" = "#52be80"
    ),
    name = "Wealth Quintile"
  ) +
  scale_linetype_manual(
    values = c(
      "Q1 (Poorest)" = "solid",
      "Q2" = "solid",
      "Q3 (Middle)" = "dashed",
      "Q4" = "solid",
      "Q5 (Richest)" = "solid"
    ),
    guide = "none"
  ) +
  labs(
    title = "Effect of LUC Intensity on Food Consumption by Wealth",
    subtitle = "Percentage change from 0% LUC baseline (controlling for urban/rural and province)",
    x = "District LUC Intensity (%)",
    y = "% Change in Food Consumption",
    caption = "Poorest (Q1) lose ~5% for every 10% increase in LUC; richest (Q5) unaffected"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.title = element_text(size = 10),
    legend.position = "right"
  )

ggsave(file.path(output_figures_path, "11_predicted_consumption_by_quintile_pct.png"),
       p2_pred, width = 10, height = 6, dpi = 300)

print(p2_pred)


cat("All visualizations saved to", output_figures_path, "\n")

