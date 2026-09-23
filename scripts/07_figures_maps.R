# ============================================================
# 07_figures.R
# Visual output for the Results section.
#
# ACADEMIC STYLE
#
# Figures use CLUSTERED models throughout.
# ============================================================

source("scripts/00_setup.R")
analysis_data   <- readRDS(file.path(processed_path, "analysis_data.rds"))
dist_luc        <- readRDS(file.path(processed_path, "dist_luc.rds"))
rwa_map         <- readRDS(file.path(processed_path, "rwa_map.rds"))

# A clean, print-friendly, serif-font base theme reused across every figure.
theme_paper <- theme_classic(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# ============================================================
# MAP 1: District-level LUC intensity
# ============================================================

map_data <- rwa_map %>%
  left_join(dist_luc %>% mutate(district_code = as.character(district_code)),
            by = c("CC_2" = "district_code"))

p_luc_map <- ggplot(map_data) +
  geom_sf(aes(fill = luc_intensity), color = "white", linewidth = 0.1) +
  scale_fill_gradient(low = "grey90", high = "black",
                      name = "LUC intensity (%)") +
  theme_void(base_size = 13, base_family = "serif") +
  theme(legend.position = "bottom")

ggsave(file.path(output_figures_path, "luc_intensity_map.png"), p_luc_map,
       width = 8, height = 6, dpi = 300)

# ============================================================
# MAP 2: Mean food consumption by district, poorest quintile
# ============================================================

q1_food_by_dist <- analysis_data %>%
  filter(as.numeric(quintile_f) == 1) %>%
  group_by(district_code) %>%
  summarise(mean_food = mean(food, na.rm = TRUE), n = n())

map_food_q1 <- rwa_map %>%
  left_join(q1_food_by_dist %>% mutate(district_code = as.character(district_code)),
            by = c("CC_2" = "district_code"))

p_food_q1 <- ggplot(map_food_q1) +
  geom_sf(aes(fill = mean_food / 1e6), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "grey90", high = "black",
                      name = "Food spending\n(millions RWF)") +
  theme_void(base_size = 13, base_family = "serif") +
  theme(legend.position = "bottom")

ggsave(file.path(output_figures_path, "food_q1_by_district.png"), p_food_q1,
       width = 10, height = 8, dpi = 300)

# ============================================================
# FIGURE 1: District-level LUC coefficient by quintile (CLUSTERED)
# Significance shown via shape (filled vs. open point) -- already
# greyscale-safe, no color used.
# ============================================================

models_by_quintile <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm_robust(log_food_ae ~ luc_intensity + ur_f + province_f,
                  data = .x, clusters = district_code))

coef_data <- map2_dfr(models_by_quintile, names(models_by_quintile), function(m, q) {
  s <- summary(m)$coefficients["luc_intensity", ]
  tibble(
    quintile = q,
    estimate = s["Estimate"],
    se       = s["Std. Error"],
    pvalue   = s["Pr(>|t|)"]
  )
}) %>%
  mutate(
    quintile_label = recode(quintile,
                            "1" = "Q1\n(Poorest)", "2" = "Q2", "3" = "Q3\n(Middle)",
                            "4" = "Q4", "5" = "Q5\n(Richest)"),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se,
    significant = ifelse(pvalue < 0.05, "p < 0.05", "n.s.")
  )

p1_coef <- ggplot(coef_data, aes(x = reorder(quintile_label, -estimate), y = estimate,
                                 shape = significant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3,
                linewidth = 0.6, color = "black") +
  geom_point(size = 3.5, fill = "black", color = "black") +
  scale_shape_manual(values = c("p < 0.05" = 16, "n.s." = 21), name = NULL) +
  labs(x = "Wealth quintile", y = "Coefficient on LUC intensity (log scale)") +
  theme_paper

ggsave(file.path(output_figures_path, "fig1_coefficients_by_quintile.png"), p1_coef,
       width = 8, height = 5.5, dpi = 300)

# ============================================================
# FIGURE 2: Predicted food consumption by LUC intensity and quintile
# (district-level, secondary analysis)
# Grey shade AND linetype both map to quintile.
# ============================================================

pred_grid <- expand_grid(
  luc_intensity = seq(min(dist_luc$luc_intensity), max(dist_luc$luc_intensity), length.out = 30),
  ur_f = factor(1), province_f = factor(1)
)

preds <- map2_dfr(models_by_quintile, names(models_by_quintile), function(m, q) {
  pred_grid %>%
    mutate(
      quintile = paste0("Q", q),
      predicted_log_food = predict(m, newdata = pred_grid),
      predicted_food = exp(predicted_log_food)
    )
})

p2_pred <- ggplot(preds, aes(x = luc_intensity, y = predicted_food,
                             color = quintile, linetype = quintile)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = c("Q1" = "black", "Q2" = "grey25", "Q3" = "grey45",
               "Q4" = "grey65", "Q5" = "grey85"),
    name = "Wealth quintile"
  ) +
  scale_linetype_manual(
    values = c("Q1" = "solid", "Q2" = "dashed", "Q3" = "dotted",
               "Q4" = "dotdash", "Q5" = "longdash"),
    name = "Wealth quintile"
  ) +
  labs(x = "District LUC intensity (%)",
       y = "Predicted food consumption per adult equiv. (RWF)") +
  theme_paper

ggsave(file.path(output_figures_path, "fig2_predicted_consumption_by_quintile.png"), p2_pred,
       width = 8, height = 5.5, dpi = 300)

# ============================================================
# FIGURE 3: PRIMARY RESULT. Household-level crop concentration
# effect, moderated by district-level LUC intensity.
# Grey shade AND linetype both map to district context.
# ============================================================

extension_data <- readRDS(file.path(processed_path, "extension_data.rds"))
model_ext_main <- lm_robust(
  log_food_ae ~ crop_hhi_c * luc_intensity_c + quintile_f + ur_f + province_f,
  data = extension_data, clusters = district_code
)

luc_levels <- quantile(extension_data$luc_intensity_c, c(0.1, 0.5, 0.9), na.rm = TRUE)

ext_pred_grid <- expand_grid(
  crop_hhi_c = seq(min(extension_data$crop_hhi_c), max(extension_data$crop_hhi_c), length.out = 30),
  luc_intensity_c = luc_levels,
  quintile_f = factor(levels(extension_data$quintile_f)[1], levels = levels(extension_data$quintile_f)),
  ur_f = factor(levels(extension_data$ur_f)[1], levels = levels(extension_data$ur_f)),
  province_f = factor(levels(extension_data$province_f)[1], levels = levels(extension_data$province_f))
) %>%
  mutate(
    luc_label = factor(case_when(
      luc_intensity_c == luc_levels[1] ~ "Low-LUC district",
      luc_intensity_c == luc_levels[2] ~ "Median-LUC district",
      luc_intensity_c == luc_levels[3] ~ "High-LUC district"
    ), levels = c("Low-LUC district", "Median-LUC district", "High-LUC district")),
    predicted_log_food = predict(model_ext_main, newdata = .),
    predicted_food = exp(predicted_log_food)
  )

p3_extension <- ggplot(ext_pred_grid, aes(x = crop_hhi_c, y = predicted_food,
                                          color = luc_label, linetype = luc_label)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = c("Low-LUC district" = "grey70",
               "Median-LUC district" = "grey40",
               "High-LUC district" = "black"),
    name = "District context"
  ) +
  scale_linetype_manual(
    values = c("Low-LUC district" = "solid", "Median-LUC district" = "dashed",
               "High-LUC district" = "dotted"),
    name = "District context"
  ) +
  labs(x = "Household crop concentration (centered HHI)",
       y = "Predicted food consumption per adult equiv. (RWF)") +
  theme_paper

ggsave(file.path(output_figures_path, "fig3_crop_hhi_by_luc.png"), p3_extension,
       width = 8, height = 5.5, dpi = 300)


# ============================================================
# Generate the results table directly in R -- reproducible, and
# avoids hand-transcribing coefficients into LaTeX.
# ============================================================



models_list <- list(
  "Main + interaction (LUC)" = model_ext_main,
  "Quintile interaction"     = model_ext_hhi_quintile,
  "Wealth-group interaction" = model_wealth_interaction
)

cm <- c(
  "crop_hhi_c:luc_intensity_c" = "crop_hhi_c TIMES luc_intensity_c",
  "crop_hhi:quintile_f2" = "crop_hhi TIMES quintile_f2",
  "crop_hhi:quintile_f3" = "crop_hhi TIMES quintile_f3",
  "crop_hhi:quintile_f4" = "crop_hhi TIMES quintile_f4",
  "crop_hhi:quintile_f5" = "crop_hhi TIMES quintile_f5",
  "wealth_groupWealthier (Q3-Q5)" = "wealth_group: Wealthier",
  "crop_hhi_c:wealth_groupWealthier (Q3-Q5)" = "crop_hhi_c TIMES wealth_group"
)

modelsummary(
  models_list,
  output    = file.path(output_tables_path, "extension_results.tex"),
  coef_rename = cm,
  coef_omit = "province_f",
  gof_omit  = "Log.Lik|F|RMSE",
  stars = TRUE,
  title = "Household-Level Crop Concentration and Food Consumption",
  add_rows = data.frame(term = "Province FE", m1 = "Yes", m2 = "Yes", m3 = "Yes"),
  # Use a plain-text placeholder for "crop_hhi" too -- notes text is
  # NOT auto-escaped, so any underscore or backslash here causes
  # problems. Swap it for the real name in post-processing instead.
  notes = c(
    "Standard errors clustered by district in parentheses. Columns 1 and 3 use CROPHHI centred on its sample mean; Column 2 uses the uncentred variable, so its CROPHHI coefficient reflects the effect within Q1 (the reference quintile) only. Wealthier refers to the Q3-Q5 group, with poorer (Q1-Q2) as the reference category.",
    "Source: EICV7 (2023/24) and AHS (2024), NISR."
  )
)

# ---- Automated post-processing ----
tex_path <- file.path(output_tables_path, "extension_results.tex")
tex <- paste(readLines(tex_path, warn = FALSE), collapse = "\n")

tex <- gsub("TIMES", "$\\times$", tex, fixed = TRUE)
tex <- gsub("CROPHHI", "crop\\_hhi", tex, fixed = TRUE)
tex <- gsub("Source: EICV7", "\\textit{Source:} EICV7", tex, fixed = TRUE)
tex <- gsub("district\\_code", "district", tex, fixed = TRUE)

tex <- sub(
  "\\caption{Household-Level Crop Concentration and Food Consumption}",
  "\\caption{Household-Level Crop Concentration and Food Consumption}\n\\label{tab:extension}",
  tex, fixed = TRUE
)

tex <- sub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{%\n\\begin{tabular}", tex, fixed = TRUE)
tex <- sub("\\end{tabular}", "\\end{tabular}%\n}", tex, fixed = TRUE)

writeLines(tex, tex_path)
message("Table generated and fixed: ", tex_path)
message("All figures and tables generated successfully.")


# ============================================================
# FIGURE 1: District-level LUC coefficient by quintile,
# naive OLS vs. clustered (CR2) standard errors.
# Shape encodes BOTH model type and significance (greyscale-safe,
# no colour needed): filled = significant, open = not; circle =
# naive, triangle = clustered.
# ============================================================

models_by_quintile_naive <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm(log_food_ae ~ luc_intensity + ur_f + province_f, data = .x))

models_by_quintile <- analysis_data %>%
  group_split(quintile_f) %>%
  set_names(sort(unique(analysis_data$quintile_f))) %>%
  map(~ lm_robust(log_food_ae ~ luc_intensity + ur_f + province_f,
                  data = .x, clusters = district_code))

extract_naive <- function(m, q) {
  s <- summary(m)$coefficients["luc_intensity", ]
  tibble(quintile = q, model = "Naive OLS",
         estimate = s["Estimate"], se = s["Std. Error"], pvalue = s["Pr(>|t|)"])
}

extract_clustered <- function(m, q) {
  s <- summary(m)$coefficients["luc_intensity", ]
  tibble(quintile = q, model = "Clustered (CR2)",
         estimate = s["Estimate"], se = s["Std. Error"], pvalue = s["Pr(>|t|)"])
}

coef_data <- bind_rows(
  map2_dfr(models_by_quintile_naive, names(models_by_quintile_naive), extract_naive),
  map2_dfr(models_by_quintile, names(models_by_quintile), extract_clustered)
) %>%
  mutate(
    quintile_label = recode(quintile,
                            "1" = "Q1\n(Poorest)", "2" = "Q2", "3" = "Q3\n(Middle)",
                            "4" = "Q4", "5" = "Q5\n(Richest)"),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se,
    significant = ifelse(pvalue < 0.05, "sig", "ns"),
    shape_group = case_when(
      model == "Naive OLS" & significant == "sig"       ~ "Naive, p < 0.05",
      model == "Naive OLS" & significant == "ns"         ~ "Naive, n.s.",
      model == "Clustered (CR2)" & significant == "sig"  ~ "Clustered, p < 0.05",
      model == "Clustered (CR2)" & significant == "ns"   ~ "Clustered, n.s."
    ),
    shape_group = factor(shape_group, levels = c(
      "Naive, p < 0.05", "Naive, n.s.",
      "Clustered, p < 0.05", "Clustered, n.s."
    ))
  )

p1_coef <- ggplot(coef_data, aes(x = quintile_label, y = estimate,
                                 shape = shape_group,
                                 group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.5),
                width = 0.25, linewidth = 0.6, color = "black") +
  geom_point(position = position_dodge(width = 0.5), size = 3.2,
             fill = "black", color = "black") +
  scale_shape_manual(
    values = c("Naive, p < 0.05" = 16, "Naive, n.s." = 1,
               "Clustered, p < 0.05" = 17, "Clustered, n.s." = 2),
    name = NULL
  ) +
  labs(x = "Wealth quintile", y = "Coefficient on LUC intensity (log scale)") +
  theme_paper

ggsave(file.path(output_figures_path, "fig1_coefficients_by_quintile.png"), p1_coef,
       width = 8.5, height = 5.5, dpi = 300)

