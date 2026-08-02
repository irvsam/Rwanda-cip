# ============================================================
# 05_main_models.R
# Core test: does district-level LUC intensity affect household
# food welfare, controlling for wealth — and is that effect
# concentrated among poorer households? (Shock variable and
# shock interaction dropped.)
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))
str(analysis_data)
head(analysis_data)
colnames(analysis_data)

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

ana %>%
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


rwa_map <- readRDS(file.path(processed_path, "rwa_map.rds"))
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

# Aggregate food consumption to district level, by wealth quintile
dist_food_by_quintile <- analysis_data %>%
  group_by(district_code, quintile_f) %>%
  summarise(
    mean_food_ae = mean(food, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Join LUC intensity
dist_food_by_quintile <- dist_food_by_quintile %>%
  left_join(dist_luc %>% select(district_code, luc_intensity), by = "district_code")

# Join to map — one row per district-quintile combo
# change cc2 to double from character
rwa_map$CC_2 <- as.numeric(rwa_map$CC_2)

map_data_quintile <- rwa_map %>%
  left_join(
    dist_food_by_quintile,
    by = c("CC_2" = "district_code")
  )

# Create a faceted map: one panel per quintile
ggplot(map_data_quintile) +
  geom_sf(aes(fill = mean_food_ae), color = "white", size = 0.05) +
  facet_wrap(~ quintile_f, nrow = 2, labeller = labeller(
    quintile_f = c("1" = "Q1 (Poorest)", "2" = "Q2", "3" = "Q3 (Middle)", 
                   "4" = "Q4", "5" = "Q5 (Richest)")
  )) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Mean food consumption\nper adult equiv (RWF)",
    limits = c(
      min(dist_food_by_quintile$mean_food_ae, na.rm = TRUE),
      max(dist_food_by_quintile$mean_food_ae, na.rm = TRUE)
    ),
    trans = "sqrt"  # square-root scale stretches small differences
  ) +
  labs(
    title = "Rwanda: Food consumption by district, quintile, and LUC intensity",
    subtitle = "EICV7 2023/24 — note poorest (Q1) consistently lower; richest (Q5) consistently higher"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold")
  )

ggsave(file.path(output_figures_path, "food_by_quintile_map.png"),
       width = 14, height = 10, dpi = 300)
