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

# =================================================================================================


analysis_data <- readRDS(file.path(processed_path, "analysis_data_refined.rds"))


# ===== MAP 2: Mean food consumption by district (Q1 poorest only) =====
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

ggsave(file.path(output_figures_path, "02_food_q1_by_district.png"),
       p2_food_q1, width = 10, height = 8, dpi = 300)

print(p2_food_q1)

# ===== MAP 3: Poverty rate by district =====
poverty_by_dist <- analysis_data %>%
  group_by(district_code) %>%
  summarise(
    pct_poor = mean(is_poor, na.rm = TRUE) * 100,
    n = n()
  )

map_poverty <- rwa_map %>%
  left_join(poverty_by_dist %>% mutate(district_code = as.character(district_code)),
            by = c("CC_2" = "district_code"))

p3_poverty <- ggplot(map_poverty) +
  geom_sf(aes(fill = pct_poor), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "Poverty Rate (%)",
    limits = c(0, 100)
  ) +
  labs(
    title = "Rwanda: Poverty Rate by District",
    subtitle = "% of households below poverty line",
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

ggsave(file.path(output_figures_path, "03_poverty_by_district.png"),
       p3_poverty, width = 10, height = 8, dpi = 300)

print(p3_poverty)

# ===== MAP 4: The interaction — LUC vs Food (Q1) =====
# Merge LUC and Q1 food onto map
interaction_data <- dist_luc %>%
  left_join(q1_food_by_dist, by = "district_code") %>%
  mutate(district_code = as.character(district_code))

map_interaction <- rwa_map %>%
  left_join(interaction_data, by = c("CC_2" = "district_code"))

# Color by LUC, size by food consumption (reverse scale so small = more consolidated, lower food)
p4_interaction <- ggplot(map_interaction) +
  geom_sf(aes(fill = luc_intensity, size = mean_food), color = "white", stroke = 0.3) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "LUC intensity (%)"
  ) +
  scale_size_continuous(
    name = "Food spending\n(poorest HH)",
    range = c(1, 4)
  ) +
  labs(
    title = "Rwanda: Land Consolidation & Food Consumption (Q1)",
    subtitle = "Color = LUC intensity | Size = Food spending of poorest households",
    caption = "Darker & smaller districts = more consolidation, less food consumption"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

ggsave(file.path(output_figures_path, "04_interaction_luc_food_q1.png"),
       p4_interaction, width = 10, height = 8, dpi = 300)

print(p4_interaction)

# ===== MAP 5: Difference between Q1 and Q5 (inequality) =====
q5_food_by_dist <- analysis_data %>%
  filter(as.numeric(quintile_f) == 5) %>%
  group_by(district_code) %>%
  summarise(mean_food_q5 = mean(food, na.rm = TRUE))

inequality_data <- q1_food_by_dist %>%
  left_join(q5_food_by_dist, by = "district_code") %>%
  mutate(
    inequality_gap = mean_food_q5 - mean_food,
    district_code = as.character(district_code)
  )

map_inequality <- rwa_map %>%
  left_join(inequality_data, by = c("CC_2" = "district_code"))

p5_inequality <- ggplot(map_inequality) +
  geom_sf(aes(fill = inequality_gap / 1e6), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "cividis",
    name = "Q5–Q1 Gap\n(millions RWF)",
    direction = 1
  ) +
  labs(
    title = "Rwanda: Food Consumption Inequality by District",
    subtitle = "Gap between richest (Q5) and poorest (Q1) households",
    caption = "Darker = bigger inequality"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

ggsave(file.path(output_figures_path, "05_inequality_gap_by_district.png"),
       p5_inequality, width = 10, height = 8, dpi = 300)

print(p5_inequality)

cat("All maps saved to", output_figures_path, "\n")

