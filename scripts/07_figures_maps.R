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


