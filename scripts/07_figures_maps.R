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



# ==============================         Create one for 2019 data       ==============================

dist_luc_2019 <- readRDS(file.path(processed_path, "dist_luc_2019.rds"))
rwa_map  <- readRDS(file.path(processed_path, "rwa_map.rds"))

map_data <- rwa_map %>%
  left_join(dist_luc_2019 %>% mutate(district_code = as.character(district_code)),
            by = c("CC_2" = "district_code"))

p_luc_map <- ggplot(data = map_data) +
  geom_sf(aes(fill = luc_intensity), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Avg Annual % Land in LUC",
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "Rwanda: Annual Land Use Consolidation Intensity (2019)",
    subtitle = "Population-weighted estimates from SAS Seasons A, B & C",
    caption = "Source: NISR SAS 2019 Microdata"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_blank())

p_luc_map
ggsave(file.path(output_figures_path, "luc_intensity_map_2019.png"), p_luc_map,
       width = 8, height = 6, dpi = 300)