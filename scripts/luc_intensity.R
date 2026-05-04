# Load libraries
library(haven)
library(tidyverse)
library(sf) 
library(labelled)
library(geodata)

# Load and Collapse SAS data to District Level
sas_data <- read_dta("data/SAS 2024/Season A/Rwa_raw_SeasonA2024_Screening.dta")

# Print out the column names to check for the correct ones
print(colnames(sas_data))

# Shows column names alongside their descriptive labels
View(labelled::look_for(sas_data))



# Calculate Intensity per District
dist_cip_summary <- sas_data %>%
  group_by(s1q2) %>% # Group by District
  summarise(
    total_ha = sum(Plot_size_ha, na.rm = TRUE),
    luc_ha = sum(Plot_size_ha[s2q14 == 1], na.rm = TRUE), # Sum area where s2q14 is 'Yes'
    cip_intensity = (luc_ha / total_ha) * 100
  )

# View the 30 rows (one for each district)
print(dist_cip_summary)


# Add district names for merging with the map


# Get Rwanda District Map
rwa_map <- gadm(country = "RWA", level = 2, path = tempdir()) %>% st_as_sf()


# ======================= WITH ALL 3 SEASONS =========================

# Load all three 
sas_a <- read_dta("data/SAS 2025/Season A/Rwa_raw_SeasonA2025_Screening.dta")
sas_b <- read_dta("data/SAS 2025/Season B/Rwa_raw_SeasonB2025_Screening.dta")
sas_c <- read_dta("data/SAS 2025/Season C/Rwa_raw_SeasonC2025_Screening.dta")

# Add a season indicator to each 
sas_a$season <- "A"
sas_b$season <- "B"
sas_c$season <- "C"

# Stack the datasets
sas_annual <- bind_rows(sas_a, sas_b, sas_c)

# Calculate Annual Intensity per District
dist_annual_summary <- sas_annual %>%
  group_by(s1q2) %>% 
  summarise(
    total_ha = sum(Plot_size_ha, na.rm = TRUE),
    luc_ha = sum(Plot_size_ha[s2q14 == 1], na.rm = TRUE), 
    cip_intensity = (luc_ha / total_ha) * 100
  ) %>%
  mutate(s1q2 = as.character(s1q2))

# Merge with Map
map_data_annual <- rwa_map %>%
  left_join(dist_annual_summary, by = c("CC_2" = "s1q2"))

# Visualize
ggplot(data = map_data_annual) +
  geom_sf(aes(fill = cip_intensity), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "mako", name = "Annual % Land in LUC") +
  labs(
    title = "Annual CIP Intensity: Land Use Consolidation (2025)",
    subtitle = "Combined Data from Seasons A, B, and C",
    caption = "Source: SAS 2025 Plot-Level Microdata"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_blank())



# ====================== IMPROVED =========================

# Improved Function to Clean and Weight Season Data
process_sas_season <- function(df, season_label) {
  df %>%
    # Filter for only Agricultural Land (Code 96 in s2q7)
    filter(as.numeric(s2q7) == 96) %>%
    group_by(s1q2) %>%
    summarise(
      # Weighted Estimated Total Agricultural Hectares in District
      total_ha_est = sum(Plot_size_ha * plot_weight, na.rm = TRUE),
      # Weighted Estimated LUC Hectares in District (s2q14 == 1 is 'Yes')
      luc_ha_est = sum((Plot_size_ha * plot_weight)[as.numeric(s2q14) == 1], na.rm = TRUE)
    ) %>%
    mutate(
      season = season_label,
      seasonal_intensity = (luc_ha_est / total_ha_est) * 100
    )
}

# Apply to all seasons
summary_a <- process_sas_season(sas_a, "A")
summary_b <- process_sas_season(sas_b, "B")
summary_c <- process_sas_season(sas_c, "C")

# Calculate Final Annual Average Intensity per District
dist_annual_summary <- bind_rows(summary_a, summary_b, summary_c) %>%
  group_by(s1q2) %>%
  summarise(
    # We average the seasonal intensities to get a balanced annual footprint
    cip_intensity = mean(seasonal_intensity, na.rm = TRUE),
    # Keep the district code as a character for mapping
    s1q2 = as.character(first(s1q2))
  )

# Visualization (Updated with scaled colors)
ggplot(data = map_data_annual) +
  geom_sf(aes(fill = cip_intensity), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma", 
    name = "Avg Annual % LUC",
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "Rwanda: Annual Land Use Consolidation Intensity (2025)",
    subtitle = "Population-Weighted Estimates from SAS Seasons A, B, & C",
    caption = "Source: SAS 2025 Microdata | Filtered for Agricultural Land"
  ) +
  theme_minimal()


colnames(sas_a)[49] # Should be 'plot_weight'
colnames(sas_b)[52] # Check if this is the weight for Season B
colnames(sas_c)[41] # Check if this is the weight for Season C