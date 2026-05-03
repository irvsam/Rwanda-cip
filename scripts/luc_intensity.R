# Load libraries
library(haven)
library(tidyverse)
library(sf) 
library(labelled)
library(geodata)

# Load and Collapse SAS data to District Level
sas_data <- read_dta("data/SAS 2025/Season A/Rwa_raw_SeasonA2025_Screening.dta")

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

# Turn the whole first column of dist cip summary all into characters instead of integers
dist_cip_summary <- dist_cip_summary %>%
  mutate(s1q2 = as.character(s1q2))

# Merge the data with the map
map_data <- rwa_map %>%
  left_join(dist_cip_summary, by = c("CC_2" = "s1q2"))


# Create the Visualization
ggplot(data = map_data) +
  geom_sf(aes(fill = cip_intensity), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "mako", 
    name = "% Land in LUC",
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "CIP Intensity: Land Use Consolidation by District",
    subtitle = "Source: SAS 2025 Season A Plot-Level Data",
    caption = "Intensity calculated as (Total Area in LUC / Total Agricultural Area)"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), grid = element_blank())
