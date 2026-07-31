# ============================================================
# 03_build_luc.R
# Construct the district-level LUC intensity moderator from
# SAS 2024 : population-weighted
# share of agricultural land under consolidation, averaged across
# Seasons A, B, C.
# ============================================================

source("scripts/00_setup.R")


# Read in the SAS 2024 Screening datasets for Seasons A, B, and C
sas_a <- read_dta(file.path(data_path, "SAS 2024/Season A/Rwa_raw_SeasonA2024_Screening.dta")) %>% mutate(season = "A")
sas_b <- read_dta(file.path(data_path, "SAS 2024/Season B/Rwa_raw_SeasonB2024_Screening.dta")) %>% mutate(season = "B")
sas_c <- read_dta(file.path(data_path, "SAS 2024/Season C/Rwa_raw_SeasonC2024_Screening.dta")) %>% mutate(season = "C")



process_sas_season <- function(df, season_label) {
  # Filter for agricultural plots with a valid LUC response, group by district, and calculate total and LUC area estimates
  df %>%
    filter(is.na(s2q7), !is.na(s2q12)) %>%   # agricultural plots with a valid LUC response
    group_by(s1q2) %>%                        # district
    summarise(
      total_ha_est = sum(Plot_size_ha * plot_weight, na.rm = TRUE),
      luc_ha_est   = sum((Plot_size_ha * plot_weight)[as.numeric(s2q12) == 1], na.rm = TRUE)
    ) %>%
    mutate(
      season = season_label,
      seasonal_intensity = (luc_ha_est / total_ha_est) * 100
    )
}
# Combine the processed data from all three seasons and calculate the average LUC intensity per district
dist_luc <- bind_rows(
  process_sas_season(sas_a, "A"),
  process_sas_season(sas_b, "B"),
  process_sas_season(sas_c, "C")
) %>%
  group_by(s1q2) %>%
  summarise(
    luc_intensity = mean(seasonal_intensity, na.rm = TRUE),
    district_code = as.numeric(first(s1q2))
  )

summary(dist_luc$luc_intensity)
nrow(dist_luc)  # check for 30 districts

saveRDS(dist_luc, file.path("data/preprocessed", "dist_luc.rds"))

# District map for visual representation later on
rwa_map <- gadm(country = "RWA", level = 2, path = tempdir()) %>% st_as_sf()
saveRDS(rwa_map, file.path("data/preprocessed", "rwa_map.rds"))

