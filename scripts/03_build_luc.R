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

sas_a_production <- read_dta(file.path(data_path, "SAS 2024/Season A/Rwa_raw_SeasonA2024_Production.dta")) %>% mutate(season = "A")
sas_b_production <- read_dta(file.path(data_path, "SAS 2024/Season B/Rwa_raw_SeasonB2024_Production.dta")) %>% mutate(season = "B")
sas_c_production <- read_dta(file.path(data_path, "SAS 2024/Season C/Rwa_raw_SeasonC2024_Production.dta")) %>% mutate(season = "C")

# View(labelled::look_for(sas_a_production))
# View(labelled::look_for(sas_b_production))
# View(labelled::look_for(sas_c_production))

sas_a_practice <- read_dta(file.path(data_path, "SAS 2024/Season A/Rwa_raw_SeasonA2024_Agricultural_practice.dta")) %>% mutate(season = "A")
sas_b_practice <- read_dta(file.path(data_path, "SAS 2024/Season B/Rwa_raw_SeasonB2024_Agricultural_practice.dta")) %>% mutate(season = "B")
sas_c_practice <- read_dta(file.path(data_path, "SAS 2024/Season C/Rwa_raw_SeasonC2024_Agricultural_practice.dta")) %>% mutate(season = "C")
# View(labelled::look_for(sas_a_practice))
# View(labelled::look_for(sas_b_practice))
# View(labelled::look_for(sas_c_practice))


# Look at all the labels to see which would be useful
# View(labelled::look_for(sas_a))
# View(labelled::look_for(sas_b))
# View(labelled::look_for(sas_c))

# Clean up each one and just keep the following variables - segment id, s1q13, s2q1, s2q13, s2q7, s2q6, s2q12, s1q2, plotsize, plot weight
clean_sas <- function(df) {
  df %>%
    select(Segment_ID,s1q1, s1q2, s1q13, s2q1, s2q6, s2q7, s2q12, Plot_size_ha, plot_weight)
}

clean_sas(sas_a) -> sas_a
clean_sas(sas_b) -> sas_b
clean_sas(sas_c) -> sas_c



process_sas_season <- function(df, season_label) {
  # Filter for agricultural plots with a valid LUC response, group by district, and calculate total and LUC area estimates
  
  df %>%
    # first filter for agricultural plots which would be s2q7 = 96

    filter(as.numeric(s2q6) == 96) %>%  # agricultural plots
    filter(!is.na(s2q12)) %>%   #  valid LUC response
    group_by(s1q2) %>%          # group by district
    
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

saveRDS(dist_luc, file.path(processed_path, "dist_luc.rds"))


dir.create(processed_path, showWarnings = FALSE)

# 3. Try downloading with error handling
tryCatch({
  rwa_map <- gadm(country = "RWA", level = 2, path = processed_path) %>% st_as_sf()
  saveRDS(rwa_map, file.path(processed_path, "rwa_map.rds"))
}, error = function(e) {
  print(paste("Error:", e$message))
})

