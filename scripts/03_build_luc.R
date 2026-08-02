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


# Look at all the labels to see which would be useful
colnames(sas_a)
colnames(sas_b)
colnames(sas_c)
View(labelled::look_for(sas_a))
View(labelled::look_for(sas_b))
View(labelled::look_for(sas_c))

# clean up each one and just keep the following variables - segment id, s1q13, s2q1, s2q13, s2q7, s2q6, s2q12, s1q2, plotsize, plot weight
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

# District map for visual representation later on
rwa_map <- gadm(country = "RWA", level = 2, path = tempdir()) %>% st_as_sf()
saveRDS(rwa_map, file.path(processed_path, "rwa_map.rds"))

# -------------------------------------------------------------------


# Now for 2019

# Read in the SAS 2019 Screening datasets for Seasons A, B, and C
sas_a_2019 <- read_dta(file.path(data_path, "older SAS/SAS 2019/rwa-sas-seasonA_Screening_Antierosion_land consolidation.dta")) %>% mutate(season = "A")
sas_b_2019 <- read_dta(file.path(data_path, "older SAS/SAS 2019/rwa-sas-seasonB_Screening_Antierosion_land consolidation.dta")) %>% mutate(season = "B")
sas_c_2019 <- read_dta(file.path(data_path, "older SAS/SAS 2019/rwa-sas-seasonC_Screening_Antierosion_land consolidation.dta")) %>% mutate(season = "C")


# Look at the columns to see which would be useful
colnames(sas_a_2019)
colnames(sas_b_2019)
colnames(sas_c_2019)
View(labelled::look_for(sas_a_2019))
View(labelled::look_for(sas_b_2019))
View(labelled::look_for(sas_c_2019))

sas_a_crops_2019 <- read_dta(file.path(data_path, "older SAS/SAS 2019/rwa-sas-seasonA-Screening_crops.dta")) %>% mutate(season = "A")
sas_b_crops_2019 <- read_dta(file.path(data_path, "older SAS/SAS 2019/rwa-sas-seasonB-Screening_crops.dta")) %>% mutate(season = "B")
sas_c_crops_2019 <- read_dta(file.path(data_path, "older SAS/SAS 2019/rwa-sas-seasonC-Screening_crops.dta")) %>% mutate(season = "C")

View(labelled::look_for(sas_a_crops_2019))
View(labelled::look_for(sas_b_crops_2019))
View(labelled::look_for(sas_c_crops_2019))

# rename sas C plot_weight to Plot_weight to match the other seasons
sas_c_2019 <- sas_c_2019 %>% rename(Plot_weight = plot_weight)

join_luc_crops <- function(luc_df, crops_df) {
  luc_agri  <- luc_df   %>% filter(as.numeric(s2q6) == 96)  # Agricultural
  crops_agri <- crops_df %>% filter(as.numeric(s2q6) == 96)
  
  joined <- luc_agri %>%
    # it is spelled plot_weight in season C so need to account for both
    # make it all lower case
    select(Segment_ID, s1q2, s2q1, s2q12, Plot_weight) %>%
    inner_join(
      crops_agri %>% select(Segment_ID, s2q1, s2q4),
      by = c("Segment_ID", "s2q1")
    )
  
  # Sanity check: how much data did the join keep/drop?
  cat(sprintf(
    "LUC agri plots: %d | Crops agri plots: %d | Joined: %d\n",
    nrow(luc_agri), nrow(crops_agri), nrow(joined)
  ))
  
  joined
}

sas_a_joined <- join_luc_crops(sas_a_2019, sas_a_crops_2019)
sas_b_joined <- join_luc_crops(sas_b_2019, sas_b_crops_2019)
sas_c_joined <- join_luc_crops(sas_c_2019, sas_c_crops_2019)






# 2019 equivalent of process_sas_season() from 03_build_luc.R —
# uses Crop_Area instead of Plot_size_ha, Plot_weight (capital P)
# instead of plot_weight, and s2q12 straight (no is.na(s2q7) proxy
# needed since the agricultural filter already happened above).
process_sas_season_2019 <- function(df, season_label) {
  df %>%
    filter(!is.na(s2q12)) %>%
    group_by(s1q2) %>%
    summarise(
      total_ha_est = sum(s2q4 * Plot_weight, na.rm = TRUE),
      luc_ha_est   = sum((s2q4 * Plot_weight)[as.numeric(s2q12) == 1], na.rm = TRUE)
    ) %>%
    mutate(
      season = season_label,
      seasonal_intensity = (luc_ha_est / total_ha_est) * 100
    )
}

dist_luc_2019 <- bind_rows(
  process_sas_season_2019(sas_a_joined, "A"),
  process_sas_season_2019(sas_b_joined, "B"),
  process_sas_season_2019(sas_c_joined, "C")
) %>%
  group_by(s1q2) %>%
  summarise(
    luc_intensity = mean(seasonal_intensity, na.rm = TRUE),
    district_code = as.numeric(first(s1q2))
  )

summary(dist_luc_2019$luc_intensity)
nrow(dist_luc_2019)  # should be 30

saveRDS(dist_luc_2019, file.path(processed_path, "dist_luc_2019.rds"))







