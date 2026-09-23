# ============================================================
# 03_build_luc.R
# Construct the district-level LUC intensity variable from
# SAS 2024: area-weighted share of agricultural land under
# consolidation (plot area x SAS plot weight, NISR SAS metadata
# handbook), in percentage points.
#   luc_intensity   : mean of Seasons A, B and C (primary moderator)
#   luc_intensity_A : Season A only, matching the AHS crop season
# Also caches the district boundary shapefile for the map in 07.
# ============================================================

if (!exists(".setup_done")) source("scripts/00_setup.R")

# Only the Screening files are used (they carry the LUC response,
# s2q12, and plot size/weight needed to build the intensity measure).

sas_a <- read_dta(file.path(data_path, "SAS 2024/Season A/Rwa_raw_SeasonA2024_Screening.dta")) %>% mutate(season = "A")
sas_b <- read_dta(file.path(data_path, "SAS 2024/Season B/Rwa_raw_SeasonB2024_Screening.dta")) %>% mutate(season = "B")
sas_c <- read_dta(file.path(data_path, "SAS 2024/Season C/Rwa_raw_SeasonC2024_Screening.dta")) %>% mutate(season = "C")

# Columns needed: segment id, district (s1q2), plot type (s2q6), LUC response (s2q12), plot size, and plot weight.
clean_sas <- function(df) {
  df %>%
    select(Segment_ID, s1q1, s1q2, s1q13, s2q1, s2q6, s2q7, s2q12,
           Plot_size_ha, plot_weight)
}

sas_a <- clean_sas(sas_a)
sas_b <- clean_sas(sas_b)
sas_c <- clean_sas(sas_c)

process_sas_season <- function(df, season_label) {
  # Filter to agricultural plots (s2q6 == 96) with a valid LUC
  # response (s2q12), group by district (s1q2), and compute the
  # area-weighted share of agricultural land under LUC.
  df %>%
    filter(as.numeric(s2q6) == 96) %>%
    filter(!is.na(s2q12)) %>%
    group_by(s1q2) %>%
    summarise(
      .groups = "drop",
      total_ha_est = sum(Plot_size_ha * plot_weight, na.rm = TRUE),
      luc_ha_est   = sum((Plot_size_ha * plot_weight)[as.numeric(s2q12) == 1], na.rm = TRUE)
    ) %>%
    mutate(
      season = season_label,
      seasonal_intensity = (luc_ha_est / total_ha_est) * 100
    )
}

# Average the three seasonal intensities to get one district-level
# LUC intensity value per district.
dist_luc <- bind_rows(
  process_sas_season(sas_a, "A"),
  process_sas_season(sas_b, "B"),
  process_sas_season(sas_c, "C")
) %>%
  group_by(s1q2) %>%
  summarise(
    luc_intensity   = mean(seasonal_intensity, na.rm = TRUE),
    luc_intensity_A = first(seasonal_intensity[season == "A"]),
    district_code   = as.numeric(first(s1q2)),
    .groups = "drop"
  )

stopifnot(!any(is.na(dist_luc$luc_intensity_A)))  # every district has Season A

stopifnot(nrow(dist_luc) == 30)  # all 30 districts present
print(summary(dist_luc$luc_intensity))

saveRDS(dist_luc, file.path(processed_path, "dist_luc.rds"))

# ---- District boundary shapefile (for mapping) ----
# Only download if not already cached locally -- gadm() otherwise
# re-fetches the shapefile from the GADM server on every run.
rwa_map_path <- file.path(processed_path, "rwa_map.rds")
if (!file.exists(rwa_map_path)) {
  tryCatch({
    rwa_map <- gadm(country = "RWA", level = 2, path = processed_path) %>% st_as_sf()
    saveRDS(rwa_map, rwa_map_path)
  }, error = function(e) {
    message("Could not download Rwanda boundary shapefile: ", e$message)
  })
} else {
  message("rwa_map.rds already exists, skipping download.")
}