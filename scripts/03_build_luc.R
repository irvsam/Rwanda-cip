
# 03_build_luc.R ------------------------------------------------------
# Construct the district-level LUC intensity variable from
# SAS 2024: area-weighted share of agricultural land under
# consolidation (plot area x SAS plot weight, NISR SAS metadata
# handbook), in percentage points.
#   luc_intensity   : mean of Seasons A, B and C (primary moderator)
#   luc_intensity_A : Season A only, matching the AHS crop season
# Also caches the district boundary shapefile for the map in 07.


if (!exists(".setup_done")) source("scripts/00_setup.R")

# load screening files -----------------------------------
# Only the Screening files are used (they carry the LUC response,
# s2q12, and plot size/weight needed to build the intensity measure).

sas_a <- read_dta(file.path(data_path, "SAS 2024/Season A/Rwa_raw_SeasonA2024_Screening.dta")) %>% mutate(season = "A")
sas_b <- read_dta(file.path(data_path, "SAS 2024/Season B/Rwa_raw_SeasonB2024_Screening.dta")) %>% mutate(season = "B")
sas_c <- read_dta(file.path(data_path, "SAS 2024/Season C/Rwa_raw_SeasonC2024_Screening.dta")) %>% mutate(season = "C")


# select only what is needed -------------------------------
# Columns needed: segment id, district (s1q2), plot type (s2q6), LUC response (s2q12), plot size, and plot weight.
clean_sas <- function(df, rule = c("any", "all")) {
  rule <- match.arg(rule)
  agg  <- if (rule == "any") any else all
  df %>%
    group_by(Segment_ID, s2q1) %>%              # one row per plot
    summarise(
      s1q2         = first(s1q2),
      agri         = agg(as.numeric(s2q6) == 96, na.rm = TRUE),
      luc_answered = any(!is.na(s2q12)),
      luc          = agg(as.numeric(s2q12) == 1, na.rm = TRUE),
      Plot_size_ha = first(Plot_size_ha),
      plot_weight  = first(plot_weight),
      .groups = "drop"
    )
}

process_sas_season <- function(plots, season_label) {
  plots %>%
    filter(agri, luc_answered) %>%
    group_by(s1q2) %>%
    summarise(
      total_ha_est = sum(Plot_size_ha * plot_weight, na.rm = TRUE),
      luc_ha_est   = sum((Plot_size_ha * plot_weight)[luc], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(season = season_label,
           seasonal_intensity = 100 * luc_ha_est / total_ha_est)
}

# One row per plot, then district shares per season
luc_seasons <- bind_rows(
  process_sas_season(clean_sas(sas_a), "A"),
  process_sas_season(clean_sas(sas_b), "B"),
  process_sas_season(clean_sas(sas_c), "C")
)

dist_luc <- luc_seasons %>%
  group_by(s1q2) %>%
  summarise(
    luc_intensity   = mean(seasonal_intensity, na.rm = TRUE),
    luc_intensity_A = first(seasonal_intensity[season == "A"]),
    luc_intensity_B = first(seasonal_intensity[season == "B"]),
    luc_intensity_C = first(seasonal_intensity[season == "C"]),
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


