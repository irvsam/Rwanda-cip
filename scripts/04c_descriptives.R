# ============================================================
# 04c_descriptives.R
# Descriptive statistics for the variables used in the models,
# for the two estimation samples:
#   - Primary: AHS sub-panel (household crop concentration x
#     district LUC intensity)
#   - Extension: full EICV7 sample (district LUC intensity only)
# Also catches coding errors before modelling.
# ============================================================

source("scripts/00_setup.R")
master <- readRDS(file.path(processed_path, "master.rds"))

# ---- Primary sample (AHS) -----------------------------------
primary_vars <- master %>%
  filter(in_ahs) %>%
  transmute(
    `Food consumption per AE, Jan 2024 prices (RWF)` = food_ae_real,
    `Food consumption per AE (log)`                  = log_food_ae_real,
    `Crop concentration (HHI, Season A)`             = crop_hhi,
    `Number of crops, Season A`                      = n_crops,
    `Share of crop area in CIP priority crops`       = prio_share,
    `District LUC intensity (%)`                     = luc_intensity,
    `Agricultural land held (ha)`                    = land_ha,
    `Household size`                                 = hh_size,
    `Dependency ratio`                               = dep_ratio,
    `Head age`                                       = head_age,
    `Female-headed household`                        = head_female,
    `Head education`                                 = head_educ,
    `Urban/rural`                                    = ur_f,
    `Province`                                       = province_f
  )

summary(primary_vars)

datasummary_skim(
  primary_vars,
  output = file.path(output_tables_path, "table1_descriptives_primary.docx")
)
datasummary_skim(
  primary_vars, type = "categorical",
  output = file.path(output_tables_path, "table1_descriptives_primary_categorical.docx")
)

# ---- Extension sample (full EICV7) --------------------------
extension_vars <- master %>%
  transmute(
    `Food consumption per AE, Jan 2024 prices (RWF)` = food_ae_real,
    `Food consumption per AE (log)`                  = log_food_ae_real,
    `District LUC intensity (%)`                     = luc_intensity,
    `Household size`                                 = hh_size,
    `Urban/rural`                                    = ur_f,
    `Province`                                       = province_f
  )

datasummary_skim(
  extension_vars,
  output = file.path(output_tables_path, "table1_descriptives_extension.docx")
)

# ---- Checks -------------------------------------------------

# HHI distribution: is there a spike at 1 (single-crop households)?
master %>%
  filter(in_ahs) %>%
  count(hhi_band = cut(crop_hhi, breaks = c(0, 0.25, 0.5, 0.75, 0.999, 1),
                       include.lowest = TRUE)) %>%
  print()

# LUC range across districts (and its units)
master %>%
  distinct(district_code, luc_intensity) %>%
  summarise(districts = n(),
            min = min(luc_intensity), p10 = quantile(luc_intensity, 0.1),
            median = median(luc_intensity), p90 = quantile(luc_intensity, 0.9),
            max = max(luc_intensity)) %>%
  print()

# Does crop concentration differ by land size? (confounding check)
master %>%
  filter(in_ahs) %>%
  mutate(land_tercile = ntile(land_ha, 3)) %>%
  group_by(land_tercile) %>%
  summarise(n = n(),
            mean_land_ha = mean(land_ha, na.rm = TRUE),
            mean_hhi     = mean(crop_hhi),
            mean_log_food = mean(log_food_ae_real, na.rm = TRUE)) %>%
  print()

# ---- Is concentration concentration in CIP priority crops? ---
# Descriptive support for the H1 framing. If concentrated households
# are concentrated in priority crops, one sentence in 4.3 can say so.
master %>%
  filter(in_ahs) %>%
  mutate(hhi_tercile = ntile(crop_hhi, 3)) %>%
  group_by(hhi_tercile) %>%
  summarise(n = n(),
            mean_hhi          = mean(crop_hhi),
            mean_prio_share   = mean(prio_share),
            share_top_is_prio = mean(top_crop_prio)) %>%
  print()

cat("Correlation of HHI with priority-crop share:",
    round(cor(master$crop_hhi, master$prio_share, use = "complete.obs"), 3), "\n")

# ---- Are households more concentrated in high-LUC districts? --
district_hhi <- master %>%
  filter(in_ahs) %>%
  group_by(district_code) %>%
  summarise(mean_hhi        = mean(crop_hhi),
            mean_prio_share = mean(prio_share),
            luc_intensity   = first(luc_intensity),
            n = n(), .groups = "drop")

cat("District level (n = 30), correlation of mean HHI with LUC intensity:",
    round(cor(district_hhi$mean_hhi, district_hhi$luc_intensity), 3), "\n")
cat("District level, correlation of mean priority-crop share with LUC intensity:",
    round(cor(district_hhi$mean_prio_share, district_hhi$luc_intensity), 3), "\n")