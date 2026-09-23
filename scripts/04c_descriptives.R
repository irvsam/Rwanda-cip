# ============================================================
# 04c_descriptives.R
# Descriptive statistics for the primary (AHS) estimation sample,
# plus the checks reported in the research design section.
# ============================================================

if (!exists(".setup_done")) source("scripts/00_setup.R")
master  <- readRDS(file.path(processed_path, "master.rds"))
primary <- filter(master, in_ahs)

# ---- Table 1: descriptives ----------------------------------
desc_vars <- primary %>%
  transmute(
    # Outcomes, ordered along the mechanism
    `Own-produced food groups`                        = hdds_own,
    `Purchased share of consumed items`               = purch_share,
    `Purchased food groups`                           = hdds_purch,
    `Dietary diversity (HDDS, 0-12)`                  = hdds,
    `Non-staple food groups (0-6)`                    = hdds_nonstaple,
    `Food consumption per AE, Jan 2024 prices (RWF)`  = food_ae_real,
    # Explanatory variables
    `Crop concentration (HHI, Season A)`              = crop_hhi,
    `Number of crops, Season A`                       = n_crops,
    `Share of crop area in CIP priority crops`        = prio_share,
    `District LUC intensity (pp)`                     = luc_intensity,
    # Controls
    `Agricultural land held (ha)`                     = land_ha,
    `Household size`                                  = hh_size,
    `Dependency ratio`                                = dep_ratio,
    `Head age`                                        = head_age,
    `Female-headed household`                         = head_female,
    `Head education`                                  = head_educ,
    `Rural`                                           = ur_f,
    `Province`                                        = province_f
  )

print(summary(desc_vars))

datasummary_skim(desc_vars, type = "numeric",
                 title = "Descriptive statistics, AHS sample",
                 output = file.path(output_tables_path, "table1_descriptives.tex"))
datasummary_skim(desc_vars, type = "categorical",
                 title = "Categorical variables, AHS sample",
                 output = file.path(output_tables_path, "table1_descriptives_categorical.tex"))

# ---- Checks cited in the design section ---------------------

cat("\nHHI distribution (spike at 1?):\n")
primary %>%
  count(hhi_band = cut(crop_hhi, breaks = c(0, 0.25, 0.5, 0.75, 0.999, 1),
                       include.lowest = TRUE)) %>%
  print()

cat("\nLUC intensity across the 30 districts (pp):\n")
master %>%
  distinct(district_code, luc_intensity) %>%
  summarise(districts = n(),
            min = min(luc_intensity), p10 = quantile(luc_intensity, 0.1),
            median = median(luc_intensity), p90 = quantile(luc_intensity, 0.9),
            max = max(luc_intensity)) %>%
  print()

cat("\nLand confounding: HHI and outcomes by land tercile:\n")
primary %>%
  mutate(land_tercile = ntile(land_ha, 3)) %>%
  group_by(land_tercile) %>%
  summarise(n = n(),
            mean_land_ha  = mean(land_ha),
            mean_hhi      = mean(crop_hhi),
            mean_hdds     = mean(hdds, na.rm = TRUE),
            mean_log_food = mean(log_food_ae_real, na.rm = TRUE)) %>%
  print()

cat("\nIs concentration concentration in CIP priority crops?\n")
primary %>%
  mutate(hhi_tercile = ntile(crop_hhi, 3)) %>%
  group_by(hhi_tercile) %>%
  summarise(n = n(),
            mean_hhi          = mean(crop_hhi),
            mean_prio_share   = mean(prio_share),
            share_top_is_prio = mean(top_crop_prio)) %>%
  print()

cat("Correlation of HHI with priority-crop share:",
    round(cor(primary$crop_hhi, primary$prio_share), 3), "\n")
cat("Correlation of HDDS with log food per AE:",
    round(cor(primary$hdds, primary$log_food_ae_real, use = "complete.obs"), 3), "\n")
cat("AHS households with HDDS = 0:", sum(primary$hdds == 0, na.rm = TRUE), "\n")