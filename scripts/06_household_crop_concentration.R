# ============================================================
# 06_household_crop_concentration.R
# PRIMARY ANALYSIS.
#
# Builds a household-level "consolidation-like" proxy from AHS2024
# plot/crop data: a Herfindahl-Hirschman concentration index over
# crop area shares. High value = household's land is dominated by
# one or few crops (behaviorally consolidated); low value = diverse
# cropping (traditional intercropping pattern).
#
# AHS2024 hhids fall within the EICV7 numbering scheme and share
# EICV7's quintile/poverty variables (confirmed via Section1's
# pid = "Person ID Number (EICV7)" and Section2's quintile/epov_jan/
# pov_jan fields) -- this is a ~3,700-household sub-panel of the
# EICV7 sample, not a separate survey population.
#
# Household-level crop_hhi is the primary independent variable;
# district-level luc_intensity (built in 03_build_luc.R) is the
# moderator, echoing Del Prete et al. (2019)'s household-level
# design while adding the structural spillover context their
# design could not capture.
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

# ---- Load raw AHS2024 files ----
crop_data <- read_dta("data/raw/AHS 2024/AHS2024_Section3_4_CROP_GROWN__SEEDS_AND_PRODUCTION___AGRICULTURAL_INPUTS_AND_PRACTICES.dta")
land_data <- read_dta("data/raw/AHS 2024/AHS2024_Section2_LAND_TENURE.dta")

# Filter to Season A only. AHS records crop data across multiple
# seasons; without this filter, the same physical plot's crops get
# summed across seasons, double-counting land and distorting the
# concentration index (confirmed by comparing pre/post-filter HHI
# distributions during development).
crop_data <- crop_data %>% filter(Season == 1)

# ---- Reshape crop slots (1-7) from wide to long ----
# Each plot-row can list up to 7 crops with a name + % of plot area.
crop_slots <- map_dfr(1:7, function(i) {
  name_col <- paste0("s3_q4_", i)
  prop_col <- paste0("s3_q4_", i, "a")
  
  crop_data %>%
    select(hhid, plot_id = s2q1, plot_area = s2q2,
           crop_name = all_of(name_col),
           crop_pct  = all_of(prop_col)) %>%
    mutate(slot = i)
}) %>%
  filter(!is.na(crop_name), !is.na(crop_pct), !is.na(plot_area)) %>%
  mutate(crop_area = plot_area * (as.numeric(crop_pct) / 100))

# ---- Aggregate to household-crop level, then compute HHI ----
hh_concentration <- crop_slots %>%
  group_by(hhid, crop_name) %>%
  summarise(total_crop_area = sum(crop_area, na.rm = TRUE), .groups = "drop") %>%
  group_by(hhid) %>%
  mutate(hh_total_area = sum(total_crop_area, na.rm = TRUE),
         crop_share    = total_crop_area / hh_total_area) %>%
  summarise(
    crop_hhi      = sum(crop_share^2, na.rm = TRUE),  # 1/n (diverse) to 1 (monocrop)
    n_crops       = n_distinct(crop_name),
    hh_total_area = first(hh_total_area),
    .groups = "drop"
  ) %>%
  filter(is.finite(crop_hhi))

summary(hh_concentration$crop_hhi)
summary(hh_concentration$n_crops)
nrow(hh_concentration)

# ---- Household-level land size (one row per household) ----
# total_agr_land is repeated identically across a household's plot
# rows in Section 2; distinct() collapses to one row per household.
hh_land <- land_data %>%
  select(hhid, total_agr_land) %>%
  distinct(hhid, .keep_all = TRUE)

# ---- Merge onto the EICV7 analysis data ----
# inner_join deliberately restricts to the ~3,700-household AHS2024
# sub-panel; left_join for land size doesn't need to gate membership.
extension_data <- analysis_data %>%
  inner_join(hh_concentration, by = "hhid") %>%
  left_join(hh_land, by = "hhid") %>%
  mutate(
    crop_hhi_c      = crop_hhi - mean(crop_hhi, na.rm = TRUE),
    luc_intensity_c = luc_intensity - mean(luc_intensity, na.rm = TRUE),
    wealth_group    = if_else(as.numeric(quintile_f) <= 2,
                              "Poorer (Q1-Q2)", "Wealthier (Q3-Q5)")
  )

nrow(extension_data)
n_distinct(extension_data$district_code)  # confirm coverage across all 30 districts

saveRDS(extension_data, file.path(processed_path, "extension_data.rds"))

# ============================================================
# H1: household-level crop concentration -> food consumption
# H2: amplified by district-level LUC intensity
# ============================================================

# Primary model: centered so crop_hhi's main effect is interpretable
# at an average district's LUC intensity, rather than a hypothetical
# LUC = 0. Centering does not change the interaction coefficient.
model_ext_main <- lm_robust(
  log_food_ae ~ crop_hhi_c * luc_intensity_c + quintile_f + ur_f + province_f,
  data = extension_data,
  clusters = district_code
)
summary(model_ext_main)

# ---- Supplementary: is the crop_hhi effect concentrated among
# poorer households? Tested two ways. ----

# (a) Full quintile interaction
model_ext_hhi_quintile <- lm_robust(
  log_food_ae ~ crop_hhi * quintile_f + ur_f + province_f,
  data = extension_data,
  clusters = district_code
)
summary(model_ext_hhi_quintile)

# (b) Binary wealth-group interaction (direct, better-powered test)
model_wealth_interaction <- lm_robust(
  log_food_ae ~ crop_hhi_c * wealth_group + luc_intensity_c + ur_f + province_f,
  data = extension_data,
  clusters = district_code
)
summary(model_wealth_interaction)
# Result: no significant wealth-group interaction (see write-up) --
# the crop_hhi main effect holds across both wealth groups, but is
# not more amplified for poorer households than wealthier ones.

# ---- Cross-check: multilevel model as an alternative to clustered
# OLS, modeling the household-in-district structure directly rather
# than correcting for it post hoc. Confirms the clustered OLS results. ----
model_hlm <- lmer(
  log_food_ae ~ crop_hhi_c * luc_intensity_c + quintile_f + ur_f +
    (1 | district_code),
  data = extension_data
)
summary(model_hlm)

# ---- Export for the paper (LaTeX/booktabs, for \input{} in Overleaf) ----
modelsummary(
  list(
    "Main + interaction (LUC)"   = model_ext_main,
    "Quintile interaction"       = model_ext_hhi_quintile,
    "Wealth-group interaction"   = model_wealth_interaction
  ),
  output = file.path(output_tables_path, "extension_results.tex"),
  stars  = TRUE,
  title  = "Household-Level Crop Concentration and Food Consumption",
  notes  = "Standard errors clustered by district in parentheses. Columns 1 and 3 use crop\\_hhi centered on its sample mean; Column 2 uses the uncentered variable, so its crop\\_hhi coefficient reflects the effect within Q1 (the reference quintile) only."
)




cor(extension_data$crop_hhi, as.numeric(as.character(extension_data$quintile_f)))
summary(lm(crop_hhi ~ quintile_f, data = extension_data))$r.squared



crop_data_raw <- read_dta("data/raw/AHS 2024/AHS2024_Section3_4_CROP_GROWN__SEEDS_AND_PRODUCTION___AGRICULTURAL_INPUTS_AND_PRACTICES.dta")

n_distinct(crop_data_raw$hhid)                               # households with ANY crop record, any season
n_distinct(crop_data_raw$hhid[crop_data_raw$Season == 1])     # households with a Season A record



model_ext_wealth_luc <- lm_robust(
  log_food_ae ~ crop_hhi_c * luc_intensity_c * wealth_group + ur_f + province_f,
  data = extension_data,
  clusters = district_code
)