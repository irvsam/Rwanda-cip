# ============================================================
# 04a_build_analysis_data.R
# Builds the master file: one row per EICV7 household.
#
#   - Base, outcome and household size: EICV7 poverty file
#   - Head characteristics, composition: AHS Section 1
#   - Land held and AHS weight: AHS Section 2
#   - Crop concentration (Season A): AHS Section 3/4
#   - Programme proxies (validation only, NOT controls): AHS Section 6
#   - District LUC intensity: dist_luc.rds (from the SAS script)
#
# Deliberately excluded: quintile, cons1ae, sol_jan and the poverty
# variables (all built from consumption, so not valid controls).
# ============================================================

source("scripts/00_setup.R")
if (!exists("poverty_data")) source("scripts/01_load_eicv7.R")
if (!exists("ahs_s1"))       source("scripts/01b_load_ahs.R")
dist_luc <- readRDS(file.path(processed_path, "dist_luc.rds"))

# ---- Codes (confirmed against the value labels printed by 01b) ----
HEAD_CODE       <- 1  # s1q2: Household head (HH)
FEMALE_CODE     <- 2  # s1q1: Female
MEMBER_CODES    <- 1  # s1q14: household member = Yes
EVER_SCHOOL_YES <- 1  # s4aq1: ever attended school = Yes
SEASON_A_CODE   <- 1  # Season: Season A
YES_CODE        <- 1  # Section 6 yes/no questions (2 = No)

# ---- Helpers -----------------------------------------------
id_num <- function(x) as.numeric(zap_labels(x))

check_unique <- function(df, name) {
  n_dup <- sum(duplicated(df$hhid))
  if (n_dup > 0) stop(name, ": ", n_dup, " duplicated hhid values")
  message(name, ": ", nrow(df), " households, hhid unique")
}

# 1 if any row for the household answered yes, 0 if all answered,
# NA if the household never answered
any_yes <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) NA_integer_ else as.integer(any(x == YES_CODE, na.rm = TRUE))
}

# Collapses s1 education into five groups:
#   Never attended            s4aq1 = No
#   Attended, no certificate  attended, s4aq3 missing or 16 (None)
#   Primary                   s4aq3 = 1
#   Secondary or TVET         s4aq3 = 2-9 (post-primary, EMA/ENTA, O level,
#                             A3, A2, TVET III-V)
#   Tertiary                  s4aq3 = 10-15 (A1 diplomas up to PhD)
#   s4aq3 = 99 (Do not know) -> NA
# Tertiary is small; merge it into "Secondary or TVET" if it causes
# estimation problems.
collapse_educ <- function(ever_school, diploma_code) {
  case_when(
    ever_school != EVER_SCHOOL_YES             ~ "Never attended",
    diploma_code == 99                         ~ NA_character_,
    is.na(diploma_code) | diploma_code == 16   ~ "Attended, no certificate",
    diploma_code == 1                          ~ "Primary",
    diploma_code %in% 2:9                      ~ "Secondary or TVET",
    diploma_code %in% 10:15                    ~ "Tertiary"
  )
}

EDUC_LEVELS <- c("Never attended", "Attended, no certificate", "Primary",
                 "Secondary or TVET", "Tertiary")

# CIP priority crops (codes as in the SAS crop list; check they match the
# AHS labels printed by 01b): maize, paddy rice, wheat, bush bean,
# climbing bean, Irish potato, soybean, cassava, small red bean
PRIORITY_CROPS <- c(101, 102, 104, 106, 107, 110, 122, 130, 305)

# ============================================================
# Step 1: base (EICV7 poverty file)
# ============================================================

base <- poverty_data %>%
  transmute(
    hhid          = id_num(hhid),
    clust         = id_num(clust),
    province      = id_num(province),
    district_code = id_num(district),
    ur            = id_num(ur),
    ae            = as.numeric(ae),
    hh_size       = as.numeric(member),
    food          = as.numeric(food),
    hh_index      = as.numeric(hh_index),
    wt_eicv       = as.numeric(weight),
    food_ae_nominal     = food / ae,
    food_ae_real        = food / ae / hh_index,   # Jan 2024 prices, as sol_jan
    log_food_ae_nominal = if_else(food_ae_nominal > 0, log(food_ae_nominal), NA_real_),
    log_food_ae_real    = if_else(food_ae_real > 0, log(food_ae_real), NA_real_)
  )

check_unique(base, "Step 1 base")

# Checks: deflator, food aggregate, zeros
pov_checks <- poverty_data %>%
  transmute(
    deflator_diff = as.numeric(cons1ae) / as.numeric(hh_index) - as.numeric(sol_jan),
    food_diff     = as.numeric(food) - (coalesce(as.numeric(exp9), 0) +
                                          coalesce(as.numeric(exp10), 0) +
                                          coalesce(as.numeric(Food_at_school), 0))
  )
cat("Max |cons1ae / hh_index - sol_jan|:", max(abs(pov_checks$deflator_diff), na.rm = TRUE), "\n")
cat("Max |food - (exp9 + exp10 + Food_at_school)|:", max(abs(pov_checks$food_diff), na.rm = TRUE), "\n")
cat("Households with food = 0 or NA:", sum(is.na(base$food) | base$food == 0), "\n")

# ============================================================
# Step 2: head characteristics and composition (AHS Section 1)
# ============================================================

members <- ahs_s1 %>%
  transmute(
    hhid        = id_num(hhid),
    member      = id_num(s1q14),
    rel         = id_num(s1q2),
    sex         = id_num(s1q1),
    age         = as.numeric(s1q3y),
    ever_school = id_num(s4aq1),
    diploma     = id_num(s4aq3),
    diploma_lab = as_factor(s4aq3)
  ) %>%
  filter(member %in% MEMBER_CODES)

hh_comp <- members %>%
  group_by(hhid) %>%
  summarise(
    n_members_ahs = n(),
    n_dep         = sum(age < 15 | age > 64, na.rm = TRUE),
    n_working     = sum(age >= 15 & age <= 64, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # pmax avoids dividing by zero where nobody is of working age
  mutate(dep_ratio = n_dep / pmax(n_working, 1))

heads <- members %>% filter(rel == HEAD_CODE)
cat("Households with more than one head:",
    heads %>% count(hhid) %>% filter(n > 1) %>% nrow(), "\n")

hh_head <- heads %>%
  group_by(hhid) %>%
  slice_max(age, n = 1, with_ties = FALSE) %>%   # oldest if duplicated
  ungroup() %>%
  transmute(
    hhid,
    head_female = as.integer(sex == FEMALE_CODE),
    head_age    = age,
    head_educ   = factor(collapse_educ(ever_school, diploma),
                         levels = EDUC_LEVELS),
    head_diploma_raw = diploma_lab
  )

# Check the education collapse, then drop head_diploma_raw later if happy
print(count(hh_head, head_diploma_raw, head_educ), n = Inf)

check_unique(hh_comp, "Step 2 composition")
check_unique(hh_head, "Step 2 head")
cat("AHS households with no head found:",
    n_distinct(id_num(ahs_s1$hhid)) - nrow(hh_head), "\n")

# ============================================================
# Step 3: land held and AHS weight (AHS Section 2)
# ============================================================

land <- ahs_s2 %>%
  transmute(hhid = id_num(hhid),
            land_ha = as.numeric(total_agr_land),
            wt_ahs  = as.numeric(weight)) %>%
  group_by(hhid) %>%
  summarise(
    n_land_values = n_distinct(na.omit(land_ha)),   # should be 0 or 1
    land_ha       = first(na.omit(land_ha)),
    wt_ahs        = first(na.omit(wt_ahs)),
    .groups = "drop"
  )

cat("Households with conflicting total_agr_land values:",
    sum(land$n_land_values > 1), "\n")
land <- select(land, -n_land_values)
check_unique(land, "Step 3 land")

# ============================================================
# Step 4: crop concentration, Season A (AHS Section 3/4)
# Crop area = plot area x crop proportion (NISR SAS handbook, p. 22).
# Shares use the SUM of crop areas as the denominator, because
# proportions can exceed 100% in total where crops harvested in
# different seasons are double counted.
# Uses proportion (s3_q4_Xa), never density (s3_q4_Xb).
# ============================================================

s34 <- ahs_s34 %>% mutate(row_id = row_number())

plots <- s34 %>%
  transmute(row_id,
            hhid     = id_num(hhid),
            season   = id_num(Season),
            plot     = id_num(s2q1),
            area_sqm = as.numeric(s2q2))

crop_codes <- s34 %>%
  select(row_id, matches("^s3_q4_[1-7]$")) %>%
  mutate(across(-row_id, id_num)) %>%
  pivot_longer(-row_id, names_to = "slot", names_prefix = "s3_q4_",
               values_to = "crop")

crop_props <- s34 %>%
  select(row_id, matches("^s3_q4_[1-7]a$")) %>%
  mutate(across(-row_id, as.numeric)) %>%
  pivot_longer(-row_id, names_to = "slot", names_pattern = "s3_q4_(\\d)a",
               values_to = "prop")

crops_A <- plots %>%
  filter(season == SEASON_A_CODE) %>%
  inner_join(crop_codes, by = "row_id") %>%
  left_join(crop_props, by = c("row_id", "slot")) %>%
  filter(!is.na(crop), !is.na(prop), prop > 0,
         !is.na(area_sqm), area_sqm > 0) %>%
  mutate(crop_area = area_sqm * prop / 100)

# How often do proportions on a plot sum above 100%?
crops_A %>%
  group_by(row_id) %>%
  summarise(total_prop = sum(prop), .groups = "drop") %>%
  summarise(plots = n(), over_100 = sum(total_prop > 100)) %>%
  print()

hh_crops <- crops_A %>%
  group_by(hhid, crop) %>%                 # same crop on several plots is summed
  summarise(crop_area = sum(crop_area), .groups = "drop_last") %>%
  mutate(share = crop_area / sum(crop_area)) %>%
  summarise(crop_hhi   = sum(share^2),
            n_crops    = n(),
            # share of Season A crop area under CIP priority crops
            prio_share = sum(crop_area[crop %in% PRIORITY_CROPS]) / sum(crop_area),
            # is the household's largest crop a priority crop?
            top_crop_prio = as.integer(crop[which.max(crop_area)] %in% PRIORITY_CROPS),
            .groups    = "drop")

hh_area_A <- crops_A %>%
  distinct(hhid, row_id, area_sqm) %>%     # each plot counted once
  group_by(hhid) %>%
  summarise(area_A_ha = sum(area_sqm) / 10000, .groups = "drop")

check_unique(hh_crops, "Step 4 crops")
cat("Share of households with HHI = 1 (single crop):",
    round(mean(hh_crops$crop_hhi == 1), 3), "\n")

# ============================================================
# Step 5: programme proxies (AHS Section 6)
# For validating HHI as a proxy for LUC. NOT controls: these are
# CIP institutions and sit on the causal path.
# ============================================================

programmes <- ahs_s6 %>%
  mutate(hhid = id_num(hhid)) %>%
  group_by(hhid) %>%
  summarise(
    coop     = any_yes(s6q10),
    contract = any_yes(s6q7),
    crop_ins = any_yes(s6q8),
    .groups  = "drop"
  )

check_unique(programmes, "Step 5 programmes")

# ============================================================
# Step 6: district LUC intensity
# TODO: rebuild dist_luc in the SAS script as an area-weighted share
#   sum(area x weight x consolidated) / sum(area x weight)
# over agricultural plots, and add a Season A only version.
# ============================================================

dist_luc_join <- dist_luc %>% select(district_code, luc_intensity)
cat("Districts in base not found in dist_luc:",
    setdiff(unique(base$district_code), dist_luc_join$district_code), "\n")

# ============================================================
# Step 7: assemble
# ============================================================

master <- base %>%
  left_join(hh_comp,    by = "hhid") %>%
  left_join(hh_head,    by = "hhid") %>%
  left_join(land,       by = "hhid") %>%
  left_join(hh_crops,   by = "hhid") %>%
  left_join(hh_area_A,  by = "hhid") %>%
  left_join(programmes, by = "hhid") %>%
  left_join(dist_luc_join, by = "district_code") %>%
  mutate(
    in_ahs     = !is.na(crop_hhi),
    log_land   = log(pmax(land_ha, 0.001)),   # floor at 0.001 ha (one household)
    ur_f       = factor(ur),
    province_f = factor(province)
  )

check_unique(master, "Master")
stopifnot(nrow(master) == nrow(base))

# ---- Final checks -------------------------------------------
cat("\nPrimary (AHS) sample:", sum(master$in_ahs), "households\n")

cat("\nMissing values within the AHS sample:\n")
master %>%
  filter(in_ahs) %>%
  summarise(across(c(log_food_ae_real, crop_hhi, prio_share, luc_intensity, log_land,
                     hh_size, dep_ratio, head_age, head_female, head_educ,
                     wt_ahs), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print()

cat("\nSeason A area larger than land held (should be rare):",
    sum(master$area_A_ha > master$land_ha * 1.05, na.rm = TRUE), "\n")

cat("Correlation, nominal vs real log food per ae:",
    round(cor(master$log_food_ae_nominal, master$log_food_ae_real,
              use = "complete.obs"), 3), "\n")

cat("Weighted share female-headed, AHS sample (NISR reports 25.7%):",
    round(with(filter(master, in_ahs),
               weighted.mean(head_female, wt_ahs, na.rm = TRUE)), 3), "\n")

cat("Correlation of HHI with cooperative membership (proxy check):",
    round(cor(master$crop_hhi, master$coop, use = "complete.obs"), 3), "\n")

saveRDS(master, file.path(processed_path, "master.rds"))


