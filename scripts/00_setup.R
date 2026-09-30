# all shared helpers an functions and initial setup

# reminder: install these first if not yet done...
# ---- Libraries ---------------------------------------------
library(haven)
library(labelled)
library(tidyverse)
library(sf)
library(terra)         # needed to convert gadm() output with st_as_sf()
library(geodata)       # gadm() district boundaries
library(estimatr)      # lm_robust(), CR2 cluster-robust SEs
library(lme4)
library(lmerTest)      # Satterthwaite p-values for lmer()
library(modelsummary)
library(clubSandwich)  # CR2 Satterthwaite df for marginal effects
library(kableExtra)

# ---- Paths -------------------------------------------------
data_path           <- "data/raw"
processed_path      <- "data/preprocessed"
output_path         <- "output"
output_figures_path <- file.path(output_path, "figures")
output_tables_path  <- file.path(output_path, "tables")

for (p in c(processed_path, output_figures_path, output_tables_path)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}

# ---- Options -----------------------------------------------
# LaTeX tables via kableExtra (booktabs), so they compile without the
# tabularray preamble that modelsummary's default backend requires
options(modelsummary_factory_latex = "kableExtra",
        modelsummary_format_numeric_latex = "plain")

# Use broom for tidy() and glance() methods, not modelsummary's own
options(modelsummary_get = "broom")

# Print value labels in 01b? Set TRUE when checking codes by hand.
if (!exists("CHECK_LABELS")) CHECK_LABELS <- FALSE


# Shared helpers - id number and uniqueness check -----------------------------


# Labelled Stata ID/code -> plain numeric
# function for handling the stata ids
id_num <- function(x) as.numeric(zap_labels(x))

# Stop if a household-level table has duplicated hhid
check_unique <- function(df, name) {
  n_dup <- sum(duplicated(df$hhid))
  if (n_dup > 0) stop(name, ": ", n_dup, " duplicated hhid values")
  message(name, ": ", nrow(df), " households, hhid unique")
}

# Model helpers (used by 05 and 05b so both use the identical sample, centring, specification and inference)  --------------

# Primary estimation sample: AHS households, HHI and LUC centred on
# this sample

make_primary <- function(master) {
  master %>%
    filter(in_ahs) %>%
    mutate(
      crop_hhi_c    = crop_hhi        - mean(crop_hhi),
      hhi_prio_c    = hhi_prio        - mean(hhi_prio),
      hhi_nonprio_c = hhi_nonprio     - mean(hhi_nonprio),
      luc_c         = luc_intensity   - mean(luc_intensity),
      luc_A_c       = luc_intensity_A - mean(luc_intensity_A)
    )
}

# Season A check sample: HHI (both parts) and LUC measured in Season A,
# recentred on this sample
make_season_A <- function(primary) {
  primary %>%
    filter(!is.na(crop_hhi_A)) %>%
    mutate(
      crop_hhi_c    = crop_hhi_A      - mean(crop_hhi_A),
      hhi_prio_c    = hhi_prio_A      - mean(hhi_prio_A),
      hhi_nonprio_c = hhi_nonprio_A   - mean(hhi_nonprio_A),
      luc_c         = luc_intensity_A - mean(luc_intensity_A)
    )
}

# Predetermined controls (no consumption-based variables)
CONTROLS_GEO <- c("ur_f", "province_f")
CONTROLS_HH  <- c("hh_size", "dep_ratio", "head_age", "head_female", "head_educ")
CONTROLS     <- c(CONTROLS_GEO, CONTROLS_HH, "log_land")

make_f <- function(outcome, controls = CONTROLS) {
  reformulate(c("crop_hhi_c * luc_c", controls), response = outcome)
}

# ---- Priority split (primary design) ------------------------
# HHI = priority part + non-priority part (sums of squared shares of
# priority and non-priority crops). Terms are written out in full so the
# interaction names are fixed: "hhi_nonprio_c:luc_c", not "luc_c:hhi_nonprio_c".
SPLIT_TERMS <- c("hhi_prio_c", "hhi_nonprio_c", "luc_c",
                 "hhi_prio_c:luc_c", "hhi_nonprio_c:luc_c")

make_f_split <- function(outcome, controls = CONTROLS) {
  reformulate(c(SPLIT_TERMS, controls), response = outcome)
}

# Same model, reparametrised (HHI = prio + nonprio): the hhi_prio_c
# coefficient is the priority slope MINUS the non-priority slope, and
# hhi_prio_c:luc_c the difference between their LUC interactions
DIFF_TERMS <- c("crop_hhi_c", "hhi_prio_c", "luc_c",
                "crop_hhi_c:luc_c", "hhi_prio_c:luc_c")

make_f_diff <- function(outcome, controls = CONTROLS) {
  reformulate(c(DIFF_TERMS, controls), response = outcome)
}

# Village fixed effects version: province, urban/rural and the LUC main
# effect are constant within a village, so they are left out
make_f_split_vfe <- function(outcome) {
  reformulate(c("hhi_prio_c", "hhi_nonprio_c",
                "hhi_prio_c:luc_c", "hhi_nonprio_c:luc_c",
                setdiff(CONTROLS, CONTROLS_GEO), "factor(clust)"),
              response = outcome)
}

# OLS with CR2 standard errors clustered by district
fit_cl <- function(f, data, se_type = "CR2", ...) {
  lm_robust(f, data = data, clusters = district_code, se_type = se_type, ...)
}

# Random-slope multilevel check (Heisig & Schaeffer, 2019)
fit_ml <- function(f, data) {
  lmer(update(f, . ~ . + (1 + crop_hhi_c | district_code)), data = data,
       control = lmerControl(optimizer = "bobyqa"))
}

# Random slopes for BOTH parts of HHI (Heisig & Schaeffer, 2019)
fit_ml_split <- function(f, data) {
  lmer(update(f, . ~ . + (1 + hhi_prio_c + hhi_nonprio_c | district_code)),
       data = data, control = lmerControl(optimizer = "bobyqa"))
}

# Marginal effect of each part of HHI at the 10th, 50th and 90th
# percentiles of district LUC, per one-SD increase in that part.
# Refits the model with lm() so clubSandwich can give CR2 standard
# errors WITH Satterthwaite df for each combination (slope + L x
# interaction), instead of a fixed 29 df.
slope_at_split <- function(outcome, data, controls = CONTROLS) {
  d   <- data %>% filter(!is.na(.data[[outcome]]))
  fit <- lm(make_f_split(outcome, controls), data = d)
  V   <- vcovCR(fit, cluster = d$district_code, type = "CR2")
  b   <- coef(fit)
  luc_mean <- mean(d$luc_intensity)
  
  pct <- d %>%
    distinct(district_code, luc_intensity) %>%
    summarise(p10 = quantile(luc_intensity, 0.10),
              p50 = quantile(luc_intensity, 0.50),
              p90 = quantile(luc_intensity, 0.90)) %>%
    pivot_longer(everything(), names_to = "luc_pctile", values_to = "luc_intensity")
  
  parts <- c(Priority = "hhi_prio", `Non-priority` = "hhi_nonprio")
  
  expand_grid(pct, part = names(parts)) %>%
    mutate(raw = parts[part], L = luc_intensity - luc_mean) %>%
    pmap_dfr(function(luc_pctile, luc_intensity, part, raw, L) {
      cvec <- setNames(rep(0, length(b)), names(b))
      cvec[paste0(raw, "_c")]       <- 1
      cvec[paste0(raw, "_c:luc_c")] <- L
      lc <- linear_contrast(fit, vcov = V, test = "Satterthwaite",
                            contrasts = matrix(cvec, nrow = 1,
                                               dimnames = list(NULL, names(b))))
      sd_part <- sd(d[[raw]])
      tibble(outcome = outcome, part = part, luc_pctile = luc_pctile,
             luc_intensity = luc_intensity, slope = lc$Est, se = lc$SE, df = lc$df,
             effect_1sd = lc$Est  * sd_part,
             low_1sd    = lc$CI_L * sd_part,
             high_1sd   = lc$CI_U * sd_part)
    })
}

# Marginal effect of each part of HHI across the whole range of district
# LUC, per one-SD increase in that part, with 95% CIs (CR2, Satterthwaite
# df for every point). Used for the marginal effect curve figure.
slope_curve_split <- function(outcome, data, controls = CONTROLS, n_grid = 100) {
  d   <- data %>% filter(!is.na(.data[[outcome]]))
  fit <- lm(make_f_split(outcome, controls), data = d)
  V   <- vcovCR(fit, cluster = d$district_code, type = "CR2")
  b   <- coef(fit)
  luc_range <- range(d$luc_intensity)
  grid <- seq(luc_range[1], luc_range[2], length.out = n_grid)
  L    <- grid - mean(d$luc_intensity)
  
  parts <- c(Priority = "hhi_prio", `Non-priority` = "hhi_nonprio")
  
  imap_dfr(parts, function(raw, part) {
    C <- matrix(0, nrow = n_grid, ncol = length(b), dimnames = list(NULL, names(b)))
    C[, paste0(raw, "_c")]       <- 1
    C[, paste0(raw, "_c:luc_c")] <- L
    lc <- as.data.frame(linear_contrast(fit, vcov = V, contrasts = C,
                                        test = "Satterthwaite"))
    sd_part <- sd(d[[raw]])
    tibble(outcome = outcome, part = part, luc_intensity = grid,
           effect_1sd = lc$Est * sd_part,
           low_1sd    = lc$CI_L * sd_part,
           high_1sd   = lc$CI_U * sd_part)
  })
}


# Plain-HHI version (comparison models in 05 and 05b).
# NOTE: CIs use a fixed 29 df, which is too narrow; slope_at_split()
# above uses Satterthwaite df and is the one used for the paper figures.
slope_at <- function(model, data, hhi = "crop_hhi_c", int = "crop_hhi_c:luc_c") {
  b <- coef(model)
  V <- vcov(model)
  df_clust <- n_distinct(data$district_code) - 1
  sd_hhi   <- sd(data$crop_hhi)
  luc_mean <- mean(data$luc_intensity)
  
  data %>%
    distinct(district_code, luc_intensity) %>%
    summarise(p10 = quantile(luc_intensity, 0.10),
              p50 = quantile(luc_intensity, 0.50),
              p90 = quantile(luc_intensity, 0.90)) %>%
    pivot_longer(everything(), names_to = "luc_pctile",
                 values_to = "luc_intensity") %>%
    mutate(
      L          = luc_intensity - luc_mean,
      slope      = unname(b[hhi] + L * b[int]),
      se         = unname(sqrt(V[hhi, hhi] + L^2 * V[int, int] + 2 * L * V[hhi, int])),
      effect_1sd = slope * sd_hhi,
      low_1sd    = (slope - qt(0.975, df_clust) * se) * sd_hhi,
      high_1sd   = (slope + qt(0.975, df_clust) * se) * sd_hhi
    )
}

# Labels for the coefficients reported in every table
KEY_LABELS <- c(
  "crop_hhi_c"       = "Crop concentration (HHI, centred)",
  "luc_c"            = "District LUC intensity (pp, centred)",
  "crop_hhi_c:luc_c" = "HHI x LUC intensity",
  "log_land"         = "Log agricultural land (ha)"
)

SPLIT_LABELS <- c(
  "hhi_prio_c"          = "Priority-crop concentration (centred)",
  "hhi_nonprio_c"       = "Non-priority concentration (centred)",
  "luc_c"               = "District LUC intensity (pp, centred)",
  "hhi_prio_c:luc_c"    = "Priority x LUC intensity",
  "hhi_nonprio_c:luc_c" = "Non-priority x LUC intensity",
  "log_land"            = "Log agricultural land (ha)"
)

DIFF_LABELS <- c(
  "hhi_prio_c"       = "Difference in slopes",
  "hhi_prio_c:luc_c" = "Difference x LUC intensity"
)

ALL_LABELS <- c(
  KEY_LABELS,
  "hh_size"                           = "Household size",
  "dep_ratio"                         = "Dependency ratio",
  "head_age"                          = "Head age",
  "head_female"                       = "Female head",
  "head_educAttended, no certificate" = "Head: attended, no certificate",
  "head_educPrimary"                  = "Head: primary",
  "head_educSecondary or TVET"        = "Head: secondary or TVET",
  "head_educTertiary"                 = "Head: tertiary",
  "ur_f2"                             = "Rural"
)

TABLE_NOTES <- paste(
  "CR2 standard errors clustered by district (30 clusters) in parentheses.",
  "All models include province fixed effects, urban/rural, household size,",
  "dependency ratio, head age, sex and education, and log land held.",
  "HHI and LUC intensity are centred on their sample means.",
  "Reference education category: never attended."
)

SPLIT_NOTES <- paste(
  "CR2 standard errors clustered by district (30 clusters) in parentheses.",
  "All models include province fixed effects, urban/rural, household size,",
  "dependency ratio, head age, sex and education, and log land held.",
  "Crop concentration (HHI, Seasons A and B) is split into the part from CIP priority crops",
  "and the part from all other crops; the two sum to the HHI.",
  "Both parts and LUC intensity are centred on their sample means.",
  "Reference education category: never attended."
)

STEP_NOTES <- paste(
  "CR2 standard errors clustered by district (30 clusters) in parentheses.",
  "(1) urban/rural and province fixed effects; (2) adds household size, dependency ratio",
  "and head age, sex and education; (3) adds log land held.",
  "HHI and LUC intensity are centred on their sample means. Reference education category: never attended."
)

.setup_done <- TRUE
message("Setup complete")