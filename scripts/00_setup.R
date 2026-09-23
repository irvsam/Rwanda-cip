# ============================================================
# 00_setup.R
# Libraries, paths, options and helpers shared across scripts.

# ============================================================

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

# Print value labels in 01b? Set TRUE when checking codes by hand.
if (!exists("CHECK_LABELS")) CHECK_LABELS <- FALSE

# ============================================================
# Shared helpers
# ============================================================

# Labelled Stata ID/code -> plain numeric
id_num <- function(x) as.numeric(zap_labels(x))

# Stop if a household-level table has duplicated hhid
check_unique <- function(df, name) {
  n_dup <- sum(duplicated(df$hhid))
  if (n_dup > 0) stop(name, ": ", n_dup, " duplicated hhid values")
  message(name, ": ", nrow(df), " households, hhid unique")
}

# ---- Model helpers (used by 05 and 05b so both use the identical
# sample, centring, specification and inference) --------------

# Primary estimation sample: AHS households, HHI and LUC centred on
# this sample
make_primary <- function(master) {
  master %>%
    filter(in_ahs) %>%
    mutate(
      crop_hhi_c = crop_hhi        - mean(crop_hhi),
      luc_c      = luc_intensity   - mean(luc_intensity),
      luc_A_c    = luc_intensity_A - mean(luc_intensity_A)
    )
}

# Predetermined controls (no consumption-based variables)
CONTROLS_GEO <- c("ur_f", "province_f")
CONTROLS_HH  <- c("hh_size", "dep_ratio", "head_age", "head_female", "head_educ")
CONTROLS     <- c(CONTROLS_GEO, CONTROLS_HH, "log_land")

make_f <- function(outcome, controls = CONTROLS) {
  reformulate(c("crop_hhi_c * luc_c", controls), response = outcome)
}

# OLS with CR2 standard errors clustered by district
fit_cl <- function(f, data, ...) {
  lm_robust(f, data = data, clusters = district_code, ...)
}

# Random-slope multilevel check (Heisig & Schaeffer, 2019)
fit_ml <- function(f, data) {
  lmer(update(f, . ~ . + (1 + crop_hhi_c | district_code)), data = data,
       control = lmerControl(optimizer = "bobyqa"))
}

# Marginal effect of HHI at the 10th, 50th and 90th percentiles of
# district LUC, in outcome units for a one-SD increase in HHI.
# t critical value uses (clusters - 1) df, conservative with 30.
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

.setup_done <- TRUE
message("Setup complete")