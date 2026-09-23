# ============================================================
# 00_setup.R
# Libraries, paths, options.
# ============================================================

library(haven)
library(tidyverse)
library(labelled)
library(sf)
library(terra)
library(geodata)
library(modelsummary)   # regression tables (alt: stargazer)
library(pandoc)
library(estimatr)       # lm_robust(), clustered SEs
library(lme4)           # multilevel model cross-check
library(modelsummary)
library(tinytex)
library(kableExtra)

data_path      <- "data/raw"
processed_path <- "data/preprocessed"
output_path    <- "output"

output_figures_path <- file.path(output_path, "figures")
output_tables_path  <- file.path(output_path, "tables")

for (p in c(processed_path, output_path, output_figures_path, output_tables_path)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}
