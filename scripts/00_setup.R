# ============================================================
# 00_setup.R
# Libraries, paths, options. 
# ============================================================

# Loading necessary libraries

library(haven)
library(tidyverse)
library(labelled)
library(sf)
library(geodata)
library(modelsummary)   # regression tables (alt: stargazer)

data_path   <- "data/raw"
output_path <- "output"

if (!dir.exists(output_path)) dir.create(output_path)
if (!dir.exists(file.path(output_path, "figures"))) dir.create(file.path(output_path, "figures"))
if (!dir.exists(file.path(output_path, "tables")))  dir.create(file.path(output_path, "tables"))

output_figures_path <- file.path(output_path, "figures")
output_tables_path  <- file.path(output_path, "tables")

