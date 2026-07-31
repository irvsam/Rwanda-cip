# ============================================================
# 04c_collinearity_check.R
# Is luc_intensity confounded with quintile_f? The descriptive
# table shows both shock rate and mean LUC falling with wealth
# quintile — this checks how much independent variation is left
# in luc_intensity once quintile is accounted for.
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

# Simple correlation (quintile treated as ordinal 1-5)
cor(analysis_data$luc_intensity, as.numeric(as.character(analysis_data$quintile_f)))

# How much of luc_intensity's variance is explained by quintile alone?
summary(lm(luc_intensity ~ quintile_f, data = analysis_data))$r.squared

# Variance inflation factors on the H2 model — anything above ~5-10 on
# luc_intensity, shock, or their interaction with quintile_f suggests
# the interaction term is unstable because of this overlap.
cor(analysis_data$luc_intensity, as.numeric(as.character(analysis_data$quintile_f)))