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

# RESULTS: r = -0.096 and R² = 0.0096, essentially no collinearity between LUC intensity and wealth quintile!
# r is scale -1 to +1 : -0.096 means: knowing someone's wealth quintile tells you almost nothing about their district's LUC intensity. They're independent.
# R² is scale 0 to 1 : 0.0096 means: only 0.96% of the variance in LUC intensity is explained by wealth quintile. basically nothing.