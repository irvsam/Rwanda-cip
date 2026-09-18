# ============================================================
# 04c_collinearity_check.R
# Is luc_intensity confounded with quintile_f? Since H2 will be tested
# via a quintile-split or LUC×quintile interaction, this checks how much
# independent variation is left in luc_intensity once quintile is
# accounted for — if LUC and quintile are correlated, the quintile-specific
# coefficients partly reflect compositional differences, not just
# differential sensitivity to LUC.
# ============================================================

source("scripts/00_setup.R")
analysis_data <- readRDS(file.path(processed_path, "analysis_data.rds"))

# Simple correlation (quintile treated as ordinal 1-5)
cor(analysis_data$luc_intensity, as.numeric(as.character(analysis_data$quintile_f)))

# How much of luc_intensity's variance is explained by quintile alone?
summary(lm(luc_intensity ~ quintile_f, data = analysis_data))$r.squared

# RESULTS: r = -0.096, R² = 0.0096 — essentially no collinearity between
# LUC intensity and wealth quintile. Knowing a household's quintile tells
# you almost nothing about its district's LUC intensity; they vary
# independently. Good news for interpreting quintile-specific LUC
# coefficients as genuine differences in sensitivity, not artifacts of
# LUC/quintile overlap.
# Real VIF check on the interaction/pooled model deferred to 05, once
# that model is specified.