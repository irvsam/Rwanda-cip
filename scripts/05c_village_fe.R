# 05c_village_fe.R ---------------------
# Village (EICV7 cluster) fixed effects for the whole mechanism chain.
#
# All nine EICV7 households in a village are interviewed in the same
# 12-day sub-cycle, so village fixed effects absorb interview timing,
# local prices and local markets. Each household is compared only with
# its neighbours (581 villages, median seven AHS households each).
#
# Absorbed by the fixed effects, so left out of the formula:
#   province and urban/rural (constant within a village)
#   the LUC main effect (constant within a district)
# Still estimated: HHI and the HHI x LUC interaction.
#
# Village effects enter as explicit dummies rather than through
# lm_robust(fixed_effects = ), which can fall back to CR0. This keeps
# CR2 standard errors clustered by district, as in 05 and 05b.

if (!exists(".setup_done")) source("scripts/00_setup.R")
master  <- readRDS(file.path(processed_path, "master.rds"))
primary <- make_primary(master)

# ---- Sample checks -------------------------------------------
cat("Villages:", n_distinct(primary$clust), "\n")
print(primary %>% count(clust) %>% count(n, name = "n_villages"))
cat("Median AHS households per village:",
    median(count(primary, clust)$n), "\n")
stopifnot(nrow(primary %>% distinct(clust, district_code) %>%
                 count(clust) %>% filter(n > 1)) == 0)   # villages nested in districts

# ---- Models --------------------------------------------------
make_f_vfe <- function(outcome) {
  reformulate(c("crop_hhi_c", "crop_hhi_c:luc_c",
                setdiff(CONTROLS, c("province_f", "ur_f")),
                "factor(clust)"),
              response = outcome)
}

vfe_outcomes <- c(
  "Own-produced groups" = "hdds_own",
  "Purchased share"     = "purch_share",
  "Purchased groups"    = "hdds_purch",
  "HDDS"                = "hdds",
  "Non-staple groups"   = "hdds_nonstaple",
  "Log food value"      = "log_food_ae_real"
)

vfe_models <- map(vfe_outcomes, ~ fit_cl(make_f_vfe(.x), primary))

walk2(vfe_models, names(vfe_models), function(m, nm) {
  cat("\n", nm, "\n")
  print(summary(m)$coefficients[c("crop_hhi_c", "crop_hhi_c:luc_c"), ])
})

# ---- Table ---------------------------------------------------
VFE_NOTES <- paste(
  "CR2 standard errors clustered by district (30 clusters) in parentheses.",
  "All models include village (EICV7 cluster) fixed effects, household size,",
  "dependency ratio, head age, sex and education, and log land held.",
  "Province, urban/rural and the district LUC main effect are absorbed by the village fixed effects.",
  "HHI and LUC intensity are centred on their sample means. Reference education category: never attended."
)

modelsummary(
  vfe_models, coef_map = KEY_LABELS[c(1, 3, 4)], gof_map = c("nobs", "r.squared"),
  stars = TRUE, notes = VFE_NOTES,
  title = "Village fixed effects: crop concentration and household diets",
  output = file.path(output_tables_path, "village_fe.tex")
)

saveRDS(vfe_models, file.path(processed_path, "village_fe_models.rds"))