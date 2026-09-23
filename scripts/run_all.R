# ============================================================
# run_all.R
# Runs the full pipeline in order, from raw data to tables and
# figures. Run from the project root (where Rwanda-cip.Rproj is):
#   source("scripts/run_all.R")
#
# Setup and raw data loading happen once; each script checks what
# is already in memory before loading anything again.
# ============================================================

rm(list = ls(all.names = TRUE))   # clean start, including .setup_done

CHECK_LABELS <- FALSE   # TRUE prints the value labels behind the codes

steps <- c(
  "00_setup.R",                 # libraries, paths, shared helpers
  "01a_load_eicv7.R",           # raw EICV7: poverty file, food module
  "01b_load_ahs.R",             # raw AHS: sections 1, 2, 3/4, 6 + join check
  "03_build_luc.R",             # district LUC intensity (SAS) -> dist_luc.rds
  "04a_build_analysis_data.R",  # master file                  -> master.rds
  "04b_build_diet.R",           # diet measures added          -> master.rds
  "04c_descriptives.R",         # descriptive tables and checks
  "05_main_models.R",           # food value (distal outcome)  -> primary_models.rds
  "05b_diet_models.R",          # diet outcomes, chain table   -> diet_models.rds
  "07_figures_maps.R"           # map and mechanism figure
)

dir.create("output", showWarnings = FALSE)
log_file <- file.path("output", "run_log.txt")
sink(log_file, split = TRUE)      # printed output also goes to the log (messages stay in the console)

t_start <- Sys.time()
for (s in steps) {
  cat("\n==================== ", s, " ====================\n")
  t0 <- Sys.time()
  source(file.path("scripts", s))
  cat(s, "done in", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n")
}
cat("\nPipeline finished in",
    round(difftime(Sys.time(), t_start, units = "mins"), 1), "min\n")

sink()
message("Log written to ", log_file)