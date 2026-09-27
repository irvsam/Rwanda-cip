if (!exists(".setup_done")) source("scripts/00_setup.R")
master  <- readRDS(file.path(processed_path, "master.rds"))
primary <- make_primary(master)

# how big and how related are the two parts?
primary %>% summarise(across(c(hhi_prio, hhi_nonprio), list(mean = mean, sd = sd)),
                      r = cor(hhi_prio, hhi_nonprio)) %>% print()

split_outcomes <- c(
  "Own-produced groups" = "hdds_own",   "Purchased share"   = "purch_share",
  "Purchased groups"    = "hdds_purch", "HDDS"              = "hdds",
  "Non-staple groups"   = "hdds_nonstaple", "Log food value" = "log_food_ae_real", "Animal-source groups" = "hdds_asf"
)

split_models <- map(split_outcomes, ~ fit_cl(make_f_split(.x), primary))
diff_models  <- map(split_outcomes, ~ fit_cl(make_f_diff(.x),  primary))

# print the coefficients that matter, as in 05b
# save a modelsummary table of split_models (and optionally the
# diff row) to output_tables_path, then saveRDS both lists


split_terms <- c("hhi_prio_c", "hhi_nonprio_c",
                 "hhi_prio_c:luc_c", "luc_c:hhi_nonprio_c")

walk2(split_models, names(split_models), function(m, nm) {
  cat("\n", nm, "(split)\n")
  print(summary(m)$coefficients[split_terms, ])
})

# hhi_prio_c here = priority slope minus non-priority slope
walk2(diff_models, names(diff_models), function(m, nm) {
  cat("\n", nm, "(difference: priority minus non-priority)\n")
  print(summary(m)$coefficients[c("hhi_prio_c", "luc_c:hhi_prio_c"), ])
})