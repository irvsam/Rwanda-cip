# ============================================================
# 02_explore_codebook.R
# exploring variables etc
# run line by line
# ============================================================

source("scripts/00_setup.R")
source("scripts/01_load_eicv7.R")

# Search variable labels for a keyword
labels_list <- map_df(hh_data, ~ attr(.x, "label") %||% NA) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "label")

filter(labels_list, str_detect(label, "shock|recover"))

# Browse full codebooks
View(labelled::look_for(hh_data))
View(labelled::look_for(poverty_data))
View(labelled::look_for(savings_data))
View(labelled::look_for(credits_data))
View(labelled::look_for(expenditure_A))
View(labelled::look_for(expenditure_B))
View(labelled::look_for(expenditure_C))

# Quick sanity checks on the shock module used in the proposal
hh_data %>% count(s5eq1)

hh_data %>%
  filter(as.numeric(s5eq1) == 1) %>%
  count(s5eq2a) %>%
  mutate(s5eq2a = as_factor(s5eq2a)) %>%
  print(n = 50)

hh_data %>%
  filter(as.numeric(s5eq1) == 1) %>%
  count(s5eq3a) %>%
  mutate(s5eq3a = as_factor(s5eq3a)) %>%
  print(n = 50)

# SAS plot-weight column checks (confirm indices before trusting colnames()[n])
sas_a <- read_dta(file.path(data_path, "SAS 2024/Season A/Rwa_raw_SeasonA2024_Screening.dta"))
sas_b <- read_dta(file.path(data_path, "SAS 2024/Season B/Rwa_raw_SeasonB2024_Screening.dta"))
sas_c <- read_dta(file.path(data_path, "SAS 2024/Season C/Rwa_raw_SeasonC2024_Screening.dta"))
# View(labelled::look_for(sas_a))
colnames(sas_a)[49]  # should be 'plot_weight'
colnames(sas_b)[52]
colnames(sas_c)[41]