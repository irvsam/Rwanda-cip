library(haven)
library(tidyverse)

# Load files
hh_data <- read_dta("data/EICV7/CS_S01_S5_S7_Household.dta")
poverty_data <- read_dta("data/EICV7/CS_EICV7_poverty_file.dta")
savings_data <- read_dta("data/EICV7/CS_S10C_Savings.dta")
credits_data <- read_dta("data/EICV7/CS_S10A1_A2_credits.dta")
expenditure_data_A <- read_dta("data/EICV7/CS_S8A1_Expenditure.dta")
expenditure_data_B <- read_dta("data/EICV7/CS_S8A2_Expenditure.dta")
expenditure_data_C <- read_dta("data/EICV7/CS_S8A3_Expenditure.dta")



# Extract labels to a simple data frame you can search
labels_list <- map_df(hh_data, ~attr(.x, "label") %||% NA) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "label")

# Find shock variables
filter(labels_list, str_detect(label, "shock|recover"))

# Shows column names alongside their descriptive labels

View(labelled::look_for(poverty_data))
View(labelled::look_for(savings_data))
View(labelled::look_for(credits_data))
View(labelled::look_for(expenditure_data_A))
View(labelled::look_for(expenditure_data_B))
View(labelled::look_for(expenditure_data_C))


hh_data <- read_dta("data/EICV7/CS_S01_S5_S7_Household.dta")

# How many households reported a shock
hh_data %>% count(s5eq1)

# Of those, how many have a recovery time recorded
hh_data %>%
  filter(as.numeric(s5eq1) == 1) %>%
  count(!is.na(s5eq4a))

# Distribution of recovery time among those who have it
hh_data %>%
  filter(as.numeric(s5eq1) == 1, !is.na(s5eq4a)) %>%
  summarise(
    n = n(),
    mean = mean(s5eq4a),
    median = median(s5eq4a),
    max = max(s5eq4a)
  )

hh_data %>%
  filter(as.numeric(s5eq1) == 1, !is.na(s5eq4a)) %>%
  count(s5eq4a) %>%
  print(n = 50)


hh_data %>%
  filter(as.numeric(s5eq1) == 1) %>%
  count(s5eq2a) %>%
  print(n = 50)


hh_data %>%
  filter(as.numeric(s5eq2a) %in% c(1,2,3,4,5,6,8,9)) %>%
  count(s5eq4a) %>%
  mutate(s5eq4a = as_factor(s5eq4a))


shocks <- hh_data %>%
  filter(as.numeric(s5eq1) == 1) %>%
  mutate(s5eq2a = as_factor(s5eq2a)) %>%
  count(s5eq2a)

copingstrats <- hh_data %>%
  filter(as.numeric(s5eq1) == 1) %>%
  mutate(s5eq3a = as_factor(s5eq3a)) %>%
  count(s5eq3a)
