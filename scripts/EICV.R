library(haven)
library(tidyverse)

# Load files
hh_data <- read_dta("data/EICV7/CS_S01_S5_S7_Household.dta")
poverty_data <- read_dta("data/EICV7/CS_EICV7_poverty_file.dta")
savings_data <- read_dta("data/EICV7/CS_S10C_Savings.dta")
credits_data <- read_dta("data/EICV7/CS_S10A1_A2_credits.dta")
expenditure_data <- read_dta("data/EICV7/CS_S8B_Food_Expenditure_Consumption.dta")


# Extract labels to a simple data frame you can search
labels_list <- map_df(hh_data, ~attr(.x, "label") %||% NA) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "label")

# Find shock variables
filter(labels_list, str_detect(label, "shock|recover"))

# Shows column names alongside their descriptive labels

View(labelled::look_for(poverty_data))
View(labelled::look_for(expenditure_data))
