library(haven)
library(tidyverse)

hh_data <- read_dta("data/EICV7/CS_S01_S5_S7_Household.dta")

# Build clean analysis dataset
hh_clean <- hh_data %>%
  mutate(
    had_shock = as.numeric(s5eq1) == 1,
    agri_weather_shock = as.numeric(s5eq2a) %in% c(1,2,3,4,5,6,8,9),
    not_recovered = case_when(
      as.numeric(s5eq4a) == 99 ~ 1L,
      !is.na(s5eq4a) ~ 0L,
      TRUE ~ NA_integer_
    ),
    district_code = as.numeric(district),
    quintile_f = as.factor(as.numeric(quintile)),
    ur_f = as.factor(as.numeric(ur))
  ) %>%
  filter(had_shock == TRUE,
         agri_weather_shock == TRUE,
         !is.na(not_recovered))

# Check
nrow(hh_clean)
count(hh_clean, not_recovered)
count(hh_clean, quintile_f)
count(hh_clean, district_code)

sas_a <- read_dta("data/SAS 2024/Season A/Rwa_raw_SeasonA2024_Screening.dta")
sas_b <- read_dta("data/SAS 2024/Season B/Rwa_raw_SeasonB2024_Screening.dta")
sas_c <- read_dta("data/SAS 2024/Season C/Rwa_raw_SeasonC2024_Screening.dta")

sas_a$season <- "A"
sas_b$season <- "B"
sas_c$season <- "C"


process_sas_season <- function(df, season_label) {
  df %>%
    filter(is.na(s2q7), !is.na(s2q12)) %>%
    group_by(s1q2) %>%
    summarise(
      total_ha_est = sum(Plot_size_ha * plot_weight, na.rm = TRUE),
      luc_ha_est = sum((Plot_size_ha * plot_weight)[as.numeric(s2q12) == 1], na.rm = TRUE)
    ) %>%
    mutate(
      season = season_label,
      seasonal_intensity = (luc_ha_est / total_ha_est) * 100
    )
}

dist_luc <- bind_rows(
  process_sas_season(sas_a, "A"),
  process_sas_season(sas_b, "B"),
  process_sas_season(sas_c, "C")
) %>%
  group_by(s1q2) %>%
  summarise(luc_intensity = mean(seasonal_intensity, na.rm = TRUE)) %>%
  mutate(district_code = as.numeric(s1q2))

nrow(dist_luc)
summary(dist_luc$luc_intensity)
print(dist_luc)

# Merge
analysis_data <- hh_clean %>%
  left_join(dist_luc, by = "district_code")

# Check merge quality
cat("Rows after merge:", nrow(analysis_data), "\n")
cat("Missing LUC:", sum(is.na(analysis_data$luc_intensity)), "\n")

# Test logistic regression without member for now
model <- glm(not_recovered ~ luc_intensity + quintile_f +
               luc_intensity:quintile_f +
               ur_f + as.factor(district_code),
             data = analysis_data,
             family = binomial(link = "logit"))

summary(model)

model3 <- glm(not_recovered ~ luc_intensity + quintile_f +
                luc_intensity:quintile_f +
                ur_f + as.factor(province),
              data = analysis_data,
              family = binomial(link = "logit"))

summary(model3)

poverty_data <- read_dta("data/EICV7/CS_EICV7_poverty_file.dta")

# Full household dataset - no shock filter this time
hh_all <- hh_data %>%
  mutate(
    district_code = as.numeric(district),
    quintile_f = as.factor(as.numeric(quintile)),
    ur_f = as.factor(as.numeric(ur)),
    province_f = as.factor(as.numeric(province))
  ) %>%
  left_join(poverty_data %>% select(hhid, food, sol_jan, ae, member), by = "hhid") %>%
  left_join(dist_luc, by = "district_code") %>%
  filter(!is.na(food), !is.na(luc_intensity))

nrow(hh_all)

model4 <- lm(log(food) ~ luc_intensity + quintile_f +
               luc_intensity:quintile_f +
               ur_f + province_f,
             data = hh_all)

summary(model4)

hh_data %>%
  filter(as.numeric(s5eq1) == 1) %>%
  count(s5eq3a) %>%
  mutate(s5eq3a = as_factor(s5eq3a)) %>%
  print(n = 50)


hh_data %>%
  filter(as.numeric(s5eq1) == 1,
         as.numeric(s5eq2a) %in% c(1,2,3,4,5,6,8,9)) %>%
  count(s5eq3a) %>%
  mutate(s5eq3a = as_factor(s5eq3a)) %>%
  print(n = 50)

hh_data %>%
  filter(as.numeric(s5eq1) == 1,
         as.numeric(s5eq2a) %in% c(1,2,3,4,5,6,8,9)) %>%
  summarise(
    n_s5eq3a = sum(!is.na(s5eq3a)),
    n_s5eq3b = sum(!is.na(s5eq3b)),
    n_s5eq3c = sum(!is.na(s5eq3c))
  )


distress_codes <- c(2, 3, 4, 8, 9, 11, 13, 14)
# 2=reduced food, 3=reduced health/education, 4=sold productive assets,
# 8=sold house/land, 9=withdrew child from school, 11=sold last female animals,
# 13=entire household migrated, 14=begging

hh_clean2 <- hh_data %>%
  mutate(
    had_shock = as.numeric(s5eq1) == 1,
    agri_weather_shock = as.numeric(s5eq2a) %in% c(1,2,3,4,5,6,8,9),
    distress_coping = case_when(
      as.numeric(s5eq3a) %in% distress_codes ~ 1L,
      !is.na(s5eq3a) ~ 0L,
      TRUE ~ NA_integer_
    ),
    district_code = as.numeric(district),
    quintile_f = as.factor(as.numeric(quintile)),
    ur_f = as.factor(as.numeric(ur)),
    province_f = as.factor(as.numeric(province))
  ) %>%
  filter(had_shock, agri_weather_shock, !is.na(distress_coping)) %>%
  left_join(dist_luc, by = "district_code")

# Check
count(hh_clean2, distress_coping)

# Run model
model5 <- glm(distress_coping ~ luc_intensity + quintile_f +
                luc_intensity:quintile_f +
                ur_f + province_f,
              data = hh_clean2,
              family = binomial(link = "logit"))

summary(model5)
