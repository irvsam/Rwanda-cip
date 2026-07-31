hh_clean <- hh_clean %>%
  left_join(dist_luc, by = "district_code")

# Check merge worked
cat("Missing LUC:", sum(is.na(hh_clean$luc_intensity)), "\n")

# Now run the three checks
# Recovery by province
hh_clean %>%
  group_by(province) %>%
  summarise(
    n = n(),
    pct_not_recovered = mean(not_recovered) * 100
  ) %>%
  mutate(province = as_factor(province))

# Recovery by wealth quintile
hh_clean %>%
  group_by(quintile_f) %>%
  summarise(
    n = n(),
    pct_not_recovered = mean(not_recovered) * 100
  )

# Recovery by LUC quartile
hh_clean %>%
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile) %>%
  summarise(
    n = n(),
    mean_luc = mean(luc_intensity),
    pct_not_recovered = mean(not_recovered) * 100
  )



# Is Kigali driving the non-result? Remove urban households
hh_clean %>%
  filter(as.numeric(ur) == 2) %>%  # Rural only
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile) %>%
  summarise(
    n = n(),
    mean_luc = mean(luc_intensity),
    pct_not_recovered = mean(not_recovered) * 100
  )

# What shock types dominate in high vs low LUC districts?
hh_clean %>%
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile, s5eq2a) %>%
  summarise(n = n()) %>%
  mutate(s5eq2a = as_factor(s5eq2a)) %>%
  print(n = 50)


hh_clean %>%
  filter(as.numeric(s5eq2a) == 4) %>%  # Drought only
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile) %>%
  summarise(
    n = n(),
    mean_luc = mean(luc_intensity),
    pct_not_recovered = mean(not_recovered) * 100
  )


# s7aq4 is "Did any member own/raise livestock or farm"
# but we need agricultural households specifically
# check what variables indicate farming
hh_clean %>%
  count(s7aq4) %>%
  mutate(s7aq4 = as_factor(s7aq4))


hh_clean %>%
  filter(as.numeric(s7aq4) == 1) %>%  # Farming households only
  mutate(luc_quartile = ntile(luc_intensity, 4)) %>%
  group_by(luc_quartile) %>%
  summarise(
    n = n(),
    mean_luc = mean(luc_intensity),
    pct_not_recovered = mean(not_recovered) * 100
  )

hh_clean %>%
  filter(as.numeric(s7aq4) == 1) %>%
  mutate(
    distress_coping = case_when(
      as.numeric(s5eq3a) %in% c(2,3,4,8,9,11,13,14) ~ 1L,
      !is.na(s5eq3a) ~ 0L,
      TRUE ~ NA_integer_
    ),
    luc_quartile = ntile(luc_intensity, 4)
  ) %>%
  group_by(luc_quartile) %>%
  summarise(
    n = n(),
    mean_luc = mean(luc_intensity),
    pct_distress = mean(distress_coping, na.rm = TRUE) * 100
  )