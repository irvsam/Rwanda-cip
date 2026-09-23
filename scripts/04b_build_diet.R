# ============================================================
# 04b_build_diet.R
# Builds household diet measures from the EICV7 food module
# (CS_S8B_Food_Expenditure_Consumption) and adds them to master.rds.
#
# Measures (all over the four recall visits, v2-v5):
#   hdds           : Household Dietary Diversity Score, number of the
#                    12 FAO food groups consumed (Swindale & Bilinsky,
#                    2006; FAO, 2010)
#   hdds_purch     : number of food groups with any item PURCHASED
#   hdds_own       : number of food groups with any item consumed
#                    from OWN PRODUCTION
#   hdds_nonstaple : number of the six non-staple groups consumed
#                    (vegetables, fruit, meat, eggs, fish, dairy),
#                    closest to the groups Del Prete et al. (2019)
#                    found were lost under consolidation
#   purch_share    : share of consumed items that were purchased at
#                    least once (market dependence, count-based)
#
# Counts rather than values: own-produced quantities come in mixed
# units with farmer-stated prices, so value shares add error.
# ============================================================

source("scripts/00_setup.R")

# Only the columns needed: ID, item, and the consumed / purchased /
# own-produced flags for visits 2-5 (cuts memory use by ~80%)
food <- read_dta(
  file.path(data_path, "EICV7/CS_S8B_Food_Expenditure_Consumption.dta"),
  col_select = c(hhid, s8bq0, matches("^s8bq(2|6|9)_v[2-5]$"))
)
master <- readRDS(file.path(processed_path, "master.rds"))

YES <- 1   # s8bq2 / s8bq6 / s8bq9: 1 = Yes, 2 = No (confirmed)

# ---- Item codes -> 12 FAO food groups ------------------------
# Edge cases (check these):
#   2  fresh bean: kept in legumes (shelled fresh beans)
#   3  string bean: vegetables (green pods)
#   9  cooking banana: roots/tubers (FAO counts plantains as starchy staples)
#   29 ubushera: beverage, not cereal
#   53 biscuits: cereals
#   67 ice cream: dairy
#   69-70 butter: oils and fats (FAO convention)
#   72 beer bananas: beverage (used for brewing)
#   93 green peas (fresh): vegetables; 94 dry peas: legumes
#   113 pepper (vegetable) vs 122/126 pepper (condiment)
#   124 mayonnaise, 125 tomato concentrate: condiments
# Excluded (cannot be classified): 128 baby food, 129 other food
#   items, 135 mineral water.
food_groups <- list(
  "Cereals"                        = c(10, 11, 16:19, 23, 24, 45:54),
  "White roots and tubers"         = c(5:9, 12:15, 114, 115),
  "Vegetables"                     = c(3, 20, 93, 95:113),
  "Fruits"                         = c(71, 73:86),
  "Meat"                           = c(36:44, 55:57),
  "Eggs"                           = 58,
  "Fish"                           = 59:63,
  "Legumes, nuts and seeds"        = c(1, 2, 4, 87:92, 94),
  "Milk and milk products"         = c(21, 22, 64:68),
  "Oils and fats"                  = c(31:35, 69, 70),
  "Sweets"                         = c(25, 26, 116:121),
  "Spices, condiments, beverages"  = c(27:30, 72, 122:127, 130:134, 136:148)
)
EXCLUDED_ITEMS <- c(128, 129, 135)

NONSTAPLE_GROUPS <- c("Vegetables", "Fruits", "Meat", "Eggs", "Fish",
                      "Milk and milk products")

item_map <- enframe(food_groups, name = "group", value = "item") %>%
  unnest(item)

# every item mapped exactly once, or explicitly excluded
stopifnot(!any(duplicated(item_map$item)),
          setequal(c(item_map$item, EXCLUDED_ITEMS), 1:148))

# ---- Household x item flags across the four visits ------------
yes_any <- function(...) {
  flags <- map(list(...), ~ coalesce(as.numeric(.x) == YES, FALSE))
  reduce(flags, `|`)
}

items <- food %>%
  transmute(
    hhid      = as.numeric(zap_labels(hhid)),
    item      = as.numeric(zap_labels(s8bq0)),
    consumed  = yes_any(s8bq6_v2, s8bq6_v3, s8bq6_v4, s8bq6_v5),
    purchased = yes_any(s8bq2_v2, s8bq2_v3, s8bq2_v4, s8bq2_v5),
    own       = yes_any(s8bq9_v2, s8bq9_v3, s8bq9_v4, s8bq9_v5),
    # was the consumption question answered at all for this item?
    answered  = !is.na(s8bq6_v2) | !is.na(s8bq6_v3) |
      !is.na(s8bq6_v4) | !is.na(s8bq6_v5)
  ) %>%
  filter(!item %in% EXCLUDED_ITEMS) %>%
  left_join(item_map, by = "item")

cat("Item rows per household (should be 148 for all):\n")
print(count(count(food, hhid), n))

# ---- Household-level measures --------------------------------
diet <- items %>%
  group_by(hhid) %>%
  summarise(
    hdds           = n_distinct(group[consumed]),
    hdds_purch     = n_distinct(group[purchased]),
    hdds_own       = n_distinct(group[own & consumed]),
    hdds_nonstaple = n_distinct(group[consumed & group %in% NONSTAPLE_GROUPS]),
    n_items        = sum(consumed),
    purch_share    = if (sum(consumed) > 0) sum(consumed & purchased) / sum(consumed)
    else NA_real_,
    any_answered   = any(answered),
    .groups = "drop"
  ) %>%
  # households with no answered consumption questions are missing,
  # not households that ate nothing
  mutate(across(c(hdds, hdds_purch, hdds_own, hdds_nonstaple, n_items, purch_share),
                ~ if_else(any_answered, as.numeric(.x), NA_real_)))

cat("Households with no answered consumption items (set to NA):",
    sum(!diet$any_answered), "\n")
diet <- select(diet, -any_answered)

stopifnot(!any(duplicated(diet$hhid)))
cat("\nDiet measures built for", nrow(diet), "households\n")

# ---- Checks --------------------------------------------------
cat("\nHDDS distribution (ceiling effects? most households at 11-12?):\n")
print(count(diet, hdds))

cat("\nNon-staple diversity distribution:\n")
print(count(diet, hdds_nonstaple))

# ---- Add to master -------------------------------------------
master <- master %>%
  select(-any_of(names(diet)[-1])) %>%   # safe to rerun
  left_join(diet, by = "hhid")

stopifnot(!any(duplicated(master$hhid)))

cat("\nMissing diet measures in AHS sample:",
    sum(is.na(master$hdds[master$in_ahs])), "\n")

cat("Correlation of HDDS with log food per ae (full sample):",
    round(cor(master$hdds, master$log_food_ae_real, use = "complete.obs"), 3), "\n")

master %>%
  filter(in_ahs) %>%
  summarise(across(c(hdds, hdds_purch, hdds_own, hdds_nonstaple, purch_share),
                   list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything()) %>%
  print(n = Inf)

saveRDS(master, file.path(processed_path, "master.rds"))