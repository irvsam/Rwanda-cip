# binscatter --------------------------------------------------------------
# Figure 2: priority-crop concentration and diets in the raw data.
# Own-produced groups, purchased groups and non-staple groups by
# QUINTILE of the priority part of HHI, in low- vs high-LUC districts
# (split at the district median).
#
# Outcomes are residualised on the full control set plus the
# non-priority part (as in the primary split models) and shown relative
# to the sample average, on a SHARED y-axis. No CIs: bin-level intervals
# would not be clustered by district. Inference is in tab:mechanism.


if (!exists(".setup_done")) source("scripts/00_setup.R")
master  <- readRDS(file.path(processed_path, "master.rds"))
primary <- make_primary(master)

if (!exists("theme_paper")) {   # same theme as 07_figures_maps.R
  theme_paper <- theme_classic(base_size = 12, base_family = "serif") +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(size = 11),
          panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3))
}

bin_outcomes <- c(
  hdds_own       = "Own-produced food groups",
  hdds_purch     = "Purchased food groups",
  hdds_nonstaple = "Non-staple food groups"
)

# Median LUC across the 30 districts, not across households
luc_med <- primary %>%
  distinct(district_code, luc_intensity) %>%
  pull(luc_intensity) %>%
  median()

# Residuals have mean zero, so y reads as food groups relative to
# the average household with the same controls
for (y in names(bin_outcomes)) {
  fit <- lm(reformulate(c(CONTROLS, "hhi_nonprio_c"), response = y), data = primary,
            na.action = na.exclude)
  primary[[paste0(y, "_adj")]] <- as.numeric(resid(fit))
}

bin_data <- primary %>%
  mutate(
    luc_group = factor(if_else(luc_intensity > luc_med,
                               "High-LUC districts", "Low-LUC districts"),
                       levels = c("Low-LUC districts", "High-LUC districts")),
    hhi_bin = ntile(hhi_prio, 5)   # quintiles on the full sample
  ) %>%
  pivot_longer(all_of(paste0(names(bin_outcomes), "_adj")),
               names_to = "outcome", values_to = "value") %>%
  mutate(outcome = factor(bin_outcomes[sub("_adj$", "", outcome)],
                          levels = bin_outcomes)) %>%
  group_by(luc_group, outcome, hhi_bin) %>%
  summarise(hhi = mean(hhi_prio), value = mean(value, na.rm = TRUE),
            n = n(), .groups = "drop")

cat("Households per bin (min / max):", min(bin_data$n), "/", max(bin_data$n), "\n")

p_bins <- ggplot(bin_data, aes(hhi, value, linetype = outcome)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  facet_wrap(~ luc_group) +
  labs(x = "Priority-crop concentration (priority part of HHI), quintile means",
       y = "Food groups relative to average\n(adjusted for controls)") +
  theme_paper +
  theme(legend.key.width = unit(1.5, "cm"))

ggsave(file.path(output_figures_path, "fig2_binscatter.png"), p_bins,
       width = 9, height = 4.2, dpi = 300)

message("Figure 2 saved to ", output_figures_path)



# food group table --------------------------------------------------------


# Appendix table: the 12 FAO food groups, which count as
# non-staple, example EICV7 items, and the share of primary-sample
# households consuming each group (any / own-produced / purchased).
#
# Uses item_map, food_groups, NONSTAPLE_GROUPS and items from
# 04b_build_diet.R, so the definitions match the measures exactly.
# Run straight after 04b (run_all keeps them in memory).


if (!exists(".setup_done")) source("scripts/00_setup.R")
if (!exists("items") || !exists("item_map")) source("scripts/04b_build_diet.R")
master <- readRDS(file.path(processed_path, "master.rds"))

# ---- Step 1: item names by group (pick examples from this) ------
item_labels <- tibble(item  = unname(as.numeric(val_labels(food$s8bq0))),
                      label = names(val_labels(food$s8bq0)))

item_map %>%
  left_join(item_labels, by = "item") %>%
  mutate(group = factor(group, levels = names(food_groups))) %>%
  group_by(group) %>%
  summarise(items = paste(label, collapse = "; "), .groups = "drop") %>%
  { walk2(.$group, .$items, ~ cat("\n", as.character(.x), ":\n", .y, "\n")) }

# ---- Step 2: 2-3 examples per group (Kinyarwanda in \\textit{}) --
examples <- c(
  "Cereals"                       = "maize flour, sorghum, bread",
  "White roots and tubers"        = "Irish potato, cassava, cooking banana (\\textit{inyamunyo})",
  "Vegetables"                    = "cassava leaves (\\textit{isombe}), amaranth, tomato",
  "Fruits"                        = "banana (\\textit{imineke}), avocado, mango",
  "Meat"                          = "beef, goat, chicken",
  "Eggs"                          = "eggs",
  "Fish"                          = "fresh fish, dried small fish",
  "Legumes, nuts and seeds"       = "dry beans, groundnuts, soya",
  "Milk and milk products"        = "fresh milk, curdled milk",
  "Oils and fats"                 = "palm oil, peanut oil, margarine",
  "Sweets"                        = "sugar, sugarcane, honey",
  "Spices, condiments, beverages" = "salt, sorghum juice (\\textit{ubushera}), banana beer (\\textit{urwagwa})"
)
if (any(examples == "")) warning("Some food groups have no examples yet")

tex_escape <- function(x) gsub("([&%#_])", "\\\\\\1", x)

# ---- Step 3: shares of primary-sample households ----------------
# Same flag rules as the measures in 04b
ahs_ids <- master$hhid[master$in_ahs]

grp <- items %>%
  filter(hhid %in% ahs_ids) %>%
  group_by(hhid, group) %>%
  summarise(consumed  = any(consumed),
            own       = any(own & consumed),   # as hdds_own
            purchased = any(purchased & consumed),   # as hdds_purch
            .groups = "drop")

n_hh <- n_distinct(grp$hhid)
cat("Households in food group table:", n_hh, "\n")   # should be 3715

fg_tab <- grp %>%
  group_by(group) %>%
  summarise(across(c(consumed, own, purchased), ~ 100 * mean(.x)),
            .groups = "drop") %>%
  mutate(group = factor(group, levels = names(food_groups))) %>%
  arrange(group) %>%
  mutate(nonstaple = if_else(group %in% NONSTAPLE_GROUPS, "Yes", ""),
         examples  = tex_escape(examples[as.character(group)]))

print(fg_tab)

# ---- Step 4: LaTeX (needs booktabs and tabularx) -----------------
SOURCE_NOTE <- paste("\\textit{Source:} Author's calculations based on NISR EICV7 (2023/24),",
                     "AHS 2024 and SAS 2024.")
rows <- sprintf("%s & %s & %s & %.0f & %.0f & %.0f \\\\",
                fg_tab$group, fg_tab$nonstaple, fg_tab$examples,
                fg_tab$consumed, fg_tab$own, fg_tab$purchased)

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Food group classification and household consumption}",
  "\\label{tab:foodgroups}",
  "\\footnotesize",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabularx}{\\linewidth}{l c X r r r}",
  "\\toprule",
  " & & & \\multicolumn{3}{c}{\\% of households} \\\\",
  "\\cmidrule(l){4-6}",
  paste("Food group & Non-staple & Example EICV7 items & Consumed &",
        "\\begin{tabular}[c]{@{}c@{}}Own-\\\\produced\\end{tabular} & Purchased \\\\"),
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "",
  "\\vspace{0.5em}",
  "\\begin{minipage}{\\linewidth}",
  "\\footnotesize",
  paste0(
    "\\textit{Notes:} Groups follow the 12-group HDDS ",
    "\\parencite{swindaleHouseholdDietaryDiversity}. ",
    "Shares are for the primary sample (n = ", format(n_hh, big.mark = "{,}"),
    "): any item in the group over the four EICV7 recall visits. ",
    "Classification decisions: fresh beans and dry peas as legumes; ",
    "string beans and fresh peas as vegetables; cooking banana as a root/tuber; ",
    "butter as oils and fats; ice cream as dairy; \\textit{ubushera}, beer bananas and fruit juices as beverages. ",
    "Baby food, other food items and mineral water are excluded. ",
    "HDDS: Household Dietary Diversity Score."
  ),
  "",
  "\\vspace{0.3em}",
  SOURCE_NOTE,
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tex, file.path(output_tables_path, "food_groups.tex"))
message("Food group table saved to ", output_tables_path)