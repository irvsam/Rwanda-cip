# ============================================================
# 07_figures_maps.R
# Figures for the paper (greyscale-safe, serif).
#
#   Map 1    : district LUC intensity
#   Figure 1 : the mechanism chain. HHI effect (per one-SD increase)
#              on each outcome, in low- vs high-LUC districts
# ============================================================

if (!exists(".setup_done")) source("scripts/00_setup.R")
dist_luc   <- readRDS(file.path(processed_path, "dist_luc.rds"))
rwa_map    <- readRDS(file.path(processed_path, "rwa_map.rds"))
diet       <- readRDS(file.path(processed_path, "diet_models.rds"))
food_value <- readRDS(file.path(processed_path, "primary_models.rds"))

theme_paper <- theme_classic(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3))

# ============================================================
# MAP 1: District LUC intensity
# ============================================================

map_data <- rwa_map %>%
  left_join(dist_luc %>% mutate(district_code = as.character(district_code)),
            by = c("CC_2" = "district_code"))

stopifnot(sum(!is.na(map_data$luc_intensity)) == 30)   # every district joined

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = luc_intensity), colour = "white", linewidth = 0.1) +
  scale_fill_gradient(low = "grey90", high = "black",
                      name = "Land under consolidation (%)") +
  theme_void(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

ggsave(file.path(output_figures_path, "map_luc_intensity.png"), p_map,
       width = 7, height = 6, dpi = 300)

# ============================================================
# FIGURE 1: The mechanism chain
# Units differ by outcome, so each panel has its own y-axis.
# ============================================================

chain_order <- c(
  "Own-produced groups"      = "1. Own-produced food groups",
  "Purchased share of items" = "2. Purchased share (pp)",
  "Purchased groups"         = "3. Purchased food groups",
  "Dietary diversity (HDDS)" = "4. Dietary diversity (HDDS)",
  "Non-staple groups"        = "4b. Non-staple food groups",
  "Log food value"           = "5. Food value (%)"
)

fig_data <- bind_rows(
  diet$diet_marginal %>%
    mutate(across(c(effect_1sd, low_1sd, high_1sd),
                  ~ if_else(outcome == "Purchased share of items", 100 * .x, .x))),
  food_value$marginal_effects %>%
    transmute(outcome = "Log food value", luc_pctile, luc_intensity,
              effect_1sd = pct_1sd, low_1sd = pct_1sd_low, high_1sd = pct_1sd_high)
) %>%
  filter(luc_pctile %in% c("p10", "p90")) %>%
  mutate(
    outcome  = factor(chain_order[outcome], levels = chain_order),
    district = factor(if_else(luc_pctile == "p10", "Low-LUC district (p10)",
                              "High-LUC district (p90)"),
                      levels = c("Low-LUC district (p10)", "High-LUC district (p90)"))
  )

p_chain <- ggplot(fig_data, aes(x = district, y = effect_1sd, shape = district)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = low_1sd, ymax = high_1sd), width = 0.15, linewidth = 0.5) +
  geom_point(size = 2.8, fill = "white") +
  scale_shape_manual(values = c(16, 21), name = NULL) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 2) +
  labs(x = NULL,
       y = "Association with a one-SD increase in crop concentration (95% CI)") +
  theme_paper +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave(file.path(output_figures_path, "fig1_mechanism_chain.png"), p_chain,
       width = 9, height = 5.5, dpi = 300)

message("Figures saved to ", output_figures_path)