# 07_figures_maps.R ----------------
# Figures for the paper (greyscale-safe, serif).
#
#   Map 1    : district LUC intensity
#   Figure 1 : the mechanism chain. Priority-crop concentration effect
#              (per one-SD increase) on each outcome, low- vs high-LUC districts


if (!exists(".setup_done")) source("scripts/00_setup.R")
dist_luc   <- readRDS(file.path(processed_path, "dist_luc.rds"))
rwa_map    <- readRDS(file.path(processed_path, "rwa_map.rds"))

theme_paper <- theme_classic(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3))


# luc intensity map ----------------------------------------------------



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


# mechanism chain ---------------------------------------------------------
# Figure 3: association between a one-SD increase in PRIORITY-crop
# concentration and each outcome, in low- (p10) and high-LUC (p90)
# districts. CR2 with Satterthwaite df (slope_at_split in 00_setup).

split <- readRDS(file.path(processed_path, "split_models.rds"))

chain_order <- c(
  "Own-produced groups"  = "1. Own-produced food groups",
  "Purchased share"      = "2. Purchased share (pp)",
  "Purchased groups"     = "3. Purchased food groups",
  "Non-staple groups"    = "4. Non-staple food groups",
  "Animal-source groups" = "4b. Animal-source food groups",
  "Log food value"       = "5. Food value (%)"
)

fig_data <- split$split_marginal %>%
  filter(part == "Priority", luc_pctile %in% c("p10", "p90"),
         outcome %in% names(chain_order)) %>%
  mutate(
    across(c(effect_1sd, low_1sd, high_1sd),
           ~ case_when(outcome == "Purchased share" ~ 100 * .x,
                       outcome == "Log food value"  ~ 100 * (exp(.x) - 1),
                       TRUE ~ .x)),
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
       y = "Association with a one-SD increase in\npriority-crop concentration (95% CI)") +
  theme_paper +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave(file.path(output_figures_path, "fig1_mechanism_chain.png"), p_chain,
       width = 9, height = 5.5, dpi = 300)


# marginal effect curves ---------------------------------------------------
# Figure: effect of a one-SD increase in PRIORITY-crop concentration
# across the full range of district LUC intensity, with 95% CIs (grey
# band). Tick marks along the bottom show where the 30 districts lie, so
# the reader can see how much data sits behind each part of the curve.

curve_outcomes <- c(
  "Own-produced food groups (link 1)" = "hdds_own",
  "Purchased food groups (link 3)"    = "hdds_purch",
  "Non-staple food groups"            = "hdds_nonstaple",
  "Animal-source food groups"         = "hdds_asf"
)

curve_data <- map_dfr(curve_outcomes, ~ slope_curve_split(.x, primary)) %>%
  filter(part == "Priority") %>%
  mutate(outcome = factor(names(curve_outcomes)[match(outcome, curve_outcomes)],
                          levels = names(curve_outcomes)))

district_luc <- primary %>% distinct(district_code, luc_intensity)

p_curve <- ggplot(curve_data, aes(luc_intensity, effect_1sd)) +
  geom_ribbon(aes(ymin = low_1sd, ymax = high_1sd), fill = "grey85") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_rug(data = district_luc, aes(x = luc_intensity), inherit.aes = FALSE,
           sides = "b", length = unit(0.02, "npc"), colour = "grey30") +
  facet_wrap(~ outcome, scales = "free_y", nrow = 2) +
  labs(x = "District LUC intensity (% of agricultural land under consolidation)",
       y = "Change per one-SD increase in\npriority-crop concentration (95% CI)") +
  theme_paper

ggsave(file.path(output_figures_path, "fig_marginal_curve.png"), p_curve,
       width = 8, height = 6, dpi = 300)


message("Figures saved to ", output_figures_path)