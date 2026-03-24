########################################################
## Cost of Nutritional Adequacy (CoNA)
## Figures: Binding constraints ($limit) and
##          Shadow prices ($spe) — Q1 submission
########################################################

library(tidyverse)
library(scales)
library(patchwork)

##----------------------------------------------------------
## Directories and data
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"
out_cona <- file.path(base_dir, "food-security-paper", "output", "cona")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "figures")

cona <- readRDS(file.path(out_cona, "230326_cona_full.rds"))

df.limit <- cona$limit %>% mutate(fecha = as.Date(fecha), year = lubridate::year(fecha))
df.spe   <- cona$spe   %>% mutate(fecha = as.Date(fecha), year = lubridate::year(fecha))

##----------------------------------------------------------
## Shared aesthetics
##----------------------------------------------------------

city_colors <- c(
  "BOGOTA"   = "#2C3E6B",
  "MEDELLIN" = "#C0392B",
  "CALI"     = "#27AE60"
)

city_labels <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

member_labels <- c(
  "0_[31,51)"  = "Adult male",
  "1_[10, 14)" = "Female child",
  "1_[31,51)"  = "Adult female"
)

paper_theme <- theme_minimal(base_size = 11) +
  theme(
    text              = element_text(family = "serif"),
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(size = 10, color = "grey40"),
    plot.caption      = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    panel.grid.minor  = element_blank(),
    strip.text        = element_text(face = "bold", size = 10)
  )

##----------------------------------------------------------
## Prepare $limit
##----------------------------------------------------------

df.limit <- df.limit %>%
  mutate(
    member = paste0(Sex, "_", Age),
    member = recode(member, !!!member_labels),
    ciudad_label = recode(ciudad, !!!city_labels)
  )

# Frequency of binding constraints (Limiting == 1) by nutrient
# aggregated across all city × date observations
binding_agg <- df.limit %>%                # lower bounds only
  group_by(Nutrients, ciudad, ciudad_label) %>%
  summarize(
    freq_binding = mean(Limiting == 1, na.rm = TRUE),
    .groups = "drop"
  )

binding_member <- df.limit %>%
  group_by(Nutrients, member, ciudad, ciudad_label) %>%
  summarize(
    freq_binding = mean(Limiting == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Reorder nutrients by average binding frequency
nutrient_order <- binding_agg %>%
  group_by(Nutrients) %>%
  summarize(mean_freq = mean(freq_binding)) %>%
  arrange(desc(mean_freq)) %>%
  pull(Nutrients)

binding_agg    <- binding_agg    %>% mutate(Nutrients = factor(Nutrients, levels = nutrient_order))
binding_member <- binding_member %>% mutate(Nutrients = factor(Nutrients, levels = nutrient_order))

##----------------------------------------------------------
## Figure 4: Frequency of binding constraints — aggregated
##----------------------------------------------------------

fig4 <- ggplot(binding_agg,
               aes(x = Nutrients, y = freq_binding, fill = ciudad_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = setNames(city_colors, city_labels[names(city_colors)])) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title    = "Figure 4. Frequency of binding nutritional constraints by city, 2019–2024",
    subtitle = "Share of city–month observations in which each nutrient lower bound is binding",
    caption  = paste("Note: A constraint is binding when the optimised diet exactly meets",
                     "the lower bound for a given nutrient. Results aggregate over all",
                     "city–month observations. Only minimum (lower bound) constraints shown."),
    x        = "Nutrient",
    y        = "Share of binding observations"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig4_binding_constraints_agg.png"),
       fig4, width = 10, height = 5, dpi = 300)

##----------------------------------------------------------
## Figure 5: Frequency of binding constraints — by member
##----------------------------------------------------------

fig5 <- ggplot(binding_member,
               aes(x = Nutrients, y = freq_binding, fill = ciudad_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
  facet_wrap(~ member, ncol = 1) +
  scale_fill_manual(values = setNames(city_colors, city_labels[names(city_colors)])) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title    = "Figure 5. Frequency of binding nutritional constraints by household member and city, 2019–2024",
    subtitle = "Share of city–month observations in which each nutrient lower bound is binding",
    caption  = paste("Note: A constraint is binding when the optimised diet exactly meets",
                     "the lower bound for a given nutrient. Only minimum (lower bound) constraints shown."),
    x        = "Nutrient",
    y        = "Share of binding observations"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig5_binding_constraints_member.png"),
       fig5, width = 11, height = 10, dpi = 300)

##----------------------------------------------------------
## Prepare $spe
##----------------------------------------------------------

df.spe <- df.spe %>%
  mutate(
    member       = paste0(Sex, "_", Age),
    member       = recode(member, !!!member_labels),
    ciudad_label = recode(ciudad, !!!city_labels),
    SPE_abs      = abs(SPE)                        # use absolute normalised shadow price
  )

# Mean absolute shadow price by nutrient × city — aggregated
spe_agg <- df.spe %>%
  filter(constraint == "Min") %>%
  group_by(Nutrients, ciudad, ciudad_label) %>%
  summarize(mean_spe = mean(SPE_abs, na.rm = TRUE), .groups = "drop")

# Mean absolute shadow price by nutrient × member × city
spe_member <- df.spe %>%
  filter(constraint == "Min") %>%
  group_by(Nutrients, member, ciudad, ciudad_label) %>%
  summarize(mean_spe = mean(SPE_abs, na.rm = TRUE), .groups = "drop")

# Reorder nutrients by mean shadow price
spe_order <- spe_agg %>%
  group_by(Nutrients) %>%
  summarize(avg = mean(mean_spe)) %>%
  arrange(desc(avg)) %>%
  pull(Nutrients)

spe_agg    <- spe_agg    %>% mutate(Nutrients = factor(Nutrients, levels = spe_order))
spe_member <- spe_member %>% mutate(Nutrients = factor(Nutrients, levels = spe_order))

##----------------------------------------------------------
## Figure 6: Shadow prices — aggregated, cross-city comparison
##----------------------------------------------------------

fig6 <- ggplot(spe_agg,
               aes(x = Nutrients, y = mean_spe, fill = ciudad_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = setNames(city_colors, city_labels[names(city_colors)])) +
  scale_y_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    title    = "Figure 6. Mean normalised shadow prices by nutrient and city, 2019–2024",
    subtitle = "Average absolute standardised shadow price (SPE) across all city–month observations",
    caption  = paste("Note: Shadow prices (SPE) measure the marginal cost reduction",
                     "from relaxing each nutritional constraint by one unit.",
                     "Values are normalised by the constraint level. Higher values",
                     "indicate nutrients that drive diet cost more strongly."),
    x        = "Nutrient",
    y        = "Mean |SPE|"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig6_shadow_prices_agg.png"),
       fig6, width = 10, height = 5, dpi = 300)

##----------------------------------------------------------
## Figure 7: Shadow prices — by household member
##----------------------------------------------------------

fig7 <- ggplot(spe_member,
               aes(x = Nutrients, y = mean_spe, fill = ciudad_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
  facet_wrap(~ member, ncol = 1) +
  scale_fill_manual(values = setNames(city_colors, city_labels[names(city_colors)])) +
  scale_y_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    title    = "Figure 7. Mean normalised shadow prices by household member and city, 2019–2024",
    subtitle = "Average absolute standardised shadow price (SPE) across all city–month observations",
    caption  = paste("Note: Shadow prices (SPE) measure the marginal cost reduction from relaxing",
                     "each nutritional constraint by one unit, normalised by the constraint level."),
    x        = "Nutrient",
    y        = "Mean |SPE|"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig7_shadow_prices_member.png"),
       fig7, width = 11, height = 10, dpi = 300)

message("Figures 4–7 saved to: ", out_fig)