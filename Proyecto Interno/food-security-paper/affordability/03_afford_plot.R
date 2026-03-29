########################################################
## Affordability analysis — Figures
## Figure 1: Non-affordability heatmap by decile × diet × city
## Figure 2: Overall non-affordability rate — facet by city
## Figure 3: Cost-to-expenditure ratio (capped at 5)
########################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir   <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"
afford_dir <- file.path(base_dir, "food-security-paper", "output", "affordability_metrics")
out_fig    <- file.path(base_dir, "food-security-paper", "output", "figures")

##----------------------------------------------------------
## Load data
##----------------------------------------------------------

results     <- readRDS(file.path(afford_dir, "Afford_results_monthly.rds"))
afford_pov  <- results$poverty   %>% mutate(fecha = as.Date(fecha))
afford_food <- results$mean_food %>% mutate(fecha = as.Date(fecha))

##----------------------------------------------------------
## Shared aesthetics
##----------------------------------------------------------

diet_colors <- c(
  "CoCA" = "#2C3E6B",
  "CoNA" = "#C0392B",
  "CoRD" = "#27AE60"
)

diet_linetypes <- c(
  "CoCA" = "solid",
  "CoNA" = "dashed",
  "CoRD" = "dotdash"
)

diet_shapes <- c(
  "CoCA" = 16,
  "CoNA" = 17,
  "CoRD" = 15
)

city_labels <- c(
  "BOGOTÁ D.C." = "Bogotá",
  "CALI"        = "Cali",
  "MEDELLÍN"    = "Medellín"
)

decile_order <- paste0("Decil ", 1:10)

theme_q1 <- theme_bw(base_size = 10) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 11,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9, color = "grey35",
                                    margin = margin(b = 6)),
    plot.caption     = element_text(size = 7.5, color = "grey45",
                                    hjust = 0, margin = margin(t = 6)),
    axis.title       = element_text(size = 9),
    axis.text        = element_text(size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.0, "cm"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey96", color = "grey70"),
    strip.text       = element_text(face = "bold", size = 9),
    plot.margin      = margin(6, 8, 6, 6)
  )

##----------------------------------------------------------
## Prepare afford_pov
##----------------------------------------------------------

afford_pov <- afford_pov %>%
  mutate(
    ciudad_label = recode(ciudad, !!!city_labels),
    deciles      = factor(deciles, levels = decile_order),
    diet         = factor(model, levels = c("CoCA", "CoNA", "CoRD"))
  )

##----------------------------------------------------------
## Figure 1: Non-affordability heatmap
##           x: fecha | y: decile | fill: rate (discrete)
##           facet: diet (rows) × city (columns)
##----------------------------------------------------------

rate_breaks <- c(0, 10, 25, 50, 75, 100)
rate_labels <- c("0\u201310%", "10\u201325%", "25\u201350%", "50\u201375%", "75\u2013100%")

rate_palette <- c(
  "0\u201310%"   = "grey75",
  "10\u201325%"  = "#CCDAEA",
  "25\u201350%"  = "#7FAED4",
  "50\u201375%"  = "#2C6FAC",
  "75\u2013100%" = "#0D2D5A"
)

afford_heatmap <- afford_pov %>%
  mutate(
    rate_bin = cut(rate,
                   breaks         = rate_breaks,
                   labels         = rate_labels,
                   include.lowest = TRUE,
                   right          = TRUE),
    rate_bin = factor(rate_bin, levels = rate_labels)
  )

fig1 <- ggplot(afford_heatmap,
               aes(x = fecha, y = deciles, fill = rate_bin)) +
  geom_tile(color = NA) +
  facet_grid(diet ~ ciudad_label) +
  scale_fill_manual(
    values   = rate_palette,
    name     = "Non-affordability rate",
    na.value = "grey92",
    drop     = FALSE
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = c(0, 0)
  ) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Non-affordability rate = percentage of households whose estimated monthly food ",
      "expenditure is below the minimum per capita diet cost.\n",
      "Deciles computed monthly within each city using per capita household income ",
      "weighted by the GEIH expansion factor.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_q1 +
  theme(
    panel.grid       = element_blank(),
    legend.key.width = unit(0.9, "cm"),
    legend.key.size  = unit(0.45, "cm")
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path(out_fig, "fig_afford_heatmap.png"),
       fig1, width = 10, height = 6, dpi = 300)
ggsave(file.path(out_fig, "fig_afford_heatmap.pdf"),
       fig1, width = 10, height = 6)

##----------------------------------------------------------
## Figure 2: Overall non-affordability rate
##           x: fecha | y: rate | color/linetype/shape: diet
##           facet: city (columns)
##----------------------------------------------------------

afford_overall <- afford_pov %>%
  group_by(ciudad_label, fecha, diet) %>%
  dplyr::summarize(
    rate_overall = mean(rate, na.rm = TRUE),
    .groups      = "drop"
  )

afford_points <- afford_overall %>%
  filter(month(fecha) == 12)

fig2 <- ggplot(afford_overall,
               aes(x        = fecha,
                   y        = rate_overall,
                   color    = diet,
                   linetype = diet)) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(data  = afford_points,
             aes(shape = diet),
             size  = 1.8, alpha = 0.9) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_color_manual(values    = diet_colors)    +
  scale_linetype_manual(values = diet_linetypes) +
  scale_shape_manual(values    = diet_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, NA)) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Non-affordability rate = percentage of households whose estimated monthly food ",
      "expenditure is below the minimum per capita diet cost.\n",
      "Rates are unweighted means across income deciles within each city\u2013month.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet."
    ),
    x = NULL,
    y = "Non-affordability rate (%)"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig_afford_overall_rate.png"),
       fig2, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_fig, "fig_afford_overall_rate.pdf"),
       fig2, width = 8, height = 5)

##----------------------------------------------------------
## Figure 3: Cost-to-expenditure ratio by decile × city
##           x: fecha | y: ratio (capped at 5)
##           facet: decile (rows) × city (columns)
##           Reference line at 1
##----------------------------------------------------------

afford_food_long <- afford_food %>%
  mutate(
    ciudad_label  = recode(ciudad, !!!city_labels),
    decile_groups = factor(decile_groups, levels = decile_order)
  ) %>%
  select(ciudad_label, fecha, decile_groups, ratio_1, ratio_2, ratio_3) %>%
  pivot_longer(
    cols      = c(ratio_1, ratio_2, ratio_3),
    names_to  = "diet",
    values_to = "ratio"
  ) %>%
  mutate(
    diet = recode(diet,
                  "ratio_1" = "CoCA",
                  "ratio_2" = "CoNA",
                  "ratio_3" = "CoRD"),
    diet = factor(diet, levels = c("CoCA", "CoNA", "CoRD")),
    # Cap ratio at 5 for readability — values above 5 are shown at 5
    ratio_capped  = pmin(ratio, 5),
    above_cap     = ratio > 5
  )

# Selected deciles for readability
deciles_show <- paste0("Decil ", c(1))

afford_food_sel <- afford_food_long %>%
  filter(decile_groups %in% deciles_show) %>%
  mutate(decile_groups = factor(decile_groups,
                                levels = paste0("Decil ", c(1, 2, 3, 5, 7, 10))))

food_points <- afford_food_sel %>%
  filter(month(fecha) == 12)

fig3 <- ggplot(afford_food_sel,
               aes(x        = fecha,
                   y        = ratio_capped,
                   color    = diet,
                   linetype = diet)) +
  geom_hline(yintercept = 1, linetype = "longdash",
             color = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.55, alpha = 0.9) +
  geom_point(data  = food_points,
             aes(shape = diet),
             size  = 1.6, alpha = 0.9) +
  facet_grid(decile_groups ~ ciudad_label) +
  scale_color_manual(values    = diet_colors)    +
  scale_linetype_manual(values = diet_linetypes) +
  scale_shape_manual(values    = diet_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.1),
    limits = c(0, 5),
    breaks = c(0, 1, 2, 3, 4, 5)
  ) +
  labs(
    title   = " ",
    caption = paste0(
      "Note: Cost-to-expenditure ratio = minimum per capita monthly diet cost divided by ",
      "mean per capita food expenditure within each decile.\n",
      "Values above 1 indicate that the diet cost exceeds the average food budget of the decile. ",
      "Values above 5 are capped at 5 for readability. ",
      "Long-dashed grey line at 1 indicates no gap.\n",
      "Only deciles 1, 2, 3, 5, 7, and 10 are shown. ",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet."
    ),
    x = NULL,
    y = "Cost-to-expenditure ratio"
  ) +
  theme_q1 +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(file.path(out_fig, "fig_afford_cost_exp_ratio.png"),
       fig3, width = 7, height = 4, dpi = 300)
ggsave(file.path(out_fig, "fig_afford_cost_exp_ratio.pdf"),
       fig3, width = 7, height = 4)

message("Affordability figures saved to: ", out_fig)