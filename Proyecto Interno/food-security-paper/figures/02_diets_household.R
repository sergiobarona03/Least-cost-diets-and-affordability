########################################################
## Least-cost diets — Results section
## Figure 3: Real daily household cost (time series)
## Figure 4: Cost premiums CoNA/CoCA, CoRD/CoCA, CoRD/CoNA
########################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}
out_real <- file.path(base_dir, "food-security-paper", "output", "real")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "figures")

##----------------------------------------------------------
## Load data
##----------------------------------------------------------

coca_real <- read_excel(file.path(out_real, "coca_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day_real, Cost_1000kcal_real) %>%
  mutate(diet = "CoCA")

cona_real <- read_excel(file.path(out_real, "cona_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day_real, Cost_1000kcal_real) %>%
  mutate(diet = "CoNA")

cord_real <- read_excel(file.path(out_real, "cord_real.xlsx")) %>%
  select(ciudad, fecha, Demo_Group, Sex, cost_day_real, Cost_1000kcal_real) %>%
  mutate(diet = "CoRD")

diets_real <- bind_rows(coca_real, cona_real, cord_real) %>%
  mutate(
    fecha        = as.Date(fecha),
    year         = year(fecha),
    ciudad_label = recode(ciudad,
                          "BOGOTA"   = "Bogotá",
                          "MEDELLIN" = "Medellín",
                          "CALI"     = "Cali"),
    diet         = factor(diet, levels = c("CoCA", "CoNA", "CoRD"))
  )

##----------------------------------------------------------
## Aesthetics
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

premium_colors <- c(
  "CoNA / CoCA" = "#C0392B",
  "CoRD / CoCA" = "#27AE60",
  "CoRD / CoNA" = "#E67E22"
)

premium_linetypes <- c(
  "CoNA / CoCA" = "dashed",
  "CoRD / CoCA" = "dotdash",
  "CoRD / CoNA" = "solid"
)

premium_shapes <- c(
  "CoNA / CoCA" = 17,
  "CoRD / CoCA" = 15,
  "CoRD / CoNA" = 18
)

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
    legend.key.width = unit(1.2, "cm"),
    panel.grid.major =  element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey96", color = "grey70"),
    strip.text       = element_text(face = "bold", size = 9),
    plot.margin      = margin(6, 8, 6, 6)
  )

##----------------------------------------------------------
## Figure 3: Real daily per capita household cost
##
## Household total  = sum of cost_day_real across 3 members
## Per capita       = household total / 3
## One line per diet, one panel per city
##----------------------------------------------------------

# Vertical lines at January 1st of each year
year_breaks <- seq(as.Date("2019-01-01"), as.Date("2025-01-01"), by = "1 year")

hh_cost <- diets_real %>%
  group_by(diet, ciudad_label, fecha) %>%
  dplyr::summarize(
    hh_total   = sum(cost_day_real,  na.rm = TRUE),
    n_members  = n(),
    per_capita = hh_total / n_members,
    .groups    = "drop"
  )

# Points every 3 months
hh_points <- hh_cost %>%
  filter(month(fecha) %in% c(1, 4, 7, 10))

fig3 <- ggplot(hh_cost,
               aes(x        = fecha,
                   y        = per_capita,
                   color    = ciudad_label,
                   linetype = ciudad_label)) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_vline(xintercept = year_breaks,
             color      = "black",
             linewidth  = 0.3,
             linetype   = "solid") +
  geom_point(data  = hh_points,
             aes(shape = ciudad_label),
             size  = 1.6,
             alpha = 0.9) +
  facet_wrap(~ diet, ncol = 3, scales = "free_y") +
  # scale_color_manual(values    = diet_colors)    +
  # scale_linetype_manual(values = diet_linetypes) +
  # scale_shape_manual(values    = diet_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = comma_format(prefix   = "$",
                                           big.mark = ",",
                                           suffix   = "")) +
  labs(
    title    = " ",
    caption  = paste0(
      "Note: Per capita cost is the sum of daily diet costs across all three household members ",
      "(adult male, adult female, female child) divided by three.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet.\n",
      "Nutritional requirements follow GABAS dietary guidelines adjusted by ",
      "city-specific estimated energy requirements (EER)."
    ),
    x = NULL,
    y = "Real COP / day (per capita)"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig3_real_hh_percapita_series.png"),
       fig3, width = 10, height = 6, dpi = 300)

ggsave(file.path(out_fig, "fig3_real_hh_percapita_series.pdf"),
       fig3, width = 10, height = 6)

##----------------------------------------------------------
## Figure 3: Real year-over-year variation in daily per capita
## household cost
##
## YoY (%) = ((value_t / value_t-12) - 1) * 100
## One line per diet, one panel per city
##----------------------------------------------------------

hh_cost_yoy <- hh_cost %>%
  dplyr::group_by(diet, ciudad_label) %>%
  dplyr::arrange(fecha, .by_group = TRUE) %>%
  dplyr::mutate(
    yoy_per_capita = ((per_capita / lag(per_capita, 12)) - 1) * 100
  ) %>%
  ungroup()

# Points every 3 months
hh_points_yoy <- hh_cost_yoy %>%
  filter(month(fecha) %in% c(1, 4, 7, 10), !is.na(yoy_per_capita))

fig3_yoy <- ggplot(hh_cost_yoy %>% filter(!is.na(yoy_per_capita)),
                   aes(x        = fecha,
                       y        = yoy_per_capita,
                       color    = ciudad_label,
                       linetype = ciudad_label)) +
  geom_hline(yintercept = 0,
             color      = "grey50",
             linewidth  = 0.4,
             linetype   = "longdash") +
  geom_vline(xintercept = year_breaks,
             color      = "black",
             linewidth  = 0.3,
             linetype   = "solid") +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(data  = hh_points_yoy,
             aes(shape = ciudad_label),
             size  = 1.6,
             alpha = 0.9) +
  facet_wrap(~ diet, ncol = 3, scales = "free_y") +
  # scale_color_manual(values    = diet_colors)    +
  # scale_linetype_manual(values = diet_linetypes) +
  # scale_shape_manual(values    = diet_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title    = " ",
    caption  = paste0(
      "Note: Real year-over-year variation is computed as the percentage change ",
      "with respect to the same month in the previous year: ",
      "((value_t / value_t-12) - 1) × 100.\n",
      "Per capita cost is the sum of daily diet costs across all three household members ",
      "(adult male, adult female, female child) divided by three.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet."
    ),
    x = NULL,
    y = "Real year-over-year change (%)"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig3_real_hh_percapita_yoy.png"),
       fig3_yoy, width = 10, height = 6, dpi = 300)

ggsave(file.path(out_fig, "fig3_real_hh_percapita_yoy.pdf"),
       fig3_yoy, width = 10, height = 6)

##----------------------------------------------------------
## Figure 4: Cost premiums
##
## Three ratios at household level (per capita):
##   CoNA / CoCA
##   CoRD / CoCA
##   CoRD / CoNA
## Facet by city. Reference line at 1.
##----------------------------------------------------------

premiums <- hh_cost %>%
  select(diet, ciudad_label, fecha, per_capita) %>%
  pivot_wider(names_from  = diet,
              values_from = per_capita) %>%
  mutate(
    `CoNA / CoCA` = CoNA / CoCA,
    `CoRD / CoCA` = CoRD / CoCA,
    `CoRD / CoNA` = CoRD / CoNA
  ) %>%
  pivot_longer(cols      = c(`CoNA / CoCA`, `CoRD / CoCA`, `CoRD / CoNA`),
               names_to  = "premium",
               values_to = "ratio") %>%
  mutate(premium = factor(premium,
                          levels = c("CoNA / CoCA", "CoRD / CoCA", "CoRD / CoNA")))

# Points every 3 months
premium_points <- premiums %>%
  filter(month(fecha) %in% c(1, 4, 7, 10))

fig4 <- ggplot(premiums,
               aes(x        = fecha,
                   y        = ratio,
                   color    = premium,
                   linetype = premium)) +
  geom_hline(yintercept = 1,
             color      = "grey50",
             linewidth  = 0.4,
             linetype   = "longdash") +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(data  = premium_points,
             aes(shape = premium),
             size  = 1.6,
             alpha = 0.9) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_color_manual(values    = premium_colors)    +
  scale_linetype_manual(values = premium_linetypes) +
  scale_shape_manual(values    = premium_shapes)    +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title    = " ",
    caption  = paste0(
      "Note: CoNA/CoCA measures the premium of nutritional adequacy over caloric adequacy. ",
      "CoRD/CoCA measures the premium of the recommended diet over the least-cost diet.\n",
      "CoRD/CoNA measures the additional cost of dietary realism over nutritional adequacy. ",
      "A ratio of 1.20 indicates a 20% cost premium over the reference diet.\n",
      "All costs expressed as per capita household daily costs. ",
      "Long-dashed grey line at 1 indicates no premium.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of the Recommended Diet."
    ),
    x = NULL,
    y = "Cost ratio"
  ) +
  theme_q1

ggsave(file.path(out_fig, "fig4_cost_premiums.png"),
       fig4, width = 12, height = 6, dpi = 300)

ggsave(file.path(out_fig, "fig4_cost_premiums.pdf"),
       fig4, width = 12, height = 6)

message("Figures 3\u20134 saved to: ", out_fig)


library(patchwork)

##----------------------------------------------------------
## Figure 3: Per capit cost + y-o-y growth
##----------------------------------------------------------

# Remove x-axis text from fig3 since fig3_yoy shares the same axis
fig3_clean <- fig3 +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin  = margin(6, 8, 0, 6))   # remove bottom margin

fig3_yoy_clean <- fig3_yoy +
  theme(plot.margin = margin(0, 8, 6, 6))    # remove top margin

combined <- fig3_clean / fig3_yoy_clean +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A")

ggsave(file.path(out_fig, "fig3_combined.png"),
       combined, width = 12, height = 9, dpi = 300)

ggsave(file.path(out_fig, "fig3_combined.pdf"),
       combined, width = 12, height = 9)

##----------------------------------------------------------
## Final table cost premiums (city × ratio)
##----------------------------------------------------------

table <- premiums %>%
  group_by(ciudad_label, premium) %>%
  dplyr::summarize(
    mean = round(mean(ratio, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = ciudad_label,
    values_from = mean
  ) %>%
  arrange(premium)

# Exportar a Excel
writexl::write_xlsx(
  table,
  file.path(out_fig, "table_cost_premiums.xlsx")
)
