########################################################
## Cost of Nutritional Adequacy (CoNA)
## Figures: Diet cost and food composition — Q1 submission
########################################################

library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

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

out_cona <- file.path(base_dir, "food-security-paper", "output", "cona")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "cona")

##----------------------------------------------------------
## Load data
##----------------------------------------------------------

path_xlsx <- file.path(out_cona, "230326_cona_full.xlsx")

df.cost  <- read_excel(path_xlsx, sheet = "cost")
df.comp  <- read_excel(path_xlsx, sheet = "comp")

df.cost <- df.cost %>% mutate(fecha = as.Date(fecha), year = lubridate::year(fecha))
df.comp <- df.comp %>% mutate(fecha = as.Date(fecha), year = lubridate::year(fecha))

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

paper_theme <- theme_bw(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 10)
  )

##----------------------------------------------------------
## Prepare $cost
##----------------------------------------------------------

df.cost <- df.cost %>%
  mutate(
    member       = paste0(Sex, "_", Demo_Group),
    member       = recode(member, !!!member_labels),
    ciudad_label = recode(ciudad, !!!city_labels)
  )

# Per capita cost (mean across members) by city × date
cost_pc <- df.cost %>%
  dplyr::group_by(ciudad, ciudad_label, fecha, year) %>%
  dplyr::summarize(
    cost_percap    = mean(cost_day, na.rm = TRUE),
    cost_household = sum(cost_day,  na.rm = TRUE),
    .groups = "drop"
  )

##----------------------------------------------------------
## Figure 1: Per capita CoNA — time series by city
##----------------------------------------------------------

fig1 <- ggplot(cost_pc, aes(x = fecha, y = cost_percap, color = ciudad_label)) +
  geom_line(linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE,
              linewidth = 0.4, linetype = "dashed", alpha = 0.6) +
  scale_color_manual(values = setNames(city_colors, city_labels[names(city_colors)])) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(prefix = "$", suffix = " COP")) +
  labs(
    title    = "Figure 1. Daily per capita Cost of Nutritional Adequacy (CoNA), 2019–2024",
    subtitle = "Daily cost per household member (Colombian pesos, COP)",
    caption  = paste("Note: Dashed lines represent LOESS trend. CoNA estimated for a representative",
                     "household comprising one adult male, one adult female, and one female child.",
                     "Per capita cost computed as the mean daily cost across household members."),
    x = NULL,
    y = "COP / day"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig1_cona_percapita_series.png"),
       fig1, width = 8, height = 4.5, dpi = 300)

##----------------------------------------------------------
## Figure 2: CoNA by household member — panel by city
##----------------------------------------------------------

fig2 <- ggplot(df.cost, aes(x = fecha, y = cost_day, color = ciudad_label)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ member, ncol = 3) +
  scale_color_manual(values = setNames(city_colors, city_labels[names(city_colors)])) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(prefix = "$")) +
  labs(
    title    = "Figure 2. Daily CoNA by household member and city, 2019–2024",
    subtitle = "Daily cost of meeting nutritional requirements per member (COP)",
    caption  = "Note: Caloric and nutritional requirements follow GABAS dietary guidelines.",
    x = NULL,
    y = "COP / day"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig2_cona_by_member.png"),
       fig2, width = 8, height = 9, dpi = 300)

##----------------------------------------------------------
## Prepare $comp
##----------------------------------------------------------

df.comp <- df.comp %>%
  mutate(
    member       = paste0(Sex, "_", Demo_Group),
    member       = recode(member, !!!member_labels),
    ciudad_label = recode(ciudad, !!!city_labels)
  )

# Keep only foods that appear in at least 5% of observations
# to avoid a cluttered legend
top_foods <- df.comp %>%
  dplyr::group_by(Food) %>%
  dplyr::summarize(freq = n_distinct(paste(ciudad, fecha)), .groups = "drop") %>%
  mutate(share = freq / max(freq)) %>%
  filter(share >= 0.05) %>%
  pull(Food)

# Monthly mean quantity by food × city — aggregated across members
comp_agg <- df.comp %>%
  filter(Food %in% top_foods) %>%
  dplyr::group_by(Food, ciudad, ciudad_label, fecha, year) %>%
  dplyr::summarize(quantity = mean(quantity, na.rm = TRUE), .groups = "drop")

# Monthly mean quantity by food × member × city
comp_member <- df.comp %>%
  filter(Food %in% top_foods) %>%
  dplyr::group_by(Food, member, ciudad, ciudad_label, fecha, year) %>%
  dplyr::summarize(quantity = mean(quantity, na.rm = TRUE), .groups = "drop")

##----------------------------------------------------------
## Figure 3: Food composition over time — aggregated
## Stacked area chart, one panel per city
##----------------------------------------------------------

fig3 <- ggplot(comp_agg,
               aes(x = fecha, y = quantity, fill = Food)) +
  geom_area(position = "stack", alpha = 0.85) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(suffix = " g")) +
  labs(
    title    = "Figure 3. Evolution of optimal diet composition by city, 2019–2024",
    subtitle = "Mean daily quantity (grams) of selected foods in the cost-minimising diet",
    caption  = paste("Note: Only foods appearing in at least 5% of city–month observations",
                     "are shown. Quantities represent the mean across household members.",
                     "Stacked areas reflect total diet volume."),
    x    = NULL,
    y    = "Quantity (g / day)",
    fill = "Food item"
  ) +
  paper_theme +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 3))

ggsave(file.path(out_fig, "fig3_comp_agg_area.png"),
       fig3, width = 12, height = 5, dpi = 300)

##----------------------------------------------------------
## Figure 4: Food composition over time — by household member
## One panel per member × city
##----------------------------------------------------------

fig4 <- ggplot(comp_member,
               aes(x = fecha, y = quantity, fill = Food)) +
  geom_area(position = "stack", alpha = 0.85) +
  facet_grid(member ~ ciudad_label) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(suffix = " g")) +
  labs(
    title    = "Figure 4. Evolution of optimal diet composition by household member and city, 2019–2024",
    subtitle = "Daily quantity (grams) of selected foods in the cost-minimising diet",
    caption  = paste("Note: Only foods appearing in at least 5% of city–month observations are shown.",
                     "Stacked areas reflect total diet volume for each household member."),
    x    = NULL,
    y    = "Quantity (g / day)",
    fill = "Food item"
  ) +
  paper_theme +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 8),
        axis.text.x     = element_text(angle = 60, hjust = 1)) +
  guides(fill = guide_legend(nrow = 3))

ggsave(file.path(out_fig, "fig4_comp_member_area.png"),
       fig4, width = 13, height = 9, dpi = 300)

message("Figures 1–4 saved to: ", out_fig)
