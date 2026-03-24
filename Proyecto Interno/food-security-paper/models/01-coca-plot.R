########################################################
## Cost of Caloric Adequacy (CoCA) — Results
## Figures for Q1 submission
########################################################

library(tidyverse)
library(scales)
library(patchwork)
library(lubridate)

##----------------------------------------------------------
## Directories and data
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"
out_coca <- file.path(base_dir, "food-security-paper", "output", "coca")
out_fig  <- file.path(base_dir, "food-security-paper", "output", "figures")

df.coca <- readxl::read_excel(file.path(out_coca, "230326_coca_results.xlsx")) %>%
  mutate(
    fecha  = as.Date(fecha),
    year   = year(fecha),
    member = case_when(
      Sex == 0 & Demo_Group == "[31,51)"  ~ "Adult male",
      Sex == 1 & Demo_Group == "[31,51)"  ~ "Adult female",
      Sex == 1 & Demo_Group == "[10, 14)" ~ "Female child"
    )
  )

##----------------------------------------------------------
## Color palette
##----------------------------------------------------------

city_colors <- c(
  "BOGOTA"   = "#2C3E6B",
  "MEDELLIN" = "#C0392B",
  "CALI"     = "#27AE60"
)

member_colors <- c(
  "Adult male"   = "#2C3E6B",
  "Adult female" = "#C0392B",
  "Female child" = "#27AE60"
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
    strip.text       = element_text(face = "bold")
  )

##----------------------------------------------------------
## Aggregation: per capita cost by city × date
##----------------------------------------------------------

coca_pc <- df.coca %>%
  group_by(ciudad, fecha, year) %>%
  summarize(
    cost_percap    = mean(cost_day, na.rm = TRUE),
    cost_household = sum(cost_day,  na.rm = TRUE),
    .groups = "drop"
  )

##----------------------------------------------------------
## Figure 1: Per capita CoCA — time series by city
##----------------------------------------------------------

fig1 <- ggplot(coca_pc, aes(x = fecha, y = cost_percap, color = ciudad)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = city_colors, labels = c("Bogotá", "Cali", "Medellín")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(prefix = "$", suffix = " COP")) +
  labs(
    title    = "Figure 1. Daily per capita Cost of Caloric Adequacy (CoCA), 2019–2024",
    subtitle = "Daily cost per household member (Colombian pesos, COP)",
    caption  = "Note: Dashed lines represent LOESS trend. CoCA estimated for a representative\nhousehold comprising one adult male, one adult female, and one female child.",
    x        = NULL,
    y        = "COP / day"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig1_coca_percapita_series.png"),
       fig1, width = 8, height = 4.5, dpi = 300)

##----------------------------------------------------------
## Figure 2: CoCA by household member — panel by city
##----------------------------------------------------------

coca_member <- df.coca %>%
  group_by(ciudad, fecha, member) %>%
  summarize(cost = sum(cost_day, na.rm = TRUE), .groups = "drop")

city_labels <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

fig2 <- ggplot(coca_member, aes(x = fecha, y = cost, color = member)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ ciudad, ncol = 3, labeller = as_labeller(city_labels)) +
  scale_color_manual(values = member_colors) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma_format(prefix = "$")) +
  labs(
    title    = "Figure 2. Daily CoCA by household member and city, 2019–2024",
    subtitle = "Daily cost per member (Colombian pesos, COP)",
    caption  = "Note: Estimated caloric requirements follow GABAS dietary guidelines.",
    x        = NULL,
    y        = "COP / day"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig2_coca_by_member.png"),
       fig2, width = 10, height = 4, dpi = 300)

##----------------------------------------------------------
## Figure 3: Cross-city comparison — boxplot by year
##----------------------------------------------------------

fig3 <- ggplot(coca_pc, aes(x = factor(year), y = cost_percap, fill = ciudad)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1, position = position_dodge(0.8)) +
  scale_fill_manual(values = city_colors, labels = c("Bogotá", "Cali", "Medellín")) +
  scale_y_continuous(labels = comma_format(prefix = "$")) +
  labs(
    title    = "Figure 3. Annual distribution of per capita CoCA by city, 2019–2024",
    subtitle = "Monthly variation within each year (Colombian pesos, COP)",
    caption  = "Note: Boxes represent interquartile range; whiskers extend to 1.5 × IQR.",
    x        = NULL,
    y        = "COP / day"
  ) +
  paper_theme

ggsave(file.path(out_fig, "fig3_coca_boxplot_cities.png"),
       fig3, width = 9, height = 4.5, dpi = 300)

