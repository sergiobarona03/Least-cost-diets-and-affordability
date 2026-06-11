########################################################
## 05_figures/bloque4_affordability/fig10_afford_gap.R
##
## Figure 10: Aggregate monthly unaffordability rate
##   by city and diet model, 2019–2024
##
## Rate aggregated across all deciles (weighted mean)
## Layout: top = levels | bottom = YoY change
## Facet: model (cols) | Color: city
##
## Reads:  AFFORD_DIR/afford_results.xlsx
##
## Writes: FIG_DIR/final/fig10_afford_gap.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
# -----------------------------------------------------------------------
# 1. Load and clean
# -----------------------------------------------------------------------
afford <- read_excel(file.path(AFFORD_DIR, "afford_results.xlsx")) %>%
  mutate(
    fecha      = as.Date(fecha),
    ciudad_lbl = case_when(
      grepl("BOGOT", ciudad, ignore.case = TRUE) ~ "Bogotá",
      grepl("MEDEL", ciudad, ignore.case = TRUE) ~ "Medellín",
      grepl("CALI",  ciudad, ignore.case = TRUE) ~ "Cali",
      TRUE ~ ciudad),
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali")),
    model      = factor(model, levels = c("CoCA", "CoNA", "CoRD"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 2. Aggregate across deciles: mean rate per city × model × month
# -----------------------------------------------------------------------
afford_agg <- afford %>%
  group_by(ciudad_lbl, model, fecha) %>%
  dplyr::summarise(rate = mean(rate, na.rm = TRUE),
                   .groups = "drop")

message(sprintf("  Aggregate: %d rows", nrow(afford_agg)))

# -----------------------------------------------------------------------
# 4. Shared aesthetics
# -----------------------------------------------------------------------
city_scale <- scale_color_manual(
  values = c(
    "Bogotá"   = unname(CITY_COLS["BOGOTA"]),
    "Medellín" = unname(CITY_COLS["MEDELLIN"]),
    "Cali"     = unname(CITY_COLS["CALI"])),
  name = NULL)

base_theme <- paper_theme() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    strip.text   = element_text(face = "bold", size = 10),
    legend.position = "none")

# -----------------------------------------------------------------------
# 5. Figure
# -----------------------------------------------------------------------
fig10 <- ggplot(afford_agg,
                aes(x = fecha, y = rate, color = ciudad_lbl)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 3, scales = "free_y") +
  city_scale +
  date_axis() +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x),
                     limits = c(0, NA)) +
  labs(
    title   = paste0("Figure 10. Aggregate monthly unaffordability rate ",
                     "by city and diet model, 2019\u20132024"),
    caption = paste0(
      "Note: Unaffordability rate = share of households whose per capita ",
      "income is below the daily diet cost, averaged across all income deciles.\n",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of Recommended Diet."),
    x = NULL,
    y = "Unaffordability rate (%)") +
  base_theme +
  theme(
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 10),
    legend.key.width  = unit(1.4, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8))

ggsave(file.path(FIG_DIR, "final", "fig10_afford_gap.png"),
       fig10, width = 8, height = 12, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig10_afford_gap.pdf"),
       fig10, width = 8, height = 12)

message("Figure 10 saved.")