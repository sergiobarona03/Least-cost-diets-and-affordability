########################################################
## 05_figures/bloque1_costos/fig01_cost_series.R
##
## Figure 1A: Nominal vs real per capita diet costs
##   Left panels : nominal COP/day
##   Right panels: real COP/day (base Dec 2018)
##   Rows: CoCA | CoNA | CoRD
##   Color: city
##
## Figure 1B: Nominal vs real year-on-year % change
##   Same structure as 1A
##
## Reads:  HCOST_DIR/hcost_full.rds
##         PREP_DIR/deflator_monthly.rds
##
## Writes: FIG_DIR/final/fig01a_cost_levels.png / .pdf
##         FIG_DIR/final/fig01b_cost_yoy.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(scales)
library(lubridate)
library(cowplot)

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
hcost <- readRDS(file.path(HCOST_DIR, "hcost_full.rds")) %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# -----------------------------------------------------------------------
# 2. Build cost series — nominal and real
# -----------------------------------------------------------------------
cost_pc <- hcost %>%
  group_by(model, ciudad, fecha) %>%
  dplyr::summarise(cost_nominal = mean(per_capita, na.rm = TRUE),
                   .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(
    cost_real  = cost_nominal * deflator,
    ciudad_lbl = factor(CITY_LABS[ciudad],
                        levels = c("Bogotá", "Medellín", "Cali")),
    model      = factor(model, levels = c("CoCA", "CoNA", "CoRD"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END,
         !is.na(cost_real))

# Long format with price_type
cost_long <- cost_pc %>%
  pivot_longer(cols      = c(cost_nominal, cost_real),
               names_to  = "price_type",
               values_to = "cost") %>%
  mutate(price_type = recode(price_type,
                             "cost_nominal" = "Nominal",
                             "cost_real"    = "Real (base: Dec 2018)"),
         price_type = factor(price_type,
                             levels = c("Nominal",
                                        "Real (base: Dec 2018)")))

# -----------------------------------------------------------------------
# 3. Year-on-year % change — nominal and real
# -----------------------------------------------------------------------
yoy_long <- cost_pc %>%
  arrange(model, ciudad_lbl, fecha) %>%
  group_by(model, ciudad_lbl) %>%
  mutate(
    yoy_nominal = (cost_nominal / lag(cost_nominal, 12) - 1) * 100,
    yoy_real    = (cost_real    / lag(cost_real,    12) - 1) * 100) %>%
  ungroup() %>%
  filter(!is.na(yoy_nominal)) %>%
  pivot_longer(cols      = c(yoy_nominal, yoy_real),
               names_to  = "price_type",
               values_to = "yoy") %>%
  mutate(price_type = recode(price_type,
                             "yoy_nominal" = "Nominal",
                             "yoy_real"    = "Real (base: Dec 2018)"),
         price_type = factor(price_type,
                             levels = c("Nominal",
                                        "Real (base: Dec 2018)")))

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
    panel.spacing = unit(0.4, "cm"))

legend_theme <- theme(
  legend.position   = "bottom",
  legend.direction  = "horizontal",
  legend.text       = element_text(family = "serif", size = 10),
  legend.key.width  = unit(1.4, "cm"),
  legend.background = element_rect(color = "black", fill = "white",
                                   linewidth = 0.5),
  legend.margin     = margin(3, 8, 3, 8))

# -----------------------------------------------------------------------
# 5. Figure 1A — levels: nominal (left) vs real (right)
##   facet_grid(model ~ price_type), scales = "free_y"
# -----------------------------------------------------------------------
p1a <- ggplot(cost_long,
              aes(x = fecha, y = cost, color = ciudad_lbl)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(price_type ~ model, scales = "free_y",
             nrow = 2) +
  city_scale +
  date_axis() +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(
    title   = " ",
    caption = " ",
    x = NULL,
    y = "COP / day (per capita)") +
  base_theme +
  legend_theme

ggsave(file.path(FIG_DIR, "final", "fig01a_cost_levels.png"),
       p1a, width = 11, height = 8, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig01a_cost_levels.pdf"),
       p1a, width = 11, height = 8)

message("Figure 1A saved.")

# -----------------------------------------------------------------------
# 6. Figure 1B — YoY: nominal (left) vs real (right)
# -----------------------------------------------------------------------
p1b <- ggplot(yoy_long,
              aes(x = fecha, y = yoy, color = ciudad_lbl)) +
  geom_hline(yintercept = 0, color = "grey50",
             linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.85) +
  facet_wrap(price_type ~ model, scales = "free_y",
             nrow = 2) +
  city_scale +
  date_axis() +
  scale_y_continuous(labels = function(x) sprintf("%+.0f%%", x)) +
  labs(
    title   = "",
    caption = "",
    x = NULL,
    y = "Year-on-year change (%)") +
  base_theme +
  legend_theme

ggsave(file.path(FIG_DIR, "final", "fig01b_cost_yoy.png"),
       p1b, width = 11, height = 8, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig01b_cost_yoy.pdf"),
       p1b, width = 11, height = 8)

message("Figure 1B saved.")
