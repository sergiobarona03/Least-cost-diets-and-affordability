########################################################
## 05_figures/bloque1_costos/fig02_quality_premium.R
##
## Figure 2: Nutritional quality premiums
##   CoNA / CoCA | CoRD / CoCA | CoRD / CoNA
##   One panel per premium, color by city
##
## Reads:  HCOST_DIR/hcost_full.rds
##         PREP_DIR/deflator_monthly.rds
##
## Writes: FIG_DIR/final/fig02_quality_premium.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(scales)
library(lubridate)

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
hcost <- readRDS(file.path(HCOST_DIR, "hcost_full.rds")) %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# -----------------------------------------------------------------------
# 2. Real per capita cost — wide then premiums
# -----------------------------------------------------------------------
PREM_LEVELS <- c("CoNA / CoCA", "CoRD / CoCA", "CoRD / CoNA")

premium_long <- hcost %>%
  group_by(model, ciudad, fecha) %>%
  dplyr::summarise(cost_pc = mean(per_capita, na.rm = TRUE), .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(cost_real = cost_pc * deflator) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real)) %>%
  select(ciudad, fecha, model, cost_real) %>%
  pivot_wider(names_from = model, values_from = cost_real) %>%
  mutate(
    `CoNA / CoCA` = CoNA / CoCA,
    `CoRD / CoCA` = CoRD / CoCA,
    `CoRD / CoNA` = CoRD / CoNA,
    ciudad_lbl    = factor(CITY_LABS[ciudad],
                           levels = c("Bogotá", "Medellín", "Cali"))) %>%
  pivot_longer(cols      = all_of(PREM_LEVELS),
               names_to  = "premium",
               values_to = "ratio") %>%
  mutate(premium = factor(premium, levels = PREM_LEVELS))

# -----------------------------------------------------------------------
# 3. Figure
# -----------------------------------------------------------------------
fig2 <- ggplot(premium_long,
               aes(x = fecha, y = ratio, color = ciudad_lbl)) +
  geom_hline(yintercept = 1, color = "grey60",
             linetype = "dotted", linewidth = 0.4) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ premium, nrow = 1, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Bogotá"   = unname(CITY_COLS["BOGOTA"]),
      "Medellín" = unname(CITY_COLS["MEDELLIN"]),
      "Cali"     = unname(CITY_COLS["CALI"])),
    name = NULL) +
  date_axis() +
  scale_y_continuous(labels = function(x) sprintf("%.2f\u00d7", x)) +
  labs(
    title   = " ",
    caption = " ",
    x = NULL,
    y = "Cost ratio") +
  paper_theme() +
  theme(
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 10),
    legend.key.width  = unit(1.4, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8),
    strip.text        = element_text(face = "bold", size = 10))

ggsave(file.path(FIG_DIR, "final", "fig02_quality_premium.png"),
       fig2, width = 12, height = 5, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig02_quality_premium.pdf"),
       fig2, width = 12, height = 5)

message("Figure 2 saved.")