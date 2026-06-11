########################################################
## 05_figures/bloque1_costos/fig01_cost_series.R
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
# 2. Real per capita cost by model × city × month
# -----------------------------------------------------------------------
cost_pc <- hcost %>%
  group_by(model, ciudad, fecha) %>%
  dplyr::summarise(cost_pc = mean(per_capita, na.rm = TRUE), .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(
    cost_real  = cost_pc * deflator,
    ciudad_lbl = factor(CITY_LABS[ciudad],
                        levels = c("Bogotá", "Medellín", "Cali")),
    model      = factor(model, levels = c("CoCA", "CoNA", "CoRD"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real))

# -----------------------------------------------------------------------
# 3. Year-on-year % change
# -----------------------------------------------------------------------
cost_yoy <- cost_pc %>%
  arrange(model, ciudad_lbl, fecha) %>%
  group_by(model, ciudad_lbl) %>%
  mutate(cost_yoy = (cost_real / lag(cost_real, 12) - 1) * 100) %>%
  ungroup() %>%
  filter(!is.na(cost_yoy))

# -----------------------------------------------------------------------
# 4. City colour scale (labels match ciudad_lbl levels)
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
    strip.text   = element_text(face = "bold", size = 10))

# -----------------------------------------------------------------------
# 5. Top row: levels — leyenda arriba con borde negro
# -----------------------------------------------------------------------
p_top <- ggplot(cost_pc,
                aes(x = fecha, y = cost_real,
                    color = ciudad_lbl)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  city_scale +
  date_axis() +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(x = NULL, y = "Real COP / day (per capita)") +
  base_theme +
  theme(
    legend.position      = "top",
    legend.direction     = "horizontal",
    legend.text          = element_text(family = "serif", size = 10),
    legend.key.width     = unit(1.4, "cm"),
    legend.background    = element_rect(color = "black",
                                        fill  = "white",
                                        linewidth = 0.5),
    legend.margin        = margin(3, 8, 3, 8))

# -----------------------------------------------------------------------
# 6. Bottom row: YoY change — sin leyenda
# -----------------------------------------------------------------------
p_bot <- ggplot(cost_yoy,
                aes(x = fecha, y = cost_yoy,
                    color = ciudad_lbl)) +
  geom_hline(yintercept = 0, color = "grey50",
             linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  city_scale +
  date_axis() +
  scale_y_continuous(labels = function(x) sprintf("%+.0f%%", x)) +
  labs(x = NULL, y = "Year-on-year change (%)") +
  base_theme +
  theme(legend.position = "none")

# -----------------------------------------------------------------------
# 7. Combine with cowplot
# -----------------------------------------------------------------------
fig1 <- plot_grid(
  p_top,
  p_bot,
  ncol        = 1,
  rel_heights = c(1.1, 1),
  labels      = c("A", "B"),
  label_fontfamily = "serif",
  label_size  = 11)

ggsave(file.path(FIG_DIR, "final", "fig01_cost_series.png"),
       fig1, width = 12, height = 8, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig01_cost_series.pdf"),
       fig1, width = 12, height = 8)

message("Figure 1 saved.")