########################################################
## 05_figures/bloque2_shadow_prices/fig05_shadow_evolution.R
##
## Figure 5: Heatmap of annual mean SPE
##   X = year | Y = nutrient | Fill = mean SPE
##   Facet: city (columns) × member (rows)
##   Top-N most binding nutrients
##
## Reads:  CONA_DIR/cona_results.rds
##
## Writes: FIG_DIR/final/fig05_shadow_evolution.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

TOP_N <- 20

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
cona <- readRDS(file.path(CONA_DIR, "cona_results.rds"))

spe <- cona$spe %>%
  mutate(
    fecha      = as.Date(fecha),
    year       = year(fecha),
    member     = recode(paste0(Sex, "_", Age), !!!MEMBER_LABS),
    member     = factor(member, levels = MEMBER_ORDER),
    ciudad_lbl = CITY_LABS[ciudad],
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali"))) %>%
  filter(constraint == "Min",
         fecha >= PAPER_START, fecha <= PAPER_END)

limit <- cona$limit %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 2. Top-N nutrients by overall binding frequency
# -----------------------------------------------------------------------
top_nutrients <- limit %>%
  group_by(Nutrients) %>%
  dplyr::summarise(freq = mean(Limiting == 1, na.rm = TRUE),
                   .groups = "drop") %>%
  arrange(desc(freq)) %>%
  slice_head(n = TOP_N) %>%
  pull(Nutrients)

message(sprintf("  Top %d: %s",
                TOP_N, paste(top_nutrients, collapse = ", ")))

# -----------------------------------------------------------------------
# 3. Annual mean SPE
# -----------------------------------------------------------------------
spe_annual <- spe %>%
  filter(Nutrients %in% top_nutrients) %>%
  group_by(Nutrients, member, ciudad_lbl, year) %>%
  dplyr::summarise(mean_SPE = mean(SPE, na.rm = TRUE),
                   .groups  = "drop") %>%
  mutate(
    Nutrients = factor(Nutrients, levels = rev(top_nutrients)))

# -----------------------------------------------------------------------
# 4. Discretise SPE into labelled intervals
# -----------------------------------------------------------------------
spe_annual <- spe_annual %>%
  mutate(SPE_cat = cut(mean_SPE,
                       breaks         = c(-Inf, 0, 0.02, 0.10, 0.40, Inf),
                       labels         = c("0",
                                          "(0, 0.02]",
                                          "(0.02, 0.10]",
                                          "(0.10, 0.40]",
                                          "(0.40, ∞)"),
                       include.lowest = TRUE,
                       right          = TRUE))

# -----------------------------------------------------------------------
# 5. Heatmap
# -----------------------------------------------------------------------
fig5 <- ggplot(spe_annual,
               aes(x = factor(year), y = Nutrients,
                   fill = SPE_cat)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_grid(member ~ ciudad_lbl) +
  scale_fill_manual(
    values = c(
      "0"              = "#FFFDE7",
      "(0, 0.02]"      = "#FFF176",
      "(0.02, 0.10]"   = "#FF9800",
      "(0.10, 0.40]"   = "#E53935",
      "(0.40, ∞)" = "#7B1FA2"),
    name   = "Mean SPE",
    drop   = FALSE) +
  labs(
    title    = " ",
    subtitle = " ",
    caption  = paste0(
      "Note: SPE = shadow price as share of total daily diet cost. ",
      "Top ", TOP_N, " nutrients selected by overall binding frequency.\n",
      "Grey cells indicate SPE \u2248 0 (non-binding or negligible constraint)."),
    x = NULL,
    y = NULL) +
  paper_theme() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y      = element_text(size = 9),
    strip.text.x     = element_text(face = "bold", size = 10),
    strip.text.y     = element_text(face = "bold", size = 9, angle = 0),
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(1.2, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8),
    panel.grid       = element_blank(),
    panel.spacing.x  = unit(0.3, "cm"),
    panel.spacing.y  = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "fig05_shadow_evolution.png"),
       fig5, width = 13, height = 9, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig05_shadow_evolution.pdf"),
       fig5, width = 13, height = 9)

message("Figure 5 saved.")