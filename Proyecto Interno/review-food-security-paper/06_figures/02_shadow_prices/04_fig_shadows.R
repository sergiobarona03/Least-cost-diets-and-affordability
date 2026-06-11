########################################################
## 05_figures/bloque2_shadow_prices/fig04_shadow_prices.R
##
## Figure 4: Mean SPE by nutrient — dot plot
##   Top-8 most binding nutrients (overall binding freq)
##   Period mean ± 1 SD (2019–2024)
##   Facet: household member (columns)
##   Color: city
##
## Reads:  CONA_DIR/cona_results.rds
##
## Writes: FIG_DIR/final/fig04_shadow_prices.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

TOP_N <- 10

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
cona <- readRDS(file.path(CONA_DIR, "cona_results.rds"))

spe <- cona$spe %>%
  mutate(
    fecha      = as.Date(fecha),
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
# 3. Period mean ± SD of SPE per nutrient × member × city
# -----------------------------------------------------------------------
spe_summary <- spe %>%
  filter(Nutrients %in% top_nutrients) %>%
  group_by(Nutrients, member, ciudad_lbl) %>%
  dplyr::summarise(
    mean_SPE = mean(SPE, na.rm = TRUE),
    sd_SPE   = sd(SPE,   na.rm = TRUE),
    .groups  = "drop") %>%
  mutate(
    lo  = pmax(mean_SPE - sd_SPE, 0),
    hi  = mean_SPE + sd_SPE,
    # Order nutrients by descending overall mean SPE
    Nutrients = factor(Nutrients,
                       levels = rev(top_nutrients)))

# -----------------------------------------------------------------------
# 4. Dot plot
# -----------------------------------------------------------------------
city_scale_fill <- scale_color_manual(
  values = c(
    "Bogotá"   = unname(CITY_COLS["BOGOTA"]),
    "Medellín" = unname(CITY_COLS["MEDELLIN"]),
    "Cali"     = unname(CITY_COLS["CALI"])),
  name = NULL)

fig4 <- ggplot(spe_summary,
               aes(x = mean_SPE, y = Nutrients,
                   color = ciudad_lbl)) +
  geom_linerange(aes(xmin = lo, xmax = hi),
                 position = position_dodge(width = 0.6),
                 linewidth = 0.7, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.6),
             size = 1.2, alpha = 0.95) +
  geom_vline(xintercept = 0, color = "grey50",
             linetype = "dotted", linewidth = 0.4) +
  facet_wrap(~ member, nrow = 1, scales = "free_x") +
  city_scale_fill +
  scale_x_continuous(labels = number_format(accuracy = 0.001)) +
  labs(
    title    = paste0("Figure 4. Shadow price elasticity (SPE) of binding ",
                      "nutritional constraints, 2019\u20132024"),
    subtitle = paste0("Period mean \u00b1 1 SD. Top ", TOP_N,
                      " nutrients by overall binding frequency."),
    caption  = paste0(
      "Note: SPE = shadow price as share of total daily diet cost. ",
      "A higher SPE implies a larger cost increase if the requirement ",
      "were tightened by one unit.\n",
      "Error bars: \u00b11 standard deviation across monthly observations. ",
      "Nutrients ordered by descending mean SPE."),
    x = "Mean SPE (share of daily diet cost)",
    y = NULL) +
  paper_theme() +
  theme(
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 10),
    legend.key.width  = unit(1.2, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8),
    axis.text.y       = element_text(size = 9),
    axis.text.x       = element_text(size = 8),
    strip.text        = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey92",
                                      linewidth = 0.3))

ggsave(file.path(FIG_DIR, "final", "fig04_shadow_prices.png"),
       fig4, width = 13, height = 6, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig04_shadow_prices.pdf"),
       fig4, width = 13, height = 6)

message("Figure 4 saved.")