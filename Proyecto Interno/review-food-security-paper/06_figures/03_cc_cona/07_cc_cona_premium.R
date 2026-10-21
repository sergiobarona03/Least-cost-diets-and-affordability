########################################################
## 06_figures/bloque3_ccona/fig07_ccona_premium.R
##
## Figure 7: CC-CoNA premium over CoNA by alpha × city
##   Premium = (CC-CoNA_alpha - CoNA) / CoNA × 100
##   (alpha = 0 is equivalent to CoNA)
##
## Two panels:
##   Top    : monthly premium levels by city
##   Bottom : annual mean premium by alpha
##
## Reported alpha grid: 0.1, 0.25, 0.4, 0.5 (alpha = 0 is the baseline).
## Values above 0.5 force virtually the whole observed expenditure
## structure, at which point the result is closer to the cost of the
## observed basket than to a least-cost diet, so they are estimated but
## not reported.
##
## Reads:  CCONA_DIR/ccona_results.rds
##         PREP_DIR/deflator_monthly.rds
##
## Writes: FIG_DIR/final/fig07_ccona_premium.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)

ALPHAS_KEEP <- c(0.1, 0.25, 0.4, 0.5)

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
ccona <- readRDS(file.path(CCONA_DIR, "ccona_results.rds"))$cost %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# -----------------------------------------------------------------------
# 2. Real per capita cost — all alphas, so the baseline stays available
# -----------------------------------------------------------------------
cost_pc <- ccona %>%
  group_by(alpha_val, ciudad, fecha) %>%
  dplyr::summarise(cost_pc = mean(cost_day, na.rm = TRUE),
                   .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(cost_real = cost_pc * deflator) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real))

# -----------------------------------------------------------------------
# 3. Premium = (CC-CoNA_alpha / CoNA) - 1
#    alpha = 0 is the CoNA baseline
# -----------------------------------------------------------------------
cona_base <- cost_pc %>%
  filter(alpha_val == 0) %>%
  select(ciudad, fecha, cost_base = cost_real)

alpha_levels <- as.character(ALPHAS_KEEP)

premium <- cost_pc %>%
  filter(alpha_val %in% ALPHAS_KEEP) %>%
  left_join(cona_base, by = c("ciudad", "fecha")) %>%
  mutate(
    premium_pct = (cost_real / cost_base - 1) * 100,
    ciudad_lbl  = factor(CITY_LABS[ciudad],
                         levels = c("Bogotá", "Medellín", "Cali")),
    alpha_chr   = factor(as.character(alpha_val), levels = alpha_levels))

# Annual mean premium
premium_annual <- premium %>%
  mutate(year = year(fecha)) %>%
  group_by(alpha_val, alpha_chr, ciudad_lbl, year) %>%
  dplyr::summarise(mean_prem = mean(premium_pct, na.rm = TRUE),
                   .groups   = "drop")

message(sprintf("  alphas plotted: %s", paste(alpha_levels, collapse = ", ")))
message(sprintf("  Premium range: %.2f%% – %.2f%%",
                min(premium$premium_pct, na.rm = TRUE),
                max(premium$premium_pct, na.rm = TRUE)))

# -----------------------------------------------------------------------
# 4. Alpha colour scale
#    ALPHA_COLS / ALPHA_LABS in 00_fig_config.R are keyed on the old grid
#    (0.25, 0.5, 0.75, 1), so they are rebuilt here for the reported one.
# -----------------------------------------------------------------------
ALPHA_PALETTE <- c("0.1"  = "#A9DFBF",
                   "0.25" = "#52BE80",
                   "0.4"  = "#1E8449",
                   "0.5"  = "#0B5A2A")

ALPHA_LABELS <- setNames(sprintf("\u03b1 = %.2f", ALPHAS_KEEP), alpha_levels)

alpha_scale <- scale_color_manual(
  values = ALPHA_PALETTE[alpha_levels],
  labels = ALPHA_LABELS,
  name   = NULL)

base_theme <- paper_theme() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    strip.text   = element_text(face = "bold", size = 10))

# -----------------------------------------------------------------------
# 5. Top row: monthly premium by city
# -----------------------------------------------------------------------
p_top <- ggplot(premium,
                aes(x = fecha, y = premium_pct,
                    color = alpha_chr)) +
  geom_hline(yintercept = 0, color = "grey50",
             linetype = "dotted", linewidth = 0.4) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  facet_wrap(~ ciudad_lbl, nrow = 1, scales = "free_y") +
  alpha_scale +
  date_axis() +
  scale_y_continuous(labels = function(x) sprintf("%+.1f%%", x)) +
  labs(x = NULL,
       y = "Premium over CoNA (%)") +
  base_theme +
  theme(
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 9),
    legend.key.width  = unit(1.4, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8))

# -----------------------------------------------------------------------
# 6. Bottom row: annual mean premium by alpha × city
# -----------------------------------------------------------------------
city_scale <- scale_color_manual(
  values = c(
    "Bogotá"   = unname(CITY_COLS["BOGOTA"]),
    "Medellín" = unname(CITY_COLS["MEDELLIN"]),
    "Cali"     = unname(CITY_COLS["CALI"])),
  name = NULL)

p_bot <- ggplot(premium_annual,
                aes(x = alpha_val, y = mean_prem,
                    color = ciudad_lbl, group = ciudad_lbl)) +
  geom_hline(yintercept = 0, color = "grey50",
             linetype = "dotted", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ year, nrow = 1) +
  city_scale +
  scale_x_continuous(
    breaks = ALPHAS_KEEP,
    labels = sprintf("%.2f", ALPHAS_KEEP)) +
  scale_y_continuous(labels = function(x) sprintf("%+.1f%%", x)) +
  labs(x = expression(alpha),
       y = "Annual mean premium (%)") +
  base_theme +
  theme(
    axis.text.x       = element_text(angle = 0, hjust = 0.5, size = 8),
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 9),
    legend.key.width  = unit(1.2, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8))

# -----------------------------------------------------------------------
# 7. Combine
# -----------------------------------------------------------------------
fig7 <- plot_grid(
  p_top, p_bot,
  ncol        = 1,
  rel_heights = c(1, 1),
  labels      = c("A", "B"),
  label_fontfamily = "serif",
  label_size  = 11)

ggsave(file.path(FIG_DIR, "final", "fig07_ccona_premium.png"),
       fig7, width = 12, height = 9, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig07_ccona_premium.pdf"),
       fig7, width = 12, height = 9)

message("Figure 7 saved.")