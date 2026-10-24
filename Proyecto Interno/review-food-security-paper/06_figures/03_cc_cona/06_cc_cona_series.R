########################################################
## 06_figures/bloque3_ccona/fig06_ccona_series.R
##
## Figure 6: CC-CoNA cost and premium over CoNA
##   Top row   : real COP/day by alpha × city
##   Bottom row: premium (CC-CoNA_alpha / CoNA - 1) × 100
##   Facet: city | Color + linetype: alpha (excl. 0 in bottom)
##
## Reported alpha grid: 0, 0.1, 0.25, 0.4, 0.5.
## Values above 0.5 force virtually the whole observed expenditure
## structure, at which point the result is closer to the cost of the
## observed basket than to a least-cost diet, so they are estimated but
## not reported.
##
## Reads:  CCONA_DIR/ccona_results.rds
##         PREP_DIR/deflator_monthly.rds
##
## Writes: FIG_DIR/final/fig06_ccona_series.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)

# Alphas reported in the paper
ALPHAS_KEEP <- c(0, 0.1, 0.25, 0.4, 0.5)

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
ccona <- readRDS(file.path(CCONA_DIR, "ccona_results.rds"))$cost %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

# -----------------------------------------------------------------------
# 2. Real per capita cost by alpha × city × month
#    The premium is built from the FULL alpha set and filtered afterwards,
#    so that the alpha = 0 baseline is always available.
# -----------------------------------------------------------------------
cost_pc_all <- ccona %>%
  group_by(alpha_val, ciudad, fecha) %>%
  dplyr::summarise(cost_pc = mean(cost_day, na.rm = TRUE),
                   .groups = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(
    cost_real  = cost_pc * deflator,
    ciudad_lbl = factor(CITY_LABS[ciudad],
                        levels = c("Bogotá", "Medellín", "Cali"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END, !is.na(cost_real))

# -----------------------------------------------------------------------
# 3. Premium over CoNA (alpha = 0 baseline)
# -----------------------------------------------------------------------
cona_base <- cost_pc_all %>%
  filter(alpha_val == 0) %>%
  select(ciudad, ciudad_lbl, fecha, cost_base = cost_real)

premium_all <- cost_pc_all %>%
  filter(alpha_val > 0) %>%
  left_join(cona_base, by = c("ciudad", "ciudad_lbl", "fecha")) %>%
  mutate(premium_pct = (cost_real / cost_base - 1) * 100)

# -----------------------------------------------------------------------
# 4. Keep the reported alphas and build the factor levels from them
# -----------------------------------------------------------------------
alpha_levels  <- as.character(ALPHAS_KEEP)
alpha_levels2 <- as.character(setdiff(ALPHAS_KEEP, 0))

cost_pc <- cost_pc_all %>%
  filter(alpha_val %in% ALPHAS_KEEP) %>%
  mutate(alpha_chr = factor(as.character(alpha_val), levels = alpha_levels))

premium <- premium_all %>%
  filter(alpha_val %in% ALPHAS_KEEP) %>%
  mutate(alpha_chr = factor(as.character(alpha_val), levels = alpha_levels2))

message(sprintf("  alphas plotted: %s",
                paste(alpha_levels, collapse = ", ")))
message(sprintf("  premium range: %.2f%% – %.2f%%",
                min(premium$premium_pct, na.rm = TRUE),
                max(premium$premium_pct, na.rm = TRUE)))

# -----------------------------------------------------------------------
# 5. Shared aesthetics
#    ALPHA_COLS / ALPHA_LABS in 00_fig_config.R are keyed on the old
#    grid (0, 0.25, 0.5, 0.75, 1), so colours and labels are built here
#    for the reported grid instead of looked up there.
# -----------------------------------------------------------------------
ALPHA_PALETTE <- c("0"    = "#AAAAAA",
                   "0.1"  = "#A9DFBF",
                   "0.25" = "#52BE80",
                   "0.4"  = "#1E8449",
                   "0.5"  = "#0B5A2A")

ALPHA_LTYPE_MAP <- c("0"    = "solid",
                     "0.1"  = "dashed",
                     "0.25" = "longdash",
                     "0.4"  = "dotdash",
                     "0.5"  = "dotted")

alpha_label <- function(a) {
  ifelse(a == "0",
         "\u03b1 = 0 (CoNA)",
         sprintf("\u03b1 = %.2f", as.numeric(a)))
}

ALPHA_COLORS <- ALPHA_PALETTE[alpha_levels]
ALPHA_LTYPES <- ALPHA_LTYPE_MAP[alpha_levels]
ALPHA_LABELS <- setNames(alpha_label(alpha_levels), alpha_levels)

ALPHA_COLORS2 <- ALPHA_PALETTE[alpha_levels2]
ALPHA_LTYPES2 <- ALPHA_LTYPE_MAP[alpha_levels2]
ALPHA_LABELS2 <- setNames(alpha_label(alpha_levels2), alpha_levels2)

base_theme <- paper_theme() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    strip.text   = element_text(face = "bold", size = 10),
    legend.position = "none")

# -----------------------------------------------------------------------
# 6. Top row: levels (with legend)
# -----------------------------------------------------------------------
p_top <- ggplot(cost_pc,
                aes(x = fecha, y = cost_real,
                    color = alpha_chr, linetype = alpha_chr)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ ciudad_lbl, nrow = 1, scales = "free_y") +
  scale_color_manual(values = ALPHA_COLORS,
                     labels = ALPHA_LABELS, name = NULL) +
  scale_linetype_manual(values = ALPHA_LTYPES,
                        labels = ALPHA_LABELS, name = NULL) +
  date_axis() +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(x = NULL, y = "Real COP / day (per capita)") +
  base_theme +
  theme(
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 9),
    legend.key.width  = unit(1.6, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8)) +
  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1))

# -----------------------------------------------------------------------
# 7. Bottom row: premium (no legend)
# -----------------------------------------------------------------------
p_bot <- ggplot(premium,
                aes(x = fecha, y = premium_pct,
                    color = alpha_chr, linetype = alpha_chr)) +
  geom_hline(yintercept = 0, color = "grey50",
             linetype = "dotted", linewidth = 0.4) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ ciudad_lbl, nrow = 1, scales = "free_y") +
  scale_color_manual(values = ALPHA_COLORS2,
                     labels = ALPHA_LABELS2, name = NULL) +
  scale_linetype_manual(values = ALPHA_LTYPES2,
                        labels = ALPHA_LABELS2, name = NULL) +
  date_axis() +
  scale_y_continuous(labels = function(x) sprintf("%+.1f%%", x)) +
  labs(x = NULL, y = "Premium over CoNA (%)") +
  base_theme

# -----------------------------------------------------------------------
# 8. Combine with cowplot
# -----------------------------------------------------------------------
fig6 <- plot_grid(
  p_top, p_bot,
  ncol        = 1,
  rel_heights = c(1.15, 1),
  labels      = c("A", "B"),
  label_fontfamily = "serif",
  label_size  = 11)

ggsave(file.path(FIG_DIR, "final", "fig06_ccona_series.png"),
       fig6, width = 12, height = 8, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig06_ccona_series.pdf"),
       fig6, width = 12, height = 8)

message("Figure 6 saved.")