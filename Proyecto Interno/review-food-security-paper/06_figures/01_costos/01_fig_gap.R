########################################################
## 05_figures/bloque1_costos/fig01c_city_gap.R
##
## Figure 1C: Evolution of pairwise intercity cost gaps (real)
##   Three city pairs × three models
##   facet_grid(pair ~ model)
##   Line + OLS trend with 95% CI
##   Annotation: beta and p-value per panel
##
## Reads:  HCOST_DIR/hcost_full.rds
##         PREP_DIR/deflator_monthly.rds
##
## Writes: FIG_DIR/final/fig01c_city_gap.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------
hcost <- readRDS(file.path(HCOST_DIR, "hcost_full.rds")) %>%
  mutate(fecha = as.Date(fecha))

deflator <- readRDS(file.path(PREP_DIR, "deflator_monthly.rds")) %>%
  filter(ciudad != "NACIONAL") %>%
  select(ciudad, fecha, deflator)

cost_pc <- hcost %>%
  group_by(model, ciudad, fecha) %>%
  dplyr::summarise(cost_nom = mean(per_capita, na.rm = TRUE),
                   .groups  = "drop") %>%
  left_join(deflator, by = c("ciudad", "fecha")) %>%
  mutate(cost_real = cost_nom * deflator,
         model     = factor(model, levels = c("CoCA","CoNA","CoRD"))) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

# -----------------------------------------------------------------------
# 2. Pairwise gaps — all three city pairs
# -----------------------------------------------------------------------
cost_wide <- cost_pc %>%
  select(model, ciudad, fecha, cost_real) %>%
  pivot_wider(names_from = ciudad, values_from = cost_real)

gap_pairs <- cost_wide %>%
  mutate(
    BOG_CAL = abs(BOGOTA - CALI),
    BOG_MED = abs(BOGOTA - MEDELLIN),
    CAL_MED = abs(CALI   - MEDELLIN)) %>%
  select(model, fecha, BOG_CAL, BOG_MED, CAL_MED) %>%
  pivot_longer(cols      = c(BOG_CAL, BOG_MED, CAL_MED),
               names_to  = "pair_code",
               values_to = "gap_real") %>%
  mutate(
    pair = recode(pair_code,
                  "BOG_CAL" = "Bogota - Cali",
                  "BOG_MED" = "Bogota - Medellin",
                  "CAL_MED" = "Cali - Medellin"),
    pair = factor(pair, levels = c("Bogota - Cali",
                                   "Bogota - Medellin",
                                   "Cali - Medellin")),
    t    = as.numeric(fecha - min(fecha)) / 365.25)

# -----------------------------------------------------------------------
# 3. OLS trend per model × pair — for annotations
# -----------------------------------------------------------------------
trend_stats <- gap_pairs %>%
  group_by(model, pair) %>%
  dplyr::summarise(
    beta = coef(lm(gap_real ~ t))[["t"]],
    pval = summary(lm(gap_real ~ t))$coefficients["t","Pr(>|t|)"],
    .groups = "drop") %>%
  mutate(
    pval_lbl = ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval)),
    label    = sprintf("b=%.0f; p=%s", beta, pval_lbl))

# -----------------------------------------------------------------------
# 4. Figure
# -----------------------------------------------------------------------
fig1c <- ggplot(gap_pairs,
                aes(x = fecha, y = gap_real)) +
  geom_line(color = "grey55", linewidth = 0.65, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE,
              color     = "#C0392B",
              fill      = "#C0392B",
              linewidth = 0.85,
              alpha     = 0.15) +
  facet_grid(pair ~ model, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.02, 0)) +
  scale_y_continuous(labels = comma_format(big.mark = ",")) +
  labs(
    title    = " ",
    caption  = " ",
    x = NULL,
    y = "Absolute gap (real COP/day)") +
  paper_theme() +
  theme(
    strip.text.x    = element_text(face = "bold", size = 10),
    strip.text.y    = element_text(face = "bold", size = 9, angle = 0),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y     = element_text(size = 8),
    panel.spacing   = unit(0.3, "cm"),
    legend.position = "none")

ggsave(file.path(FIG_DIR, "final", "fig01c_city_gap.png"),
       fig1c, width = 12, height = 8, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig01c_city_gap.pdf"),
       fig1c, width = 12, height = 8)

message("Figure 1C saved.")
