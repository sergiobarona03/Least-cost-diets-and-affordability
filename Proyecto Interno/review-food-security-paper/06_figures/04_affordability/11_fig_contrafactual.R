########################################################
## 06_figures/bloque4_affordability/figA_counterfactual.R
##
## Figure A (Appendix): Counterfactual unaffordability rate, Decile 1
##   "What if households devoted their entire income to food?"
##
## Boxplot: annual distribution of monthly rates, by city, faceted by model
##   Decile 1 only — unaffordability is concentrated there
##
## Reads:  AFFORD_DIR/counterfactual_results.rds
##
## Writes: FIG_DIR/final/figA_counterfactual.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------
# 0. Local city color map — avoids relying on CITY_COLS from other configs
# -----------------------------------------------------------------------
city_col_map <- c(
  "Bogotá"   = unname(CITY_COLORS["Bogotá"]),
  "Medellín" = unname(CITY_COLORS["Medellín"]),
  "Cali"     = unname(CITY_COLORS["Cali"]))

# -----------------------------------------------------------------------
# 1. Load counterfactual — Decile 1 only
# -----------------------------------------------------------------------
cf <- readRDS(file.path(AFFORD_DIR, "counterfactual_results.rds")) %>%
  mutate(
    fecha = as.Date(fecha),
    year  = year(fecha),
    model = factor(model, levels = c("CoCA", "CoNA", "CoRD"))) %>%
  filter(deciles == "Decil 1",
         fecha >= PAPER_START, fecha <= PAPER_END)

message(sprintf("  %d monthly rows | years: %s | cities: %s",
                nrow(cf),
                paste(sort(unique(cf$year)), collapse = ", "),
                paste(unique(cf$ciudad_lbl), collapse = ", ")))

# -----------------------------------------------------------------------
# 2. Figure — boxplot of monthly rate by year, grouped by city
# -----------------------------------------------------------------------
figA_cf <- ggplot(cf,
                  aes(x = factor(year), y = rate, fill = ciudad_lbl)) +
  geom_boxplot(
    position      = position_dodge(width = 0.75),
    width         = 0.65,
    linewidth     = 0.5,
    staplewidth   = 0.8,
    outlier.shape = NA,
    alpha         = 0.85) +
  facet_wrap(~ model, nrow = 1) +
  scale_fill_manual(values = city_col_map, name = NULL) +
  scale_y_continuous(
    labels = function(x) sprintf("%.0f%%", x),
    limits = c(0, NA)) +
  labs(
    title    = " ",
    subtitle = " ",
    x = NULL,
    y = "Unaffordability rate, Decile 1 (%)") +
  paper_theme() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y       = element_text(size = 8),
    strip.text        = element_text(face = "bold", size = 10),
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 10),
    legend.key.width  = unit(1.4, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8),
    panel.spacing     = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "figA_counterfactual.png"),
       figA_cf, width = 13, height = 5, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "figA_counterfactual.pdf"),
       figA_cf, width = 13, height = 5)

# -----------------------------------------------------------------------
# 3. Print summary — Decile 1, full period, by model x city
# -----------------------------------------------------------------------
cat("\n--- Counterfactual rate, Decile 1, full period, by model x city ---\n")
cf %>%
  group_by(model, ciudad_lbl) %>%
  dplyr::summarise(
    mean_rate = round(mean(rate, na.rm = TRUE), 1),
    sd_rate   = round(sd(rate, na.rm = TRUE), 1),
    .groups   = "drop") %>%
  arrange(model, ciudad_lbl) %>%
  print(n = 20)

message("Figure A (counterfactual boxplot, Decile 1) saved.")