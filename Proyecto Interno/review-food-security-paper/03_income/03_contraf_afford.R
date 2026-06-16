########################################################
## 06_figures/bloque4_affordability/figA_counterfactual.R
##
## Figure A (Appendix): Counterfactual unaffordability rate
##   "What if households devoted their entire income to food?"
##
## Boxplot: distribución mensual por año × ciudad
## facet_wrap(~ model, nrow = 3)
## Fill: ciudad | x: año
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
# 1. Load counterfactual
# -----------------------------------------------------------------------
cf <- readRDS(file.path(AFFORD_DIR, "counterfactual_results.rds"))

# -----------------------------------------------------------------------
# 2. Monthly aggregate rate per city × model × month
#    (mean across deciles — equiponderados ~10% each)
#    Each observation = one month → boxplot shows within-year distribution
# -----------------------------------------------------------------------
cf_monthly <- cf %>%
  group_by(ciudad_lbl, model, fecha, year) %>%
  dplyr::summarise(
    rate = mean(rate, na.rm = TRUE),
    .groups = "drop")

# Annual median for annotation below x-axis
cf_median <- cf_monthly %>%
  group_by(ciudad_lbl, model, year) %>%
  dplyr::summarise(
    median_rate = round(median(rate, na.rm = TRUE), 1),
    .groups     = "drop")

# -----------------------------------------------------------------------
# 3. Figure
# -----------------------------------------------------------------------
figA_cf <- ggplot(cf_monthly,
                  aes(x     = factor(year),
                      y     = rate,
                      fill  = ciudad_lbl)) +
  geom_boxplot(
    position      = position_dodge(width = 0.8),
    width         = 0.65,
    alpha         = 0.8,
    outlier.size  = 0.8,
    outlier.alpha = 0.5,
    linewidth     = 0.35) +
  geom_hline(yintercept = 0, color = "grey40",
             linewidth  = 0.3) +
  facet_wrap(~ model, ncol = 3, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Bogotá"   = unname(CITY_COLS["BOGOTA"]),
      "Medellín" = unname(CITY_COLS["MEDELLIN"]),
      "Cali"     = unname(CITY_COLS["CALI"])),
    name = NULL) +
  scale_color_manual(
    values = c(
      "Bogotá"   = unname(CITY_COLS["BOGOTA"]),
      "Medellín" = unname(CITY_COLS["MEDELLIN"]),
      "Cali"     = unname(CITY_COLS["CALI"])),
    guide = "none") +
  scale_y_continuous(
    labels = function(x) ifelse(x < 0, "", sprintf("%.0f%%", x)),
    expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title    = paste0("Figure A. Counterfactual unaffordability rate by city ",
                      "and diet model, 2019\u20132024"),
    subtitle = paste0("Distribution of monthly rates across the year. ",
                      "Numbers below x-axis = annual median per city. ",
                      "Each observation = mean rate across income deciles in one month."),
    caption  = paste0(
      "Note: Counterfactual unaffordability rate (FGT0) = share of households ",
      "whose annual per capita income is below the annual per capita diet cost.\n",
      "Box = IQR; whiskers = 1.5\u00d7IQR; dots = outliers. ",
      "CoCA = Cost of Caloric Adequacy; CoNA = Cost of Nutritional Adequacy; ",
      "CoRD = Cost of Recommended Diet."),
    x = NULL,
    y = "Counterfactual unaffordability rate (%)") +
  paper_theme() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y      = element_text(size = 8),
    axis.title.y     = element_text(size = 9),
    strip.text       = element_text(face = "bold", size = 10),
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.text      = element_text(family = "serif", size = 10),
    legend.key.width = unit(1.2, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin    = margin(3, 8, 3, 8),
    panel.spacing    = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "figA_counterfactual.png"),
       figA_cf, width = 12, height = 8, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "figA_counterfactual.pdf"),
       figA_cf, width = 12, height = 8)

# -----------------------------------------------------------------------
# 4. Print summary for paper text
# -----------------------------------------------------------------------
cat("\n--- Counterfactual median rate by model x city x year ---\n")
cf_median %>%
  pivot_wider(names_from = year, values_from = median_rate) %>%
  arrange(model, ciudad_lbl) %>%
  print(n = 20)

message("Figure A (counterfactual) saved.")