########################################################
## 06_figures/bloque4_affordability/figA_counterfactual.R
##
## Figure A (Appendix): Counterfactual unaffordability rate
##   "What if households devoted their entire income to food?"
##
## Single panel: aggregate monthly rate by city × model (line plot)
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
# 2. Aggregate monthly rate by city × model
# -----------------------------------------------------------------------
cf_agg <- cf %>%
  group_by(ciudad_lbl, model, fecha) %>%
  dplyr::summarise(rate = mean(rate, na.rm = TRUE),
                   .groups = "drop")

# -----------------------------------------------------------------------
# 3. Shared aesthetics
# -----------------------------------------------------------------------
city_scale <- scale_color_manual(
  values = c(
    "Bogotá"   = unname(CITY_COLS["BOGOTA"]),
    "Medellín" = unname(CITY_COLS["MEDELLIN"]),
    "Cali"     = unname(CITY_COLS["CALI"])),
  name = NULL)

# -----------------------------------------------------------------------
# 4. Figure
# -----------------------------------------------------------------------
figA_cf <- ggplot(cf_agg,
                  aes(x = fecha, y = rate, color = ciudad_lbl)) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ model, nrow = 3, scales = "free_y") +
  city_scale +
  date_axis() +
  scale_y_continuous(
    labels = function(x) sprintf("%.0f%%", x),
    limits = c(0, NA)) +
  labs(
    title   = paste0("Figure A. Counterfactual unaffordability rate by city ",
                     "and diet model, 2019\u20132024"),
    subtitle = paste0("Share of households unable to afford each diet even if ",
                      "their entire per capita income were devoted to food."),
    caption = paste0(
      "Note: Counterfactual unaffordability rate (FGT0) = share of households ",
      "whose annual per capita income is below the annual per capita diet cost.\n",
      "Averaged across all income deciles (unweighted mean). ",
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
    legend.key.width = unit(1.4, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin    = margin(3, 8, 3, 8))

ggsave(file.path(FIG_DIR, "final", "figA_counterfactual.png"),
       figA_cf, width = 8, height = 12, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "figA_counterfactual.pdf"),
       figA_cf, width = 8, height = 12)

# -----------------------------------------------------------------------
# 5. Print summary
# -----------------------------------------------------------------------
cat("\n--- Counterfactual aggregate rate by model x city x year ---\n")
cf %>%
  group_by(model, ciudad_lbl, year) %>%
  dplyr::summarise(rate = round(mean(rate, na.rm=TRUE), 1),
                   .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = rate) %>%
  arrange(model, ciudad_lbl) %>%
  print(n = 20)

message("Figure A (counterfactual) saved.")