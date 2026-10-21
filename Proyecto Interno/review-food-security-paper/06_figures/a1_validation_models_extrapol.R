########################################################
## 06_figures/appendix/figA_model_validation.R
##
## Figure A (Appendix): validation of the diet metrics
##   Mean absolute deviation (MAD) and mean signed deviation (MD) of the
##   per capita daily cost computed on extrapolated prices relative to
##   the same cost computed on observed prices, over the validation
##   window (Feb 2016 - Mar 2018).
##
##   MAD measures how much price error survives into the metric.
##   MD measures the direction of the error: a negative value means the
##   extrapolated panel understates the cost obtained on observed prices.
##
## Facet: measure | Fill: city | x: metric (CoCA, CoNA, CoRD)
##
## Reads:  VALID_DIR/model_validation_cost.csv   (from 07_model_validation.R)
##
## Writes: FIG_DIR/final/figA_model_validation.png / .pdf
########################################################

source("00_config.R", encoding = "UTF-8")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------
# 1. Load and summarise
# -----------------------------------------------------------------------
val <- read_csv(file.path(VALID_DIR, "model_validation_cost.csv"),
                show_col_types = FALSE) %>%
  mutate(
    fecha      = as.Date(fecha),
    ciudad_lbl = factor(CITY_LABS[ciudad],
                        levels = c("Bogotá", "Medellín", "Cali")),
    model      = factor(model, levels = c("CoCA", "CoNA", "CoRD")))

message(sprintf("  %d rows | %s - %s | models: %s", nrow(val),
                min(val$fecha), max(val$fecha),
                paste(levels(droplevels(val$model)), collapse = ", ")))

summ <- val %>%
  group_by(model, ciudad_lbl) %>%
  dplyr::summarise(MAD = mean(abs(dev_pct)),
                   MD  = mean(dev_pct),
                   Max = max(abs(dev_pct)),
                   n   = dplyr::n(),
                   .groups = "drop")

print(summ, n = 12)

plot_df <- summ %>%
  select(model, ciudad_lbl, MAD, MD) %>%
  pivot_longer(c(MAD, MD), names_to = "measure", values_to = "value") %>%
  mutate(measure = factor(
    measure,
    levels = c("MAD", "MD"),
    labels = c("Mean absolute deviation",
               "Mean signed deviation")))

# -----------------------------------------------------------------------
# 2. Figure
#    Scales are free by facet: MAD is bounded below by zero while MD
#    straddles it, so a common axis would compress both panels.
# -----------------------------------------------------------------------
figA <- ggplot(plot_df,
               aes(x = model, y = value, fill = ciudad_lbl)) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7, alpha = 0.9,
           color = "grey25", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%+.2f", value),
                vjust = ifelse(value >= 0, -0.4, 1.3)),
            position = position_dodge(width = 0.8),
            size = 2.6, family = "serif") +
  facet_wrap(~ measure, ncol = 2, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Bogotá"   = unname(CITY_COLS["BOGOTA"]),
      "Medellín" = unname(CITY_COLS["MEDELLIN"]),
      "Cali"     = unname(CITY_COLS["CALI"])),
    name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%+.0f%%", x),
                     expand = expansion(mult = c(0.14, 0.14))) +
  labs(x = NULL, y = "Deviation from cost on observed prices (%)") +
  paper_theme() +
  theme(
    axis.text.x       = element_text(angle = 0, hjust = 0.5, size = 9.5),
    axis.text.y       = element_text(size = 8),
    axis.title.y      = element_text(size = 9),
    strip.text        = element_text(face = "bold", size = 10),
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 9),
    legend.key.width  = unit(1.2, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8),
    panel.spacing     = unit(0.6, "cm"))

ggsave(file.path(FIG_DIR, "final", "figA_model_validation.png"),
       figA, width = 10, height = 5, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "figA_model_validation.pdf"),
       figA, width = 10, height = 5)

message("Figure A (model validation) saved.")