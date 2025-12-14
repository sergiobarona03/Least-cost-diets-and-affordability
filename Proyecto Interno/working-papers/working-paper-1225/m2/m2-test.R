############################################################
## Metodología II (SARIMAX) - Validación                    ##
## Input: predictions_by_food.csv                           ##
## Output: metrics by food + aggregate + paginated plots    ##
############################################################

# -----------------------
# Libraries
# -----------------------
library(tidyverse)
library(readr)
library(ggplot2)
library(ggforce)

# -----------------------
# Paths / settings
# -----------------------
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")
date_tag <- "121225"

out_dir <- "working-papers\\working-paper-1225\\m2\\output\\"
in_pred <- paste0(out_dir, date_tag, "_m2_predictions_by_food.csv")

# -----------------------
# Load predictions
# -----------------------
pred <- read_csv(in_pred, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# -----------------------
# Metrics functions
# -----------------------
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mape <- function(y, yhat) mean(abs((y - yhat) / y), na.rm = TRUE) * 100

coverage <- function(y, lo, hi) mean(y >= lo & y <= hi, na.rm = TRUE) * 100

# -----------------------
# 1) Metrics BY FOOD
# -----------------------
metrics_by_food <- pred %>%
  group_by(alimento_sipsa) %>%
  summarise(
    n_test = first(n_test),
    RMSE = rmse(observed, pred_mean),
    MAPE = mape(observed, pred_mean),
    COVER_80 = coverage(observed, pred_lo80, pred_hi80),
    COVER_95 = coverage(observed, pred_lo95, pred_hi95),
    .groups = "drop"
  )

write_csv(metrics_by_food, paste0(out_dir, date_tag, "_m2_metrics_by_food.csv"))

# -----------------------
# 2) Aggregate metrics
# -----------------------
metrics_agg <- tibble(
  RMSE = rmse(pred$observed, pred$pred_mean),
  MAPE = mape(pred$observed, pred$pred_mean),
  COVER_80 = coverage(pred$observed, pred$pred_lo80, pred$pred_hi80),
  COVER_95 = coverage(pred$observed, pred$pred_lo95, pred$pred_hi95)
)

write_csv(metrics_agg, paste0(out_dir, date_tag, "_m2_metrics_aggregate.csv"))
print(metrics_agg)

# -----------------------
# 3) Forecast plots BY FOOD (paginated) with band
# Choose band: 80% or 95%
# -----------------------
band_level <- "95"  # change to "80" if you prefer narrower bands

plot_food <- pred %>%
  mutate(
    lo = pred_lo95,
    hi = pred_hi95
  )

n_per_page <- 9
n_pages <- ceiling(length(unique(plot_food$alimento_sipsa)) / n_per_page)

for (p in seq_len(n_pages)) {

  gg_food <- ggplot(plot_food, aes(x = date)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25) +
    geom_line(aes(y = observed), linewidth = 0.8) +
    geom_line(aes(y = pred_mean), linetype = "dashed", linewidth = 0.8) +
    facet_wrap_paginate(
      ~ alimento_sipsa,
      scales = "free_y",
      ncol = 3, nrow = 3,
      page = p
    ) +
    labs(
      title = "Observed vs Predicted Retail Prices — Methodology II (SARIMAX)",
      subtitle = paste0("Prediction band: ", band_level, "% | Page ", p, " of ", n_pages),
      y = "Price",
      x = NULL
    ) +
    theme_classic()
  
  ggsave(
    paste0(out_dir, date_tag, "_m2_forecast_by_food_page_", p, ".png"),
    gg_food,
    width = 13,
    height = 9
  )
}

