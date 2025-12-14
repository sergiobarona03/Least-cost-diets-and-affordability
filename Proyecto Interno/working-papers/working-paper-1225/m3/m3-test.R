############################################################
## Metodología III (ECM) - Validación
## Input: m3_predictions_by_food_city_<city>.csv
## Output: metrics by food + aggregate + paginated plots
############################################################

library(tidyverse)
library(readr)
library(ggplot2)
library(ggforce)

# -----------------------
# Settings
# -----------------------
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

date_tag  <- "121225"
city_code <- "76001"

out_dir <- "working-papers\\working-paper-1225\\m3\\output\\"
in_pred <- paste0(out_dir, date_tag, "_m3_predictions_by_food_city_", city_code, ".csv")

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
    COVER_95 = coverage(observed, pred_lo95, pred_hi95),
    .groups = "drop"
  )

write_csv(metrics_by_food, paste0(out_dir, date_tag, "_m3_metrics_by_food_city_", city_code, ".csv"))

# -----------------------
# 2) Aggregate metrics
# -----------------------
metrics_agg <- tibble(
  RMSE = rmse(pred$observed, pred$pred_mean),
  MAPE = mape(pred$observed, pred$pred_mean),
  COVER_95 = coverage(pred$observed, pred$pred_lo95, pred$pred_hi95)
)

write_csv(metrics_agg, paste0(out_dir, date_tag, "_m3_metrics_aggregate_city_", city_code, ".csv"))
print(metrics_agg)

# -----------------------
# 3) Forecast plots BY FOOD (paginated) with 95% band
# -----------------------
n_per_page <- 9
n_pages <- ceiling(length(unique(pred$alimento_sipsa)) / n_per_page)

for (p in seq_len(n_pages)) {
  
  gg_food <- ggplot(pred, aes(x = date)) +
    geom_ribbon(aes(ymin = pred_lo95, ymax = pred_hi95), alpha = 0.25) +
    geom_line(aes(y = observed), linewidth = 0.8) +
    geom_line(aes(y = pred_mean), linetype = "dashed", linewidth = 0.8) +
    facet_wrap_paginate(
      ~ alimento_sipsa,
      scales = "free_y",
      ncol = 3, nrow = 3,
      page = p
    ) +
    labs(
      title = paste0("Observed vs Predicted Retail Prices — Methodology III (ECM) | City ", city_code),
      subtitle = paste("95% band | Page", p, "of", n_pages),
      y = "Price",
      x = NULL
    ) +
    theme_classic()
  
  ggsave(
    paste0(out_dir, date_tag, "_m3_forecast_by_food_city_", city_code, "_page_", p, ".png"),
    gg_food,
    width = 13,
    height = 9
  )
}


