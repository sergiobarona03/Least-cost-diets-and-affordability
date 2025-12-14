############################################################
## Methodology 1: Validation (by food + aggregate + plots) ##
############################################################

# -----------------------
# 0. Libraries
# -----------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(ggplot2)

# -----------------------
# 1. Working directory
# -----------------------
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

date_tag <- "121225"

# -----------------------
# 2. Load merged IPC–SIPSA data
# -----------------------
data_merged <- read_excel(
  paste0(
    "working-papers\\working-paper-1225\\mapeo ipc-sipsa\\output\\",
    date_tag, "_dataset_ipc_sipsa.xlsx"
  )
)

# -----------------------
# 3. Load Methodology 1 (margins)
# -----------------------
source("working-papers\\working-paper-1225\\m1\\m1-model.R")
# Expected object:
# margenes_q: alimento_sipsa | margen_q1 | margen_q2 | margen_q3

# -----------------------
# 4. Prepare validation dataset
# -----------------------
data_val <- data_merged %>%
  filter(
    !is.na(precio_sipsa),
    !is.na(precio_ipc),
    precio_sipsa > 0
  ) %>%
  left_join(
    margenes_q %>%
      select(alimento_sipsa, margen_q1, margen_q2, margen_q3),
    by = "alimento_sipsa"
  ) %>%
  drop_na(margen_q2) %>%
  mutate(
    date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(alimento_sipsa, date)

# -----------------------
# 5. Train / test split (70/30, temporal, by food)
# -----------------------
data_val <- data_val %>%
  group_by(alimento_sipsa) %>%
  mutate(
    t_id = row_number(),
    cutoff = floor(0.7 * max(t_id)),
    sample = ifelse(t_id <= cutoff, "train", "test")
  ) %>%
  ungroup()

test_data <- data_val %>% filter(sample == "test")

# -----------------------
# 6. Forecast retail prices (Q1, Q2, Q3)
# -----------------------
test_data <- test_data %>%
  mutate(
    pred_q1 = precio_sipsa * (1 + margen_q1 / 100),
    pred_q2 = precio_sipsa * (1 + margen_q2 / 100),
    pred_q3 = precio_sipsa * (1 + margen_q3 / 100)
  )

# -----------------------
# 7. Metrics functions
# -----------------------
rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2, na.rm = TRUE))
}

mape <- function(y, yhat) {
  mean(abs((y - yhat) / y), na.rm = TRUE) * 100
}

# -----------------------
# 8. Metrics BY FOOD
# -----------------------
metrics_food <- test_data %>%
  group_by(alimento_sipsa) %>%
  summarise(
    n_test = n(),
    RMSE_Q1 = rmse(precio_ipc, pred_q1),
    RMSE_Q2 = rmse(precio_ipc, pred_q2),
    RMSE_Q3 = rmse(precio_ipc, pred_q3),
    MAPE_Q1 = mape(precio_ipc, pred_q1),
    MAPE_Q2 = mape(precio_ipc, pred_q2),
    MAPE_Q3 = mape(precio_ipc, pred_q3),
    .groups = "drop"
  )

write_csv(
  metrics_food,
  paste0("working-papers\\working-paper-1225\\m1\\output\\", date_tag, "_m1_metrics_by_food.csv")
)

# -----------------------
# 9. Aggregate metrics
# -----------------------
metrics_agg <- tibble(
  scenario = c("Q1 (low margin)", "Q2 (median)", "Q3 (high margin)"),
  RMSE = c(
    rmse(test_data$precio_ipc, test_data$pred_q1),
    rmse(test_data$precio_ipc, test_data$pred_q2),
    rmse(test_data$precio_ipc, test_data$pred_q3)
  ),
  MAPE = c(
    mape(test_data$precio_ipc, test_data$pred_q1),
    mape(test_data$precio_ipc, test_data$pred_q2),
    mape(test_data$precio_ipc, test_data$pred_q3)
  )
)

write_csv(
  metrics_agg,
  paste0("working-papers\\working-paper-1225\\m1\\output\\", date_tag, "_m1_metrics_aggregate.csv")
)

print(metrics_agg)

# -----------------------
# 10. Forecast plots BY FOOD (Q1–Q3 band)
# -----------------------
library(ggforce)

plot_food <- test_data %>%
  select(
    alimento_sipsa,
    date,
    precio_ipc,
    pred_q1,
    pred_q2,
    pred_q3
  )

n_per_page <- 9
n_pages <- ceiling(length(unique(plot_food$alimento_sipsa)) / n_per_page)

for (p in seq_len(n_pages)) {
  
  gg_food <- ggplot(plot_food, aes(x = date)) +
    
    # Q1–Q3 prediction band
    geom_ribbon(
      aes(
        ymin = pred_q1,
        ymax = pred_q3
      ),
      alpha = 0.25
    ) +
    
    # Observed retail price
    geom_line(
      aes(y = precio_ipc),
      linewidth = 0.8
    ) +
    
    # Median prediction (Q2)
    geom_line(
      aes(y = pred_q2),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    facet_wrap_paginate(
      ~ alimento_sipsa,
      scales = "free_y",
      ncol = 3,
      nrow = 3,
      page = p
    ) +
    
    labs(
      title = "Observed vs Predicted Retail Prices — Methodology 1",
      subtitle = paste("Q1–Q3 margin band | Page", p, "of", n_pages),
      y = "Price",
      x = NULL
    ) +
    
    theme_classic()
  
  ggsave(
    paste0(
      "working-papers\\working-paper-1225\\m1\\output\\",
      date_tag,
      "_m1_forecast_by_food_page_",
      p,
      ".png"
    ),
    gg_food,
    width = 13,
    height = 9
  )
}
