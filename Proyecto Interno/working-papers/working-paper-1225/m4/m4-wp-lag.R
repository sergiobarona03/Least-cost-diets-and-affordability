############################################################
## M4: Asymmetric ECM (apt::ecmAsyFit, MTAR)               ##
## + selección de rezago (helper: m4-auxf-lag.R)           ##
## + métricas train/test en Δlog_ipc (dy)                  ##
## + plots AGRUPADOS en páginas (3x3)                      ##
## NOTA: Se evalúa en la variable dependiente original:    ##
##       Δlog_ipc (NO se convierte a niveles para IC)      ##
############################################################

library(tidyverse)
library(readxl)
library(apt)
library(Metrics)
library(ggplot2)
library(janitor)
library(dplyr)
library(lubridate)
library(readr)
library(ggforce)   # facet_wrap_paginate

setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# -----------------------------
# Paths
# -----------------------------
infile  <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
out_dir <- "working-papers/working-paper-1225/m4/output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Cargar funciones auxiliares
# -----------------------------
source("working-papers/working-paper-1225/m4/m4-auxf-lag.R")

# -----------------------------
# Load data
# -----------------------------
dataset <- read_excel(infile)
food.vector <- sort(unique(dataset$alimento_sipsa))

# -----------------------------
# Outputs
# -----------------------------
output_metrics <- tibble()
lag_selection_all <- tibble()
plot_store <- list()

# -----------------------------
# Loop over foods
# -----------------------------
for (i in seq_along(food.vector)) {
  
  food.x <- food.vector[i]
  message("Procesando: ", food.x)
  
  data.food <- dataset %>%
    filter(alimento_sipsa == food.x) %>%
    drop_na(precio_ipc, precio_sipsa) %>%
    mutate(
      log_ipc   = log(precio_ipc),
      log_sipsa = log(precio_sipsa),
      fecha     = as.Date(paste(Year, Month, 1, sep = "-"))
    ) %>%
    arrange(fecha)
  
  if (nrow(data.food) < 36) next
  
  # -----------------------------
  # Split 90/10 (mantengo tu estructura)
  # -----------------------------
  n <- nrow(data.food)
  cut <- floor(0.90 * n)
  
  train <- data.food[1:cut, ]
  test  <- data.food[(cut + 1):n, ]
  
  if (nrow(test) < 6) next
  
  train_idx <- 1:cut
  test_idx  <- (cut + 1):n
  
  # -----------------------------
  # Selección de rezago óptimo por RMSE en TEST (dy)
  # -----------------------------
  sel <- select_m4_lag(data.food, train_idx = train_idx, test_idx = test_idx, max_lag = 6)
  if (is.null(sel)) next
  
  # Guardar ranking
  lag_selection_all <- bind_rows(
    lag_selection_all,
    sel$table %>%
      mutate(alimento_sipsa = food.x) %>%
      select(alimento_sipsa, lag, RMSE_test_dy, n_test)
  )
  
  best_lag <- sel$best$lag
  coefs    <- sel$best$coefs
  
  # -----------------------------
  # Construcción full_data (dy, dx, etc.)
  # -----------------------------
  full_data <- data.food %>%
    mutate(
      dy     = c(NA, diff(log_ipc)),
      dx     = c(NA, diff(log_sipsa)),
      dx_pos = pmax(dx, 0),
      dx_neg = pmin(dx, 0)
    )
  
  # -----------------------------
  # Forecast en TRAIN (one-step recursivo desde t=2..cut)
  # - aquí arrancamos en idx0=2 para que dy exista
  # -----------------------------
  train_fc_idx <- 2:cut
  train_fc <- forecast_m4_dy(full_data, train_end_idx = 2, test_idx = train_fc_idx, coefs = coefs)
  
  # -----------------------------
  # Forecast en TEST (genuino)
  # -----------------------------
  test_fc <- forecast_m4_dy(full_data, train_end_idx = cut, test_idx = test_idx, coefs = coefs)
  
  if (is.null(train_fc) || is.null(test_fc)) next
  
  train_ok <- train_fc %>% drop_na(dy_obs, dy_hat)
  test_ok  <- test_fc  %>% drop_na(dy_obs, dy_hat)
  
  if (nrow(test_ok) < 3) next
  
  # -----------------------------
  # Métricas train/test (dy)
  # -----------------------------
  rmse_train <- rmse(train_ok$dy_obs, train_ok$dy_hat)
  mape_train <- mape(abs(train_ok$dy_obs), abs(train_ok$dy_hat))
  
  rmse_test  <- rmse(test_ok$dy_obs, test_ok$dy_hat)
  mape_test  <- mape(abs(test_ok$dy_obs), abs(test_ok$dy_hat))
  
  output_metrics <- bind_rows(
    output_metrics,
    tibble(
      alimento_sipsa = food.x,
      lag_optimo = best_lag,
      RMSE_train_dy = rmse_train,
      MAPE_train_dy = mape_train,
      RMSE_test_dy  = rmse_test,
      MAPE_test_dy  = mape_test,
      n_total = n
    )
  )
  
  # -----------------------------
  # Bandas naive en dy
  # - banda fija basada en sd(error_train)
  # -----------------------------
  sigma_train <- sd(train_ok$dy_obs - train_ok$dy_hat, na.rm = TRUE)
  if (is.na(sigma_train) || sigma_train <= 0) sigma_train <- 0
  
  plot_df <- bind_rows(
    train_fc %>% mutate(split = "train"),
    test_fc  %>% mutate(split = "test")
  ) %>%
    mutate(
      alimento_sipsa = food.x,
      lo = dy_hat - 1.96 * sigma_train,
      hi = dy_hat + 1.96 * sigma_train
    )
  
  plot_store[[length(plot_store) + 1]] <- plot_df
}

# -----------------------------
# Save outputs
# -----------------------------
write_csv(output_metrics,      file.path(out_dir, "summary_metrics_dlog.csv"))
write_csv(lag_selection_all,   file.path(out_dir, "lag_selection_all_foods.csv"))

# -----------------------------
# Grouped plots (3x3)
# -----------------------------
plot_all <- if (length(plot_store) == 0) tibble() else bind_rows(plot_store)

foods_ok <- sort(unique(plot_all$alimento_sipsa))
n_per_page <- 9
n_pages <- ceiling(length(foods_ok) / n_per_page)
  
for (p in seq_len(n_pages)) {
    gg_food <- ggplot(plot_all, aes(x = fecha)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.30, linetype = 2) +
      geom_line(aes(y = dy_obs), linewidth = 0.7) +
      geom_line(aes(y = dy_hat), linetype = "dashed", linewidth = 0.7) +
      facet_wrap_paginate(
        ~ alimento_sipsa,
        scales = "free_y",ncol = 3, nrow = 3,
        page = p) +labs(
        title = "M4: Asymmetric ECM (MTAR) — Observed vs Predicted (Δlog_ipc)",
        subtitle = paste("Bandas naive (±1.96*sd(error_train)) | Page", p, "of", n_pages),
        x = NULL,y = "Δlog(Precio minorista)") + theme_bw(base_size = 11) +
      theme(strip.text = element_text(face = "bold", size = 9))
  
    ggsave(
      filename = file.path(out_dir, paste0("m4_ecm_dlog_grouped_page_", sprintf("%02d", p), ".png")),
      plot = gg_food,width = 13, height = 9,dpi = 300)
  }
