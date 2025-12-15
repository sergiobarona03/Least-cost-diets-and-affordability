library(tidyverse)
library(readxl)
library(apt)
library(Metrics)
library(ggplot2)
library(janitor)
library(seasonal)
library(dplyr)

setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

infile <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
dataset <- read_excel(infile)

food.vector <- unique(dataset$alimento_sipsa)
output_metrics <- tibble()

for (i in seq_along(food.vector)) {
  food.x <- food.vector[i]
  print(paste0(food.x, " está listo :)"))
  
  data.food <- dataset %>%
    filter(alimento_sipsa == food.x) %>%
    drop_na(precio_ipc, precio_sipsa) %>%
    mutate(
      log_ipc   = log(precio_ipc),
      log_sipsa = log(precio_sipsa),
      fecha     = as.Date(paste(Year, Month, 1, sep = "-"))
    )
  
  if (nrow(data.food) < 36) next
  
  n <- nrow(data.food)
  cut <- floor(0.90 * n)
  
  train <- data.food[1:cut, ]
  test  <- data.food[(cut + 1):n, ]
  
  ts_y <- ts(train$log_ipc, start = c(train$Year[1], train$Month[1]), frequency = 12)
  ts_x <- ts(train$log_sipsa, start = c(train$Year[1], train$Month[1]), frequency = 12)
  
  detach("package:dplyr", unload = TRUE)
  fit_asy <- apt::ecmAsyFit(
    y      = ts_y,
    x      = ts_x,
    lag    = 1,
    model  = "mtar",
    thresh = 0,
    split  = TRUE
  )
  library(dplyr)
  
  coef_table <- summary(fit_asy) %>%
    janitor::clean_names() %>%
    mutate(
      dep_var = str_trim(dep_var),
      dep_var = str_replace_all(dep_var, "[\\|\\-]", ""),
      dep_var = str_trim(dep_var),
      dep_var = na_if(dep_var, "")
    ) %>%
    fill(dep_var)
  
  get_coef <- function(dep, name) {
    coef_table %>%
      filter(dep_var == dep, ind_var == name) %>%
      pull(estimate)
  }
  
  b0      <- get_coef("diff.ts_y.t_0", "(Intercept)")
  bDypos  <- get_coef("diff.ts_y.t_0", "X.diff.ts_y.t_1.pos")
  bDyneg  <- get_coef("diff.ts_y.t_0", "X.diff.ts_y.t_1.neg")
  bDpos   <- get_coef("diff.ts_y.t_0", "X.diff.ts_x.t_1.pos")
  bDneg   <- get_coef("diff.ts_y.t_0", "X.diff.ts_x.t_1.neg")
  bECTpos <- get_coef("diff.ts_y.t_0", "X.ECT.t_1.pos")
  bECTneg <- get_coef("diff.ts_y.t_0", "X.ECT.t_1.neg")
  
  full_data <- data.food %>%
    mutate(
      dy        = c(NA, diff(log_ipc)),
      dx        = c(NA, diff(log_sipsa)),
      dx_pos    = pmax(dx, 0),
      dx_neg    = pmin(dx, 0),
      log_ratio = log_ipc - log_sipsa,
      lz        = lag(log_ratio)
    )
  
  # Forecast function
  make_predictions <- function(idx_range) {
    yhat <- rep(NA, length(idx_range))
    for (k in seq_along(idx_range)) {
      idx <- idx_range[k]
      if (idx - 1 < 1) next
      
      dx_p <- full_data$dx_pos[idx]
      dx_n <- full_data$dx_neg[idx]
      dy_lag <- full_data$dy[idx - 1]
      lz_val <- full_data$lz[idx]
      
      # If any component is NA, skip prediction
      if (any(is.na(c(dx_p, dx_n, dy_lag, lz_val)))) next
      
      adj_term <- ifelse(lz_val < 0, bECTneg * lz_val, bECTpos * lz_val)
      delta_y <- b0 + bDypos * dy_lag + bDyneg * dy_lag + bDpos * dx_p + bDneg * dx_n + adj_term
      
      if (k == 1) {
        yhat[k] <- full_data$log_ipc[idx - 1] + delta_y
      } else if (!is.na(yhat[k - 1])) {
        yhat[k] <- yhat[k - 1] + delta_y
      } else {
        yhat[k] <- full_data$log_ipc[idx - 1] + delta_y  # fallback
      }
    }
    return(yhat)
  }
  
  
  # Predict for train and test
  train_idx <- 2:cut
  test_idx  <- (cut + 1):n
  
  y_hat_train <- make_predictions(train_idx)
  y_hat_test  <- make_predictions(test_idx)
  
  actual_train <- exp(full_data$log_ipc[train_idx])
  actual_test  <- exp(full_data$log_ipc[test_idx])
  pred_train   <- exp(y_hat_train)
  pred_test    <- exp(y_hat_test)
  
  rmse_train <- rmse(actual_train, pred_train)
  mape_train <- mape(actual_train, pred_train)
  rmse_test  <- rmse(actual_test, pred_test)
  mape_test  <- mape(actual_test, pred_test)
  
  output_metrics <- bind_rows(output_metrics, tibble(
    alimento_sipsa = food.x,
    RMSE_train = rmse_train,
    MAPE_train = 100 * mape_train,
    RMSE_test  = rmse_test,
    MAPE_test  = 100 * mape_test
  ))
  
  train_df <- tibble(
    fecha = full_data$fecha[train_idx],
    obs   = actual_train,
    pred  = pred_train,
    lo    = exp(log(pred_train) - 1.96 * 0.06119),
    hi    = exp(log(pred_train) + 1.96 * 0.06119)
  )
  
  test_df <- tibble(
    fecha = full_data$fecha[test_idx],
    obs   = actual_test,
    pred  = pred_test,
    lo    = exp(log(pred_test) - 1.96 * 0.06119),
    hi    = exp(log(pred_test) + 1.96 * 0.06119)
  )
  
  plot_df <- bind_rows(train_df, test_df)
  
  p <- ggplot(plot_df, aes(x = fecha)) +
    geom_line(aes(y = obs, colour = "Observado"), linewidth = 0.7) +
    geom_line(aes(y = pred, colour = "Predicho"), linetype = "dashed", linewidth = 0.7) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "red", color = "black", alpha = 0.3, linetype = 2) +
    scale_color_manual(values = c("Observado" = "black", "Predicho" = "red")) +
    labs(
      title = paste("ECM Predicción -", food.x),
      subtitle = paste0("Train MAPE: ", round(100 * mape_train, 2), "% | Test MAPE: ", round(100 * mape_test, 2), "%"),
      x = "Fecha",
      y = "Precio",
      colour = ""
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("working-papers/working-paper-1225/m4/output/", food.x, "_ecm_plot.png"),
    plot = p,
    width = 9,
    height = 5
  )
}

write_csv(output_metrics, "working-papers/working-paper-1225/m4/output/summary_metrics.csv")
