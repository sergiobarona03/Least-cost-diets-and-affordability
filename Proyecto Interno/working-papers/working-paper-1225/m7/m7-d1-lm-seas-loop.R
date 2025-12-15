# ===============================================================
# M7: First-difference model on seasonally-adjusted log prices
#     dy = a0 + a1 dx  (food-by-food)
#     + backtest 70/30, level reconstruction, CI ribbon, outputs
# ===============================================================

library(tidyverse)
library(readxl)
library(seasonal)
library(Metrics)
library(ggplot2)

setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# -----------------------------
# Paths
# -----------------------------
infile <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
out_dir <- "working-papers/working-paper-1225/m7/output_seas"
plot_dir <- file.path(out_dir)

# -----------------------------
# Load data
# -----------------------------
dataset <- read_excel(infile)

food.vector <- unique(dataset$alimento_sipsa)

output_metrics <- tibble()
pass_through_table <- tibble()

# Optional: observed "margin" summary from overlap (NOT model-based)
# margin_t = exp(sa_ipc - sa_sipsa) - 1
margin_overlap_table <- tibble()

# -----------------------------
# Helper: safe filename
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# -----------------------------
# Loop food-by-food
# -----------------------------
for (i in seq_along(food.vector)) {
  
  food.x <- food.vector[i]
  message("Procesando: ", food.x)
  
  # Keep ONLY overlap rows for estimation (need both prices)
  data.food <- dataset %>%
    filter(alimento_sipsa == food.x) %>%
    drop_na(precio_ipc, precio_sipsa) %>%
    mutate(
      log_ipc   = log(precio_ipc),
      log_sipsa = log(precio_sipsa),
      fecha     = as.Date(paste(Year, Month, 1, sep = "-"))
    ) %>%
    arrange(fecha)
  
  # Need enough obs for seasonal adjustment + split + diff
  if (nrow(data.food) < 36) next
  
  # -----------------------------
  # Seasonal adjustment (LOG series)
  # IMPORTANT: use predict(seas(ts))
  # -----------------------------
  ts_ipc   <- ts(data.food$log_ipc,   start = c(data.food$Year[1], data.food$Month[1]), frequency = 12)
  ts_sipsa <- ts(data.food$log_sipsa, start = c(data.food$Year[1], data.food$Month[1]), frequency = 12)
  
  sa_ipc <- tryCatch(as.numeric(predict(seas(ts_ipc))), error = function(e) rep(NA_real_, length(ts_ipc)))
  sa_sipsa <- tryCatch(as.numeric(predict(seas(ts_sipsa))), error = function(e) rep(NA_real_, length(ts_sipsa)))
  
  data.food <- data.food %>%
    mutate(
      sa_ipc = sa_ipc,
      sa_sipsa = sa_sipsa
    ) %>%
    drop_na(sa_ipc, sa_sipsa)
  
  if (nrow(data.food) < 30) next
  
  # -----------------------------
  # Build first differences (aligned at time t)
  # dy_t = sa_ipc_t - sa_ipc_{t-1}
  # dx_t = sa_sipsa_t - sa_sipsa_{t-1}
  # -----------------------------
  df <- data.food %>%
    mutate(
      dy = sa_ipc - lag(sa_ipc),
      dx = sa_sipsa - lag(sa_sipsa)
    ) %>%
    drop_na(dy, dx)
  
  if (nrow(df) < 24) next
  
  # -----------------------------
  # Train/test split on DIFFERENCED sample
  # -----------------------------
  n <- nrow(df)
  cut <- floor(0.7 * n)
  
  train <- df[1:cut, ]
  test  <- df[(cut + 1):n, ]
  
  if (nrow(test) < 3) next
  
  # -----------------------------
  # Fit M7 model on changes
  # -----------------------------
  model <- lm(dy ~ dx, data = train)
  
  # Pass-through coefficient (short-run)
  cs <- summary(model)$coefficients
  beta <- cs["dx", "Estimate"]
  se_beta <- cs["dx", "Std. Error"]
  
  pass_through_table <- bind_rows(pass_through_table, tibble(
    alimento_sipsa = food.x,
    beta_dx = beta,
    beta_dx_lo = beta - 1.96 * se_beta,
    beta_dx_hi = beta + 1.96 * se_beta
  ))
  
  # -----------------------------
  # Predict dy on test with CI
  # -----------------------------
  preds <- predict(model, newdata = test, interval = "confidence", level = 0.95)
  test <- test %>%
    mutate(
      dy_fit = preds[, "fit"],
      dy_lwr = preds[, "lwr"],
      dy_upr = preds[, "upr"]
    )
  
  # -----------------------------
  # Reconstruct LEVELS in logs for plotting/metrics:
  # yhat_t = y_{t-1} + dy_hat_t (recursive)
  #
  # Start level = last observed sa_ipc in train at its last date
  # -----------------------------
  y0 <- train$sa_ipc[nrow(train)]   # log-level at last train date
  
  test <- test %>%
    mutate(
      yhat_log = y0 + cumsum(dy_fit),
      yhat_log_lwr = y0 + cumsum(dy_lwr),
      yhat_log_upr = y0 + cumsum(dy_upr)
    )
  
  # Convert to level for evaluation/plot
  test <- test %>%
    mutate(
      obs_level = exp(sa_ipc),
      fit_level = exp(yhat_log),
      lwr_level = exp(yhat_log_lwr),
      upr_level = exp(yhat_log_upr)
    )
  
  # -----------------------------
  # Metrics
  # 1) RMSE on dy (model space)
  # 2) RMSE/MAPE on levels (reconstructed)
  # -----------------------------
  rmse_dy <- rmse(test$dy, test$dy_fit)
  rmse_level <- rmse(test$obs_level, test$fit_level)
  mape_level <- 1 * mape(test$obs_level, test$fit_level)
  
  output_metrics <- bind_rows(output_metrics, tibble(
    alimento_sipsa = food.x,
    RMSE_dy = rmse_dy,
    RMSE_level = rmse_level,
    MAPE_level = mape_level,
    n_obs = nrow(df)
  ))
  
  # -----------------------------
  # Optional: observed margin summary from overlap
  # margin_t = exp(sa_ipc - sa_sipsa) - 1
  # (this is NOT "estimated from the dy~dx model")
  # -----------------------------
  margin_vec <- exp(df$sa_ipc - df$sa_sipsa) - 1
  margin_overlap_table <- bind_rows(margin_overlap_table, tibble(
    alimento_sipsa = food.x,
    margin_median = median(margin_vec, na.rm = TRUE),
    margin_q05 = quantile(margin_vec, 0.05, na.rm = TRUE),
    margin_q95 = quantile(margin_vec, 0.95, na.rm = TRUE)
  ))
  
  # -----------------------------
  # Plot: full observed + test predictions with ribbon
  # -----------------------------
  plot_all <- df %>%
    mutate(obs_level_full = exp(sa_ipc)) %>%
    select(fecha, obs_level_full) %>%
    left_join(
      test %>% select(fecha, fit_level, lwr_level, upr_level),
      by = "fecha"
    )
  
  p <- ggplot(plot_all, aes(x = fecha)) +
    geom_line(aes(y = obs_level_full, color = "Observado"), linewidth = 0.7) +
    geom_line(aes(y = fit_level, color = "Predicho"), linetype = "dashed", linewidth = 0.7) +
    # geom_ribbon(aes(ymin = lwr_level, ymax = upr_level), alpha = 0.2) +
    labs(
      title = "M7: Δ(sa_log IPC) ~ Δ(sa_log SIPSA)",
      subtitle = paste0(food.x,
                        " | Pass-through β(dx): ", round(beta, 3),
                        " | MAPE(level): ", round(mape_level, 2), "%"),
      x = "Fecha",
      y = "Precio (nivel, exp(sa_log))",
      color = ""
    ) +
    theme_minimal()
  
  out_plot <- file.path(plot_dir, paste0(safe_name(food.x), "_m7_plot.png"))
  ggsave(out_plot, plot = p, width = 9, height = 5, dpi = 300)
}

# -----------------------------
# Save outputs
# -----------------------------
write_csv(output_metrics, file.path(out_dir, "summary_metrics_m7.csv"))
write_csv(pass_through_table, file.path(out_dir, "pass_through_m7_beta_dx.csv"))
write_csv(margin_overlap_table, file.path(out_dir, "margin_overlap_summary.csv"))
