# ===============================================================
# M7: First-difference model with MONTH DUMMIES (no seas())
# dy = a0 + a1 dx + month FE
# ===============================================================

library(tidyverse)
library(readxl)
library(Metrics)
library(ggplot2)

setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# -----------------------------
# Paths
# -----------------------------
infile <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
out_dir <- "working-papers/working-paper-1225/m7/output_dummies"
plot_dir <- file.path(out_dir)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Load data
# -----------------------------
dataset <- read_excel(infile)

food.vector <- unique(dataset$alimento_sipsa)

output_metrics <- tibble()
pass_through_table <- tibble()

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
      fecha     = as.Date(paste(Year, Month, 1, sep = "-")),
      month     = factor(Month)
    ) %>%
    arrange(fecha)
  
  if (nrow(data.food) < 36) next
  
  # -----------------------------
  # First differences
  # -----------------------------
  df <- data.food %>%
    mutate(
      dy = log_ipc - lag(log_ipc),
      dx = log_sipsa - lag(log_sipsa)
    ) %>%
    drop_na(dy, dx)
  
  if (nrow(df) < 24) next
  
  # -----------------------------
  # Train/test split (70/30)
  # -----------------------------
  n <- nrow(df)
  cut <- floor(0.7 * n)
  
  train <- df[1:cut, ]
  test  <- df[(cut + 1):n, ]
  
  if (nrow(test) < 3) next
  
  # -----------------------------
  # M7 model with MONTH DUMMIES
  # -----------------------------
  model <- lm(dy ~ dx + month, data = train)
  
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
  # Predict dy with CI
  # -----------------------------
  preds <- predict(model, newdata = test, interval = "confidence", level = 0.95)
  
  test <- test %>%
    mutate(
      dy_fit = preds[, "fit"],
      dy_lwr = preds[, "lwr"],
      dy_upr = preds[, "upr"]
    )
  
  # -----------------------------
  # Reconstruct LOG LEVELS
  # ŷ_t = y_{t-1} + Δŷ_t
  # -----------------------------
  y0 <- train$log_ipc[nrow(train)]
  
  test <- test %>%
    mutate(
      yhat_log     = y0 + cumsum(dy_fit),
      yhat_log_lwr = y0 + cumsum(dy_lwr),
      yhat_log_upr = y0 + cumsum(dy_upr),
      obs_level    = exp(log_ipc),
      fit_level    = exp(yhat_log),
      lwr_level    = exp(yhat_log_lwr),
      upr_level    = exp(yhat_log_upr)
    )
  
  # -----------------------------
  # Metrics
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
  # Plot
  # -----------------------------
  plot_all <- df %>%
    mutate(obs_level_full = exp(log_ipc)) %>%
    select(fecha, obs_level_full) %>%
    left_join(
      test %>% select(fecha, fit_level, lwr_level, upr_level),
      by = "fecha"
    )
  
  p <- ggplot(plot_all, aes(x = fecha)) +
    geom_line(aes(y = obs_level_full, color = "Observado"), linewidth = 0.7) +
    geom_line(aes(y = fit_level, color = "Predicho"), linetype = "dashed", linewidth = 0.7) +
    # geom_ribbon(aes(ymin = lwr_level, ymax = upr_level), alpha = 0.25) +
    labs(
      title = "M7: Δlog(Pmin) ~ Δlog(Pmay) + month dummies",
      subtitle = paste0(
        food.x,
        " | β(dx) = ", round(beta, 3),
        " | MAPE: ", round(mape_level, 2), "%"
      ),
      x = "Fecha",
      y = "Precio (nivel)",
      color = ""
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path(plot_dir, paste0(safe_name(food.x), "_m7_dummies.png")),
    plot = p,
    width = 9,
    height = 5,
    dpi = 300
  )
}

# -----------------------------
# Save outputs
# -----------------------------
write_csv(output_metrics,
          file.path(out_dir, "summary_metrics_m7_dummies.csv"))

write_csv(pass_through_table,
          file.path(out_dir, "pass_through_m7_dummies.csv"))
