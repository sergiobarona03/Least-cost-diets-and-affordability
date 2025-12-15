# Load packages
library(tidyverse)
library(readxl)
library(seasonal)
library(Metrics)
library(ggplot2)

# Set working directory
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Load dataset
infile <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
dataset <- read_excel(infile)

food.vector <- unique(dataset$alimento_sipsa)
output_metrics <- tibble()
margen_table <- tibble()

for (i in seq_along(food.vector)) {
  food.x <- food.vector[i]
  
  message("Procesando: ", food.x)
  
  # Filter and preprocess
  data.food <- dataset %>%
    filter(alimento_sipsa == food.x) %>%
    drop_na(precio_ipc, precio_sipsa) %>%
    mutate(
      log_ipc   = log(precio_ipc),
      log_sipsa = log(precio_sipsa),
      fecha     = as.Date(paste(Year, Month, 1, sep = "-"))
    )
  
  if (nrow(data.food) < 24) next  # Skip if too few obs
  
  # Create time series
  ts_ipc   <- ts(data.food$log_ipc, start = c(data.food$Year[1], data.food$Month[1]), frequency = 12)
  ts_sipsa <- ts(data.food$log_sipsa, start = c(data.food$Year[1], data.food$Month[1]), frequency = 12)
  
  # Seasonal adjustment
  sa_ipc   <- as.numeric(predict(seas(ts_ipc)))
  sa_sipsa <- as.numeric(predict(seas(ts_sipsa)))
  
  data.food <- data.food %>%
    mutate(
      sa_ipc = sa_ipc,
      sa_sipsa = sa_sipsa
    )
  
  # Split 70/30
  n <- nrow(data.food)
  cut <- floor(0.7 * n)
  train <- data.food[1:cut, ]
  test  <- data.food[(cut + 1):n, ]
  
  # Fit linear model
  model <- lm(sa_ipc ~ sa_sipsa, data = train)
  
  # Predict with intervals
  preds <- predict(model, newdata = test, interval = "confidence", level = 0.95)
  test <- test %>%
    mutate(
      fit = preds[, "fit"],
      lwr = preds[, "lwr"],
      upr = preds[, "upr"]
    )
  
  # Metrics
  rmse_val <- rmse(test$sa_ipc, test$fit)
  mape_val <- mape(exp(test$sa_ipc), exp(test$fit))  # Compare in levels
  
  output_metrics <- bind_rows(output_metrics, tibble(
    alimento_sipsa = food.x,
    RMSE_log = rmse_val,
    MAPE_level = 100 * mape_val
  ))
  
  # Margen comercialización
  coef_summary <- summary(model)$coefficients
  beta <- coef_summary["sa_sipsa", "Estimate"]
  se_beta <- coef_summary["sa_sipsa", "Std. Error"]
  
  margen_df <- tibble(
    alimento_sipsa = food.x,
    margen_exp = exp(beta),
    margen_lo = exp(beta - 1.96 * se_beta),
    margen_hi = exp(beta + 1.96 * se_beta)
  )
  
  margen_table <- bind_rows(margen_table, margen_df)
  
  # Build plot data
  plot_df <- test %>%
    mutate(
      obs_level = exp(sa_ipc),
      fit_level = exp(fit),
      lwr_level = exp(lwr),
      upr_level = exp(upr)
    )
  
  train_df <- train %>%
    mutate(
      obs_level = exp(sa_ipc),
      fit_level = NA,
      lwr_level = NA,
      upr_level = NA
    )
  
  plot_all <- bind_rows(train_df, plot_df)
  
  # Plot
  p <- ggplot(plot_all, aes(x = fecha)) +
    geom_line(aes(y = obs_level, color = "Observado"), linewidth = 0.7) +
    geom_line(aes(y = fit_level, color = "Predicho"), linetype = "dashed", linewidth = 0.7) +
    geom_ribbon(aes(ymin = lwr_level, ymax = upr_level), fill = "blue", alpha = 0.2) +
    scale_color_manual(values = c("Observado" = "black", "Predicho" = "blue")) +
    labs(
      title = paste("Modelo M6 con IC: log(Pmin) ~ log(Pmay)"),
      subtitle = paste0(food.x, " | IC 95% | MAPE: ", round(1 * mape_val, 2), "%"),
      x = "Fecha",
      y = "Precio (nivel)",
      color = ""
    ) +
    theme_minimal()
  
  # Save plot
  ggsave(
    filename = paste0("working-papers/working-paper-1225/m6/output_seas/", food.x, "_m6_plot.png"),
    plot = p,
    width = 9,
    height = 5
  )
}

# Save output tables
write_csv(output_metrics, "working-papers/working-paper-1225/m6/output_seas/summary_metrics_m6.csv")
write_csv(margen_table, "working-papers/working-paper-1225/m6/output_seas/margen_comercializacion_m6.csv")
