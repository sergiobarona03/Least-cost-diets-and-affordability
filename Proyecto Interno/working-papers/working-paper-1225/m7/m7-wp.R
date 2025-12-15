
########################################################
## Metodología 3: Modelo Pass-through de corto-plazo
## lm(Δlog_ipc ~ Δlog_sipsa + month dummies) 
## considerando la tendencia estocástica
## Train <= 70% de la muestra
## Fuente de datos: DANE - IPC y DANE-SIPSA
## Objetivo: a partir de la unión entre los datos del
## IPC y SIPSA, se calcula una regresión lineal controlando
## por estacionalidad mensual
########################################################

# Directorio de trabajo
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# -----------------------------
# 1. Librerías y rutas
# -----------------------------
library(tidyverse)
library(readxl)
library(Metrics)
library(ggplot2)
library(lubridate)
library(readr)
library(ggforce)   

# Ruta input
infile   <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
dataset <- read_excel(infile)

# Ruta output
out_dir  <- "working-papers/working-paper-1225/m7/output_dummies"
plot_dir <- file.path(out_dir)


# -----------------------------
# 2. Receptores de output y función auxiliar
# -----------------------------

# Receptores
food.vector <- sort(unique(dataset$alimento_sipsa))
output_metrics <- tibble()
pass_through_table <- tibble()
plot_store <- list()

# Función auxiliar
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# -----------------------------
# 3. Bucle sobre los alimentos
# -----------------------------
for (i in seq_along(food.vector)) {
  food.x <- food.vector[i]
  message("Procesando: ", food.x)
  
  # Creación de variables
  data.food <- dataset %>%
    filter(alimento_sipsa == food.x) %>%
    drop_na(precio_ipc, precio_sipsa) %>%
    mutate(
      log_ipc   = log(precio_ipc),
      log_sipsa = log(precio_sipsa),
      fecha     = as.Date(paste(Year, Month, 1, sep = "-")),
      month     = factor(Month)
    ) %>% arrange(fecha)
  
  if (nrow(data.food) < 36) next
  
  # Cálculo de primeras diferencias (se pierde una observación)
  df <- data.food %>%
    mutate(
      dy = log_ipc - lag(log_ipc),
      dx = log_sipsa - lag(log_sipsa)
    ) %>% drop_na(dy, dx)
  
  if (nrow(df) < 24) next

  # División de la muestra (70/30)
  n <- nrow(df)
  cut <- floor(0.7 * n)
  train <- df[1:cut, ]
  test  <- df[(cut + 1):n, ]
  
  if (nrow(test) < 3) next
  
  # Modelo en primeras diferencias
  model <- lm(dy ~ dx + month, data = train)
  cs <- summary(model)$coefficients
  beta <- cs["dx", "Estimate"]
  se_beta <- cs["dx", "Std. Error"]
  
  # Impacto de corto plazo con 95% IC
  pass_through_table <- bind_rows(
    pass_through_table,
    tibble(alimento_sipsa = food.x,
      beta_dx = beta,
      beta_dx_lo = beta - 1.96 * se_beta,
      beta_dx_hi = beta + 1.96 * se_beta))
  
  # Predicción sobre conjunto de datos de validación
  preds_ci <- predict(model, newdata = test, interval = "confidence", level = 0.95)
  
  test <- test %>%
    mutate(
      dy_fit = as.numeric(preds_ci[, "fit"]),
      dy_lwr = as.numeric(preds_ci[, "lwr"]),
      dy_upr = as.numeric(preds_ci[, "upr"])
    ) %>%
    arrange(fecha)
  
  # Forecast recursivo en niveles (log):
  y0 <- tail(train$log_ipc, 1)
  
  test <- test %>%
    mutate(
      yhat_log     = y0 + cumsum(dy_fit),
      yhat_log_lwr = y0 + cumsum(dy_lwr),
      yhat_log_upr = y0 + cumsum(dy_upr),
      
      obs_level = exp(log_ipc),
      fit_level = exp(yhat_log),
      lwr_level = exp(yhat_log_lwr),
      upr_level = exp(yhat_log_upr)
    )
  
  # Cálculo de las métricas
  rmse_dy    <- rmse(test$dy, test$dy_fit)
  rmse_level <- rmse(test$obs_level, test$fit_level)
  mape_level <- mape(test$obs_level, test$fit_level)*100
  
  output_metrics <- bind_rows(
    output_metrics,
    tibble(
      alimento_sipsa = food.x,
      RMSE_dy = rmse_dy,
      RMSE_level = rmse_level,
      MAPE_level = mape_level,
      n_obs = nrow(df)
    )
  )
  
  # Datos para gráficos
  plot_all <- df %>%
    mutate(
      alimento_sipsa = food.x,
      obs_level_full = exp(log_ipc)
    ) %>%
    select(alimento_sipsa, fecha, obs_level_full) %>%
    left_join(
      test %>% select(fecha, fit_level, lwr_level, upr_level),
      by = "fecha"
    ) %>%
    mutate(
      beta_dx = beta,
      mape_level = mape_level
    )
  
  plot_store[[length(plot_store) + 1]] <- plot_all
}

# -----------------------------
# 4. Guardar outputs
# -----------------------------
# Guardar métricas y el efecto de corto-plazo
write_csv(output_metrics, file.path(out_dir, "summary_metrics_m7_dummies.csv"))
write_csv(pass_through_table, file.path(out_dir, "pass_through_m7_dummies.csv"))

# Guadar los gráficos en 3x3
plot_df_all <- if (length(plot_store) == 0) tibble() else bind_rows(plot_store)


foods_ok <- sort(unique(plot_df_all$alimento_sipsa))
n_per_page <- 9
n_pages <- ceiling(length(foods_ok) / n_per_page)
  
for (p in seq_len(n_pages)) {
    gg_food <- ggplot(plot_df_all, aes(x = fecha)) +
      geom_line(aes(y = obs_level_full), linewidth = 1) +
      geom_line(aes(y = fit_level), linetype = "dashed", linewidth = 1,
                col = "red") +
      # geom_ribbon(aes(ymin = lwr_level, ymax = upr_level), alpha = 0.25,
                 # fill = "red") +
      facet_wrap_paginate(
        ~ alimento_sipsa, scales = "free_y",
        ncol = 3, nrow = 3, page = p) +
      labs(
        title = "M7: Δlog(Pmin) ~ Δlog(Pmay) + month dummies",
        subtitle = paste("Observed vs Predicted (levels) | Page", p, "of", n_pages),
        x = NULL, y = "Precio (nivel)") +
      theme_bw(base_size = 11) +
      theme(strip.text = element_text(face = "bold", size = 9))
    
    ggsave(
      filename = file.path(plot_dir, paste0("m7_dummies_grouped_page_", sprintf("%02d", p), ".png")),
      plot = gg_food,width = 13,
      height = 9,dpi = 300
    )
  }

