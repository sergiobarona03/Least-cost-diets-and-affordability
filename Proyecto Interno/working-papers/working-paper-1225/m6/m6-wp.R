
########################################################
## Metodología 3: Modelo Pass-through 
## lm(log_ipc ~ log_sipsa + month dummies) 
## Train <= 70% de la muestra
## Fuente de datos: DANE - IPC y DANE-SIPSA
## Objetivo: a partir de la unión entre los datos del
## IPC y SIPSA, se calcula una regresión lineal controlando
## por estacionalidad mensual
########################################################

# Directorio de trabajo
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

# Fechas de estimaciones
date_tag <- "121225"

# -----------------------
# 1. Cargar librerías y definir directorios
# -----------------------
library(tidyverse)
library(readxl)
library(Metrics)
library(ggplot2)
library(lubridate)
library(readr)
library(ggforce)  

# Ruta para input
infile <- file.path(
  base_dir,"working-papers", "working-paper-1225",
  "mapeo ipc-sipsa", "output",paste0(date_tag, "_dataset_ipc_sipsa.xlsx")
)
dataset <- read_excel(infile)

# Ruta para output
out_dir <- file.path(base_dir, "working-papers",
                     "working-paper-1225", "m6", 
                     "output_dummies")

# -----------------------
# 2. Definir receptores del output
# -----------------------
food.vector <- sort(unique(dataset$alimento_sipsa))
output_metrics <- tibble()
margen_table <- tibble()
plot_store <- list()   

# -----------------------
# 3. Bucle principal: estimaciones lm
# -----------------------
for (i in seq_along(food.vector)) {
  
  food.x <- food.vector[i]
  message("Procesando: ", food.x)
  
  # Definición de variables
  data.food <- dataset %>%
    filter(alimento_sipsa == food.x) %>%
    drop_na(precio_ipc, precio_sipsa) %>%
    mutate(
      log_ipc   = log(precio_ipc),
      log_sipsa = log(precio_sipsa),
      fecha     = as.Date(paste(Year, Month, 1, sep = "-")),
      mes       = factor(month(fecha), levels = 1:12, labels = month.abb)
    ) %>% arrange(fecha)
  
  if (nrow(data.food) < 24) next
  
  # División de la muestra (70/30)
  n <- nrow(data.food)
  cut <- floor(0.7 * n)
  train <- data.food[1:cut, ]
  test  <- data.food[(cut + 1):n, ]
  
  # Estimación de modelo log-log controlando por estacionalidad
  model <- lm(log_ipc ~ log_sipsa + mes, data = train)
  
  # Predicción sobre los datos de validación
  preds <- predict(model, newdata = test, interval = "confidence", 
                   level = 0.95)
  test <- test %>%
    mutate(fit = preds[, "fit"],lwr = preds[, "lwr"],
      upr = preds[, "upr"])
  
  # Métricas
  rmse_val <- rmse(exp(test$log_ipc), exp(test$fit))
  mape_val <- mape(exp(test$log_ipc), exp(test$fit))*100
  
  output_metrics <- bind_rows(
    output_metrics,
    tibble(
      alimento_sipsa = food.x,
      RMSE_level = rmse_val,
      MAPE_level = mape_val,
      n_total = n,
      n_test  = nrow(test)
    )
  )
  
  # Margen comercialización (exp(beta))
  coef_summary <- summary(model)$coefficients
  beta    <- coef_summary["log_sipsa", "Estimate"]
  se_beta <- coef_summary["log_sipsa", "Std. Error"]
  
  # Margen de comercialización con 95% IC
  margen_table <- bind_rows(
    margen_table,
    tibble(
      alimento_sipsa = food.x,
      margen_exp = exp(beta),
      margen_lo  = exp(beta - 1.96 * se_beta),
      margen_hi  = exp(beta + 1.96 * se_beta)
    )
  )
  
  # Preparar datos para el gráfico
  plot_test <- test %>%
    transmute(
      alimento_sipsa = food.x,
      fecha,
      sample = "test",
      obs_level = exp(log_ipc),
      fit_level = exp(fit),
      lwr_level = exp(lwr),
      upr_level = exp(upr),
      mape_food = mape_val
    )
  
  plot_train <- train %>%
    transmute(
      alimento_sipsa = food.x,
      fecha,
      sample = "train",
      obs_level = exp(log_ipc),
      fit_level = NA_real_,
      lwr_level = NA_real_,
      upr_level = NA_real_,
      mape_food = mape_val
    )
  
  plot_store[[length(plot_store) + 1]] <- bind_rows(plot_train, plot_test)
}

# -----------------------
# 4. Guardar outputs
# -----------------------

# Guardar dataset
m6_forecast_df = do.call(rbind, plot_store)

write_csv(m6_forecast_df, file.path(out_dir,
                                      "m6_forecast_dataset.csv"))
write_csv(m6_forecast_df, file.path("working-papers/working-paper-0125/input",
                                      "m6_forecast_dataset.csv"))

# Guardar métricas y márgenes
write_csv(output_metrics, file.path(out_dir, "summary_metrics_m6_dummies.csv"))
write_csv(margen_table,  file.path(out_dir, "margen_comercializacion_m6_dummies.csv"))

# Guardar en .xlsx
write_xlsx(
  list(
    summary_metrics = output_metrics,
    margen_table    = margen_table
  ),
  file.path(out_dir, "m6_outputs.xlsx")
)

# -----------------------
# 5. Guardar los gráficos agrupados en 3x3
# -----------------------
plot_all_foods <- if (length(plot_store) == 0) tibble() else bind_rows(plot_store)

foods_ok <- sort(unique(plot_all_foods$alimento_sipsa))
n_per_page <- 9
n_pages <- ceiling(length(foods_ok) / n_per_page)
  
for (p in seq_len(n_pages)) {
    
    gg_food <- ggplot(plot_all_foods, aes(x = fecha)) +
      geom_ribbon(
        aes(ymin = lwr_level, ymax = upr_level),
        alpha = 0.20, fill = "red"
      ) +
      geom_line(aes(y = obs_level), linewidth = 0.7) +
      geom_line(aes(y = fit_level), linetype = "dashed", linewidth = 0.7) +
      facet_wrap_paginate(
        ~ alimento_sipsa,
        scales = "free_y",
        ncol = 3, nrow = 3,
        page = p
      ) + labs(
        title = "M6: Observed vs Predicted Retail Price (levels) — Month dummies",
        subtitle = paste("95% CI in test | Page", p, "of", n_pages),
        x = NULL,
        y = "Precio (nivel)"
      ) +theme_bw(base_size = 11) +
      theme(strip.text = element_text(face = "bold", size = 9))
    
    ggsave(
      filename = file.path(out_dir, paste0("m6_dummies_grouped_page_", sprintf("%02d", p), ".png")),
      plot = gg_food,width = 13,
      height = 9,dpi = 300)
  }
