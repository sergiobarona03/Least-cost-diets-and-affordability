
############################################################
## M5: ECM (Error-Correction Model) + cointegration test   ##
## Cointegration: aTSA::coint.test (Engle–Granger EG test) ##
## ECM: Δlog_ipc = c + γ*ECT_{t-1} + β*Δlog_sipsa + month FE
## + Forecast (Δlog) + métricas + plots agrupados (3x3)    ##
## NOTA: Se evalúa en la variable dependiente original:    ##
##       Δlog_ipc (no se convierte a niveles para IC)      ##
############################################################

# Directorio de trabajo
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

# Fecha
date_tag <- "121225"

# -----------------------
# 1. Librerías y rutas
# -----------------------

# Cargar librerías
library(tidyverse)
library(readxl)
library(Metrics)
library(ggplot2)
library(lubridate)
library(readr)
library(ggforce)   
library(aTSA)      
library(writexl)

# Ruta input
infile <- file.path(
  base_dir,  "working-papers", "working-paper-1225",
  "mapeo ipc-sipsa", "output",paste0(date_tag, "_dataset_ipc_sipsa.xlsx")
)
dataset <- read_excel(infile)

# Ruta output
out_dir <- file.path(base_dir, "working-papers", "working-paper-1225", "m5", "output_ecm")

# -----------------------
# 2. Funciones auxiliares
# -----------------------

# Safe name (para archivos)
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# -----------------------
# 3. Receptores de output
# -----------------------
food.vector <- sort(unique(dataset$alimento_sipsa))
cointegration_table <- tibble()  
ecm_params_table <- tibble()      
metrics_table <- tibble()         
plot_store <- list()              

# -----------------------
# 4. Bucle principal
# -----------------------
for (i in seq_along(food.vector)) {
  
  food.x <- food.vector[i]
  message("Procesando: ", food.x)
  
  # Preparación de datos
  data.food <- dataset %>%
    filter(alimento_sipsa == food.x) %>%
    drop_na(precio_ipc, precio_sipsa) %>%
    mutate(
      fecha     = as.Date(paste(Year, Month, 1, sep = "-")),
      log_ipc   = log(precio_ipc),
      log_sipsa = log(precio_sipsa),
      mes       = factor(month(fecha), levels = 1:12, labels = month.abb)
    ) %>% arrange(fecha)
  
  # Mínimo de observaciones para cointegration + ECM
  if (nrow(data.food) < 36) next
  
  # División de la muestra (70/30)
  n <- nrow(data.food)
  cut <- floor(0.7 * n)
  train <- data.food[1:cut, ]
  test  <- data.food[(cut + 1):n, ]
  if (nrow(test) < 6) next
  
  # Test de cointegración
  ct <- tryCatch(
    aTSA::coint.test( y = train$log_ipc, X = train$log_sipsa),
    error = function(e) NULL)
  
  # Si falla el test, saltar alimento
  if (is.null(ct)) next
  
  # Matriz: type_i{i = 1, 2, 3} x (lag, statistic, p-value)
  ct_mat <- as.matrix(ct)
  
  # Guardar estadísticos
  cointegration_table <- bind_rows(
    cointegration_table,
    tibble(
      alimento_sipsa = food.x,
      n_train = nrow(train),
      EG_type1 = as.numeric(ct_mat[1, 2]),
      p_type1  = as.numeric(ct_mat[1, 3]),
      EG_type2 = as.numeric(ct_mat[2, 2]),
      p_type2  = as.numeric(ct_mat[2, 3]),
      EG_type3 = as.numeric(ct_mat[3, 2]),
      p_type3  = as.numeric(ct_mat[3, 3])
    )
  )
  
  # Estimación de la ecuación de largo plazo
  lr_model <- lm(log_ipc ~ log_sipsa + mes, data = train)
  
  # Dataset combinado (train y test)
  comb <- bind_rows(
    train %>% mutate(split = "train"),
    test  %>% mutate(split = "test")
  ) %>%
    arrange(fecha) %>%
    mutate(
      # Ajuste de largo plazo
      lr_fit = predict(lr_model, newdata = .),
      # Error de equilibrio (residuo)
      ect    = log_ipc - lr_fit,
      # Rezago del ECT
      ect_l1 = lag(ect),
      # Primeras diferencias en logs
      dlog_sipsa = log_sipsa - lag(log_sipsa),
      dlog_ipc   = log_ipc   - lag(log_ipc)
    )
  
  # Submuestra ECM (train)
  train_ecm <- comb %>%
    filter(split == "train") %>%
    drop_na(dlog_ipc, dlog_sipsa, ect_l1)
  
  # Mínimo de obs para estimar ECM
  if (nrow(train_ecm) < 18) next
  
  # Estimación del ECM
  ecm_model <- lm(dlog_ipc ~ dlog_sipsa + ect_l1 + mes, data = train_ecm)
  
  # Extraer parámetros clave
  cs <- summary(ecm_model)$coefficients
  beta  <- cs["dlog_sipsa", "Estimate"]
  gamma <- cs["ect_l1", "Estimate"]
  
  # Guardar tabla de parámetros ECM
  ecm_params_table <- bind_rows(
    ecm_params_table,
    tibble(
      alimento_sipsa = food.x,
      n_train_ecm = nrow(train_ecm),
      beta_dlog = beta,
      beta_se   = cs["dlog_sipsa", "Std. Error"],
      gamma_ect = gamma,
      gamma_se  = cs["ect_l1", "Std. Error"]
    )
  )
  
  # Predicción en el conjunto de datos de validación
  test_ecm <- comb %>%
    filter(split == "test") %>%
    drop_na(dlog_sipsa, ect_l1)
  
  if (nrow(test_ecm) < 3) next
  
  # Predicción de Δlog_ipc con IC 95% 
  preds_ci <- predict(
    ecm_model,
    newdata = test_ecm,
    interval = "confidence",
    level = 0.95
  )
  
  # Guardar fit + bandas 
  test_ecm <- test_ecm %>%
    mutate(
      dlog_fit = as.numeric(preds_ci[, "fit"]),
      dlog_lwr = as.numeric(preds_ci[, "lwr"]),
      dlog_upr = as.numeric(preds_ci[, "upr"])
    )
  
  # Métricas de validación
  rmse_dlog <- Metrics::rmse(test_ecm$dlog_ipc, test_ecm$dlog_fit)
  mape_dlog <- Metrics::mape(abs(test_ecm$dlog_ipc), abs(test_ecm$dlog_fit))
  
  metrics_table <- bind_rows(
    metrics_table,
    tibble(
      alimento_sipsa = food.x,
      n_test = nrow(test_ecm),
      RMSE_dlog = rmse_dlog,
      MAPE_dlog = mape_dlog,
      coint_p_type1 = as.numeric(ct_mat[1, 3]),
      coint_p_type2 = as.numeric(ct_mat[2, 3]),
      coint_p_type3 = as.numeric(ct_mat[3, 3])
    )
  )
  
  # Guardar datos para los gráficos
  plot_df <- comb %>%
    transmute(
      alimento_sipsa = food.x,
      fecha,
      obs_dlog_full = dlog_ipc
    ) %>%
    left_join(
      test_ecm %>% select(fecha, dlog_fit, dlog_lwr, dlog_upr),
      by = "fecha"
    )
  
  plot_store[[length(plot_store) + 1]] <- plot_df
}

# -----------------------
# 5. Guardar outputs
# -----------------------

# Guardar tablas en CSV
write_csv(cointegration_table, file.path(out_dir, "m5_cointegration_coint_test.csv"))
write_csv(ecm_params_table,    file.path(out_dir, "m5_ecm_parameters.csv"))
write_csv(metrics_table,       file.path(out_dir, "m5_ecm_metrics_test.csv"))

# Guardar tablas en XLSX
write_xlsx(
  list(cointegration = cointegration_table,ecm_params    = ecm_params_table,
    metrics_test  = metrics_table),
  file.path(out_dir, "m5_ecm_outputs.xlsx"))

# Gráficos agrupados en páginas de 3x3
plot_all <- if (length(plot_store) == 0) tibble() else bind_rows(plot_store)

foods_ok <- sort(unique(plot_all$alimento_sipsa))
n_per_page <- 9
n_pages <- ceiling(length(foods_ok) / n_per_page)
  
for (p in seq_len(n_pages)) {
    print(paste0(p, " de ", length(seq_len(n_pages))))
    gg_food <- ggplot(plot_all, aes(x = fecha)) +
      geom_line(aes(y = obs_dlog_full), linewidth = 1) +
      geom_line(aes(y = dlog_fit), linetype = "dashed", linewidth = 1, 
                col = "black") +
      geom_ribbon(aes(ymin = dlog_lwr, ymax = dlog_upr), alpha = 0.25,
                  fill = "red", linetype = "dashed", col = "black") +
      facet_wrap_paginate(
        ~ alimento_sipsa, scales = "free_y",
        ncol = 3, nrow = 3, page = p
      ) +
      labs(
        title = "M5 (ECM): Δlog(Pmin) ~ Δlog(Pmay) + ECT_{t-1} + month dummies",
        subtitle = paste("Observed vs Predicted (Δlog) | Page", p, "of", n_pages),
        x = NULL, y = "Δlog(Precio)"
      ) +
      theme_bw(base_size = 11) +
      theme(strip.text = element_text(face = "bold", size = 9))
    
    ggsave(
      filename = file.path(out_dir, paste0("m5_ecm_grouped_page_", sprintf("%02d", p), ".png")),
      plot = gg_food, width = 13,
      height = 9, dpi = 300
    )
  }
  
