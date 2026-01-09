############################################################
## M5: ECM (Error-Correction Model) + cointegration test   ##
## Cointegration: aTSA::coint.test (Engle–Granger EG test) ##
## ECM: Δlog_ipc = c + γ*ECT_{t-1} + rezagos + month FE     ##
## + Selección de rezagos (p,q) con helper (AIC/BIC)        ##
## + Forecast (Δlog) + métricas + plots agrupados (3x3)     ##
## NOTA: Se evalúa en Δlog_ipc (no se convierte a niveles)  ##
############################################################

# Directorio de trabajo
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

# Fecha
date_tag <- "121225"

# -----------------------
# 1. Librerías y rutas
# -----------------------
library(tidyverse)
library(readxl)
library(Metrics)
library(ggplot2)
library(lubridate)
library(readr)
library(ggforce)
library(aTSA)
library(writexl)

# Cargar funciones auxiliares
source(file.path(base_dir, "working-papers", "working-paper-1225", "m5", "m5-auxf-lag.R"))

# Ruta input
infile <- file.path(
  base_dir, "working-papers", "working-paper-1225",
  "mapeo ipc-sipsa", "output",
  paste0(date_tag, "_dataset_ipc_sipsa.xlsx")
)
dataset <- read_excel(infile) %>%
  mutate(fecha  = as.Date(paste(Year, Month, 1, sep = "-")))

# Ruta output
out_dir <- file.path(base_dir, "working-papers", "working-paper-1225", "m5", "output_ecm")


# -----------------------
# 2. Receptores de output
# -----------------------
food.vector <- sort(unique(dataset$alimento_sipsa))

cointegration_table <- tibble()
ecm_params_table <- tibble()
metrics_table <- tibble()
lag_selection_table <- tibble()
plot_store <- list()

# -----------------------
# 3. Bucle principal
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
  
  # Mínimo de observaciones
  if (nrow(data.food) < 36) next
  
  # División de la muestra (70/30)
  n <- nrow(data.food)
  cut <- floor(0.7 * n)
  train <- data.food[1:cut, ]
  test  <- data.food[(cut + 1):n, ]
  if (nrow(test) < 6) next
  
  # -----------------------
  # 3.1 Test de cointegración
  # -----------------------
  ct <- tryCatch(
    aTSA::coint.test(y = train$log_ipc, X = train$log_sipsa),
    error = function(e) NULL
  )
  if (is.null(ct)) next
  
  ct_mat <- as.matrix(ct)
  
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
  
  # -----------------------
  # 3.2 Selección de rezagos óptimos (helper)
  # -----------------------
  # NOTE: puedes cambiar criterion = "AIC" o "BIC"
  sel <- select_ecm_lags(train, test, max_p = 6, max_q = 6, criterion = "BIC")
  if (is.null(sel)) next
  
  # Guardar top 5 combinaciones
  lag_selection_table <- bind_rows(
    lag_selection_table,
    sel$table %>%
      slice(1:min(5, n())) %>%
      mutate(alimento_sipsa = food.x, criterion = "BIC") %>%
      select(alimento_sipsa, criterion, p, q, crit)
  )
  
  # Mejor modelo
  best <- sel$best
  ecm_model <- best$model
  p_star <- best$p
  q_star <- best$q
  
  # Recuperar datasets construidos por el helper
  comb <- best$tmp$comb
  train_ecm <- best$tmp$train_ecm
  test_ecm  <- best$tmp$test_ecm
  
  # -----------------------
  # 3.3 Guardar parámetros ECM (mejor modelo)
  # -----------------------
  cs <- summary(ecm_model)$coefficients
  
  # Coef contemporáneo: dlog_sipsa_l0
  b0_name <- "dlog_sipsa_l0"
  beta0 <- if (b0_name %in% rownames(cs)) cs[b0_name, "Estimate"] else NA_real_
  beta0_se <- if (b0_name %in% rownames(cs)) cs[b0_name, "Std. Error"] else NA_real_
  
  gamma <- if ("ect_l1" %in% rownames(cs)) cs["ect_l1", "Estimate"] else NA_real_
  gamma_se <- if ("ect_l1" %in% rownames(cs)) cs["ect_l1", "Std. Error"] else NA_real_
  
  ecm_params_table <- bind_rows(
    ecm_params_table,
    tibble(
      alimento_sipsa = food.x,
      p_star = p_star,
      q_star = q_star,
      n_train_ecm = nrow(train_ecm),
      beta0_dlog = beta0,
      beta0_se   = beta0_se,
      gamma_ect  = gamma,
      gamma_se   = gamma_se
    )
  )
  
  # -----------------------
  # 3.4 Predicción en el conjunto de validación (Δlog_ipc)
  # -----------------------
  # NOTE: dejar solo filas completas para predict()
  needed_vars <- all.vars(formula(ecm_model))
  
  test_ecm_pred <- test_ecm %>%
    drop_na(dlog_ipc) %>%                 # observado disponible para métricas
    drop_na(any_of(needed_vars))          # columnas necesarias para predict()
  
  if (nrow(test_ecm_pred) < 3) next
  
  preds_ci <- predict(
    ecm_model,
    newdata = test_ecm_pred,
    interval = "confidence",
    level = 0.95
  )
  
  test_ecm_pred <- test_ecm_pred %>%
    mutate(
      dlog_fit = as.numeric(preds_ci[, "fit"]),
      dlog_lwr = as.numeric(preds_ci[, "lwr"]),
      dlog_upr = as.numeric(preds_ci[, "upr"])
    )
  
  # -----------------------
  # 3.5 Métricas de validación (Δlog_ipc)
  # -----------------------
  rmse_dlog <- Metrics::rmse(test_ecm_pred$dlog_ipc, test_ecm_pred$dlog_fit)
  mape_dlog <- Metrics::mape(abs(test_ecm_pred$dlog_ipc), abs(test_ecm_pred$dlog_fit))
  
  metrics_table <- bind_rows(
    metrics_table,
    tibble(
      alimento_sipsa = food.x,
      n_test = nrow(test_ecm_pred),
      p_star = p_star,
      q_star = q_star,
      RMSE_dlog = rmse_dlog,
      MAPE_dlog = mape_dlog,
      coint_p_type1 = as.numeric(ct_mat[1, 3]),
      coint_p_type2 = as.numeric(ct_mat[2, 3]),
      coint_p_type3 = as.numeric(ct_mat[3, 3])
    )
  )
  
  # -----------------------
  # 3.6 Guardar datos para los gráficos
  # -----------------------
  plot_df <- comb %>%
    transmute(
      alimento_sipsa = food.x,
      fecha,
      obs_dlog_full = dlog_ipc
    ) %>%
    left_join(
      test_ecm_pred %>% select(fecha, dlog_fit, dlog_lwr, dlog_upr),
      by = "fecha"
    )
  
  plot_store[[length(plot_store) + 1]] <- plot_df
}

# -----------------------
# 4. Guardar outputs
# -----------------------

# Guardar forecast dataset
forecast_dataset = do.call(rbind, plot_store)
forecast_dataset = merge(dataset[c("fecha","alimento_sipsa",
                                   "precio_sipsa", "precio_ipc")],
                         forecast_dataset, by = c("fecha", "alimento_sipsa"))

write_csv(forecast_dataset, file.path(out_dir,
                              "m5_forecast_dataset.csv"))
write_csv(forecast_dataset, file.path("working-papers/working-paper-0125/input",
                              "m5_forecast_dataset.csv"))



# Guardar tablas en CSV
write_csv(cointegration_table, file.path(out_dir, "m5_cointegration_coint_test.csv"))
write_csv(ecm_params_table,    file.path(out_dir, "m5_ecm_parameters_bestlags.csv"))
write_csv(metrics_table,       file.path(out_dir, "m5_ecm_metrics_test_bestlags.csv"))
write_csv(lag_selection_table, file.path(out_dir, "m5_ecm_lag_selection_top5.csv"))

# Guardar tablas en XLSX
write_xlsx(
  list(
    cointegration = cointegration_table,
    ecm_params    = ecm_params_table,
    metrics_test  = metrics_table,
    lag_selection_top5 = lag_selection_table
  ),
  file.path(out_dir, "m5_ecm_outputs_bestlags.xlsx")
)

# -----------------------
# 5. Gráficos agrupados en páginas de 3x3
# -----------------------
plot_all <- if (length(plot_store) == 0) tibble() else bind_rows(plot_store)

foods_ok <- sort(unique(plot_all$alimento_sipsa))
n_per_page <- 9
n_pages <- ceiling(length(foods_ok) / n_per_page)
  
for (p in seq_len(n_pages)) {
    print(paste0(p, " de ", length(seq_len(n_pages))))
    
    gg_food <- ggplot(plot_all, aes(x = fecha)) +
      geom_line(aes(y = obs_dlog_full), linewidth = 1) +
      geom_line(aes(y = dlog_fit), linetype = "dashed", linewidth = 1, col = "black") +
      geom_ribbon(aes(ymin = dlog_lwr, ymax = dlog_upr), alpha = 0.25,
                  fill = "red", linetype = "dashed", col = "black") +
      facet_wrap_paginate(
        ~ alimento_sipsa, scales = "free_y",
        ncol = 3, nrow = 3, page = p
      ) +
      labs(
        title = "M5 (ECM): Δlog(Pmin) ~ Δlog(Pmay) + ECT_{t-1} + rezagos + month dummies",
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
  