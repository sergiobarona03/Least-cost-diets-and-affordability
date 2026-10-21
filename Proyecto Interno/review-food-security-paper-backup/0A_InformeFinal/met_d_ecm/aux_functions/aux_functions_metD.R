########################################################
## aux_functions_metD.R
## Funciones auxiliares para el ECM estándar (Metodología D)
## Adaptado de ecm_aux.R: build_ecm_lagged_full / fit_ecm_lag_k_full /
## select_ecm_lag_k_full, con restricción de Enders (k>=1, p=q=k, sin Δx_t)
########################################################

library(lubridate)

# -----------------------------
# Relación de largo plazo + ECT + rezagos, sobre un slice ya filtrado
# (data debe tener columnas: fecha, log_ipc, log_sipsa, mes)
# -----------------------------
build_ecm_lagged_full <- function(data, k = 1) {
  
  if (k < 1) stop("k must be >= 1")
  
  lr_model <- lm(log_ipc ~ log_sipsa, data = data)
  
  comb <- data %>%
    arrange(fecha) %>%
    mutate(
      lr_fit = predict(lr_model, newdata = .),
      ect    = log_ipc - lr_fit,
      ect_l1 = lag(ect),
      dlog_ipc   = log_ipc   - lag(log_ipc),
      dlog_sipsa = log_sipsa - lag(log_sipsa)
    )
  
  for (i in 1:k) {
    comb[[paste0("dlog_ipc_l", i)]] <- lag(comb$dlog_ipc, i)
  }
  for (j in 1:k) {
    comb[[paste0("dlog_sipsa_l", j)]] <- lag(comb$dlog_sipsa, j)
  }
  
  ecm_df <- comb %>% drop_na(dlog_ipc, ect_l1, mes)
  
  list(lr_model = lr_model, comb = comb, ecm_df = ecm_df)
}

# -----------------------------
# Ajusta el ECM para un k específico, devuelve el modelo y el criterio de info
# -----------------------------
fit_ecm_lag_k_full <- function(data, k = 1,
                               criterion = c("AIC", "BIC"),
                               min_ecm_obs = 24) {
  
  criterion <- match.arg(criterion)
  if (k < 1) return(NULL)
  
  tmp <- build_ecm_lagged_full(data, k = k)
  d <- tmp$ecm_df
  if (nrow(d) < min_ecm_obs) return(NULL)
  
  rhs <- c("ect_l1")
  rhs <- c(rhs, paste0("dlog_ipc_l", 1:k))
  rhs <- c(rhs, paste0("dlog_sipsa_l", 1:k))  # SIN l0 (sin Δx_t contemporáneo)
  rhs <- c(rhs, "mes")
  
  fml <- as.formula(paste("dlog_ipc ~", paste(rhs, collapse = " + ")))
  
  m <- tryCatch(lm(fml, data = d), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  
  crit <- if (criterion == "AIC") AIC(m) else BIC(m)
  
  list(model = m, tmp = tmp, k = k, criterion = criterion, value = crit)
}

# -----------------------------
# Selecciona el k óptimo (1..max_k) por AIC/BIC, con restricción de Enders
# (mismo número de rezagos para Δy y Δx)
# -----------------------------
select_ecm_lag_k_full <- function(data, max_k = 6,
                                  criterion = c("AIC", "BIC"),
                                  min_ecm_obs = 24) {
  
  criterion <- match.arg(criterion)
  
  ks <- 1:max_k
  results <- vector("list", length(ks))
  
  for (idx in seq_along(ks)) {
    k <- ks[idx]
    results[[idx]] <- tryCatch(
      fit_ecm_lag_k_full(data, k = k, criterion = criterion, min_ecm_obs = min_ecm_obs),
      error = function(e) NULL
    )
  }
  
  results <- results[!sapply(results, is.null)]
  if (length(results) == 0) return(NULL)
  
  tab <- tibble(
    k    = sapply(results, `[[`, "k"),
    crit = sapply(results, `[[`, "value")
  ) %>% arrange(crit)
  
  best <- results[[which.min(sapply(results, `[[`, "value"))]]
  
  list(table = tab, best = best)
}

# -----------------------------
# Predicción dinámica/recursiva un paso adelante, fuera de muestra
#
# Dado un modelo ECM ajustado (con k rezagos) y la relación de largo plazo,
# predice ln(P_min) hacia adelante mes a mes, usando los propios valores
# predichos (no observados) para construir los rezagos de Δy y el ECT
# una vez que se sale del rango con datos reales.
#
# df_eval debe tener: fecha, log_sipsa, mes (factor), y opcionalmente
# log_ipc observado (para comparar en la ventana de validación)
#
# log_ipc_anchor / log_sipsa_anchor / fecha_anchor: el último punto
# con datos REALES antes del período de evaluación (para inicializar
# los rezagos correctamente)
# -----------------------------
predict_ecm_dynamic <- function(ecm_fit, lr_model, df_history, df_eval, k) {
  
  m <- ecm_fit$model
  
  # Varianza residual del ECM en un paso (escala log), usada para propagar
  # la incertidumbre acumulada a lo largo de la cadena recursiva:
  # Var(log_ipc_hat_t) = Var(log_ipc_hat_{t-1}) + sigma2_step
  sigma2_step <- sum(resid(m)^2) / df.residual(m)
  
  full_dates <- bind_rows(
    df_history %>% select(fecha, log_sipsa, log_ipc, mes),
    df_eval    %>% select(fecha, log_sipsa, log_ipc, mes)
  ) %>%
    distinct(fecha, .keep_all = TRUE) %>%
    arrange(fecha)
  
  full_dates$log_ipc_hat <- NA_real_
  full_dates$var_log_hat <- NA_real_   # varianza acumulada, 0 en el tramo observado
  
  hist_dates <- df_history$fecha
  full_dates$log_ipc_hat[full_dates$fecha %in% hist_dates] <-
    full_dates$log_ipc[full_dates$fecha %in% hist_dates]
  full_dates$var_log_hat[full_dates$fecha %in% hist_dates] <- 0
  
  full_dates <- full_dates %>%
    mutate(dlog_sipsa_real = log_sipsa - lag(log_sipsa, default = NA_real_))
  
  # Búsqueda por FECHA real (no por posición de índice), para no asumir
  # continuidad mensual sin huecos entre TRAIN_END y EVAL_START.
  get_row_at <- function(target_date) {
    idx <- which(full_dates$fecha == target_date)
    if (length(idx) != 1) return(NA_integer_)
    idx
  }
  
  eval_dates_sorted <- sort(df_eval$fecha)
  
  for (target_date in eval_dates_sorted) {
    
    target_date <- as.Date(target_date, origin = "1970-01-01")
    idx <- get_row_at(target_date)
    if (is.na(idx)) next
    
    # Rezagos requieren las fechas EXACTAS target_date - lag_i meses
    lag_dates <- target_date %m-% months(1:k)
    
    dlog_ipc_lags <- sapply(seq_along(lag_dates), function(li) {
      d_j  <- lag_dates[li]
      d_j1 <- d_j %m-% months(1)
      j  <- get_row_at(d_j)
      j1 <- get_row_at(d_j1)
      if (is.na(j) || is.na(j1)) return(NA_real_)
      full_dates$log_ipc_hat[j] - full_dates$log_ipc_hat[j1]
    })
    
    dlog_sipsa_lags <- sapply(seq_along(lag_dates), function(li) {
      d_j <- lag_dates[li]
      j <- get_row_at(d_j)
      if (is.na(j)) return(NA_real_)
      full_dates$dlog_sipsa_real[j]
    })
    
    if (any(is.na(dlog_ipc_lags)) || any(is.na(dlog_sipsa_lags))) next
    
    # ECT_{t-1}: requiere la fecha exacta target_date - 1 mes
    date_l1 <- target_date %m-% months(1)
    j1 <- get_row_at(date_l1)
    if (is.na(j1) || is.na(full_dates$log_ipc_hat[j1])) next
    
    lr_fit_j1 <- predict(lr_model, newdata = tibble(log_sipsa = full_dates$log_sipsa[j1]))
    ect_l1 <- full_dates$log_ipc_hat[j1] - lr_fit_j1
    
    newdata <- as_tibble(setNames(as.list(dlog_ipc_lags),   paste0("dlog_ipc_l",   1:k)))
    newdata <- bind_cols(newdata,
                         as_tibble(setNames(as.list(dlog_sipsa_lags), paste0("dlog_sipsa_l", 1:k))))
    newdata$ect_l1 <- ect_l1
    newdata$mes    <- full_dates$mes[idx]
    
    dlog_pred <- tryCatch(
      suppressWarnings(predict(m, newdata = newdata)),
      error = function(e) NA_real_
    )
    
    full_dates$log_ipc_hat[idx] <- full_dates$log_ipc_hat[j1] + dlog_pred
    # La varianza se acumula: cada paso recursivo añade su propia
    # incertidumbre a la incertidumbre heredada del paso anterior
    full_dates$var_log_hat[idx] <- full_dates$var_log_hat[j1] + sigma2_step
  }
  
  full_dates %>%
    filter(fecha %in% df_eval$fecha) %>%
    transmute(
      fecha,
      price_pred     = exp(log_ipc_hat),
      price_pred_lwr = exp(log_ipc_hat - 1.96 * sqrt(var_log_hat)),
      price_pred_upr = exp(log_ipc_hat + 1.96 * sqrt(var_log_hat)),
      price_obs      = exp(log_ipc)
    )
}