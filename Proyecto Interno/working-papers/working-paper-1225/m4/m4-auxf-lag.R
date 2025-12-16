############################################################
## M4-AUXF-LAG: Funciones auxiliares para                 ##
## - ajustar ECM asimétrico (apt::ecmAsyFit, MTAR)         ##
## - forecast genuino sobre Δlog_ipc (dy)                  ##
## - selección de rezago óptimo (lag) por RMSE en test     ##
## NOTA: apt::ecmAsyFit NO tiene logLik() => no AIC/BIC    ##
############################################################

library(tidyverse)
library(apt)
library(janitor)
library(Metrics)

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
# Ajustar M4 (apt::ecmAsyFit) para un rezago L
# -----------------------------
fit_m4_asy <- function(train, lag = 1, model = "mtar", thresh = 0, split = TRUE) {
  
  ts_y <- ts(train$log_ipc, start = c(train$Year[1], train$Month[1]), frequency = 12)
  ts_x <- ts(train$log_sipsa, start = c(train$Year[1], train$Month[1]), frequency = 12)
  
  # NOTE: workaround por conflicto apt vs dplyr (mantengo tu estilo)
  if ("package:dplyr" %in% search()) detach("package:dplyr", unload = TRUE)
  
  fit_asy <- apt::ecmAsyFit(
    y      = ts_y,
    x      = ts_x,
    lag    = lag,
    model  = model,
    thresh = thresh,
    split  = split
  )
  
  if (!("package:dplyr" %in% search())) library(dplyr)
  
  fit_asy
}

# -----------------------------
# Extraer coeficientes desde summary(fit_asy)
# -----------------------------
extract_m4_coefs <- function(fit_asy) {
  
  coef_table <- summary(fit_asy) %>%
    janitor::clean_names() %>%
    mutate(
      dep_var = stringr::str_trim(dep_var),
      dep_var = stringr::str_replace_all(dep_var, "[\\|\\-]", ""),
      dep_var = stringr::str_trim(dep_var),
      dep_var = dplyr::na_if(dep_var, "")
    ) %>%
    tidyr::fill(dep_var)
  
  get_coef <- function(dep, name) {
    out <- coef_table %>%
      dplyr::filter(dep_var == dep, ind_var == name) %>%
      dplyr::pull(estimate)
    if (length(out) == 0) return(NA_real_)
    out[1]
  }
  
  dep <- "diff.ts_y.t_0"
  
  list(
    b0      = get_coef(dep, "(Intercept)"),
    bDypos  = get_coef(dep, "X.diff.ts_y.t_1.pos"),
    bDyneg  = get_coef(dep, "X.diff.ts_y.t_1.neg"),
    bDpos   = get_coef(dep, "X.diff.ts_x.t_1.pos"),
    bDneg   = get_coef(dep, "X.diff.ts_x.t_1.neg"),
    bECTpos = get_coef(dep, "X.ECT.t_1.pos"),
    bECTneg = get_coef(dep, "X.ECT.t_1.neg")
  )
}

# -----------------------------
# Forecast genuino sobre dy = Δlog_ipc
# - recursivo: usa dy_hat(t-1) y yhat_log(t-1) EN TEST
# - NO se reconstruye a niveles para IC
# -----------------------------
forecast_m4_dy <- function(full_data, train_end_idx, test_idx, coefs) {
  
  b0      <- coefs$b0
  bDypos  <- coefs$bDypos
  bDyneg  <- coefs$bDyneg
  bDpos   <- coefs$bDpos
  bDneg   <- coefs$bDneg
  bECTpos <- coefs$bECTpos
  bECTneg <- coefs$bECTneg
  
  # -----------------------------
  # 1) Inicialización robusta
  # -----------------------------
  # dy[1] siempre es NA, así que train_end_idx nunca puede ser 1
  if (train_end_idx < 2) train_end_idx <- 2
  
  idx0 <- train_end_idx
  while (idx0 >= 2 && is.na(full_data$dy[idx0])) idx0 <- idx0 - 1
  if (idx0 < 2) return(NULL)
  
  y0_log <- full_data$log_ipc[idx0]
  dy0    <- full_data$dy[idx0]
  x0_log <- full_data$log_sipsa[idx0]
  if (any(is.na(c(y0_log, dy0, x0_log)))) return(NULL)
  
  # Estado recursivo
  y_prev_hat  <- y0_log
  dy_prev_hat <- dy0
  lz_prev     <- (y0_log - x0_log)   # ECT proxy del tipo (y-x)
  
  out <- tibble::tibble(
    fecha  = full_data$fecha[test_idx],
    dy_obs = full_data$dy[test_idx],
    dy_hat = NA_real_
  )
  
  # -----------------------------
  # 2) Recursión en TEST
  # -----------------------------
  for (k in seq_along(test_idx)) {
    
    idx <- test_idx[k]
    
    dx_p <- full_data$dx_pos[idx]
    dx_n <- full_data$dx_neg[idx]
    if (any(is.na(c(dx_p, dx_n, lz_prev)))) next
    
    # Ajuste asimétrico por ECT (según signo de lz_{t-1})
    adj_term <- ifelse(lz_prev < 0, bECTneg * lz_prev, bECTpos * lz_prev)
    
    # Asimetría por signo del dy rezagado (esto sí activa bDypos vs bDyneg)
    dy_term <- ifelse(dy_prev_hat >= 0, bDypos * dy_prev_hat, bDyneg * dy_prev_hat)
    
    dx_term <- bDpos * dx_p + bDneg * dx_n
    
    dy_hat <- b0 + dy_term + dx_term + adj_term
    
    out$dy_hat[k] <- dy_hat
    
    # Actualizar estado (forecast genuino)
    y_curr_hat  <- y_prev_hat + dy_hat
    x_curr_log  <- full_data$log_sipsa[idx]
    if (is.na(x_curr_log)) break
    
    lz_curr <- y_curr_hat - x_curr_log
    
    y_prev_hat  <- y_curr_hat
    dy_prev_hat <- dy_hat
    lz_prev     <- lz_curr
  }
  
  out
}

# -----------------------------
# Selección de rezago óptimo (lag) para M4
# - criterio: menor RMSE en TEST sobre dy
# -----------------------------
select_m4_lag <- function(data.food, train_idx, test_idx, max_lag = 6) {
  
  tab <- tibble::tibble(
    lag = integer(),
    RMSE_test_dy = numeric(),
    n_test = integer()
  )
  
  best_obj <- NULL
  
  # Construir full_data (una sola vez)
  full_data <- data.food %>%
    mutate(
      dy     = c(NA, diff(log_ipc)),
      dx     = c(NA, diff(log_sipsa)),
      dx_pos = pmax(dx, 0),
      dx_neg = pmin(dx, 0)
    )
  
  # train_end_idx (último índice del train en el panel completo)
  train_end_idx <- max(train_idx)
  
  for (L in 1:max_lag) {
    
    fit_asy <- tryCatch(
      fit_m4_asy(train = data.food[train_idx, ], lag = L),
      error = function(e) NULL
    )
    if (is.null(fit_asy)) next
    
    coefs <- tryCatch(extract_m4_coefs(fit_asy), error = function(e) NULL)
    if (is.null(coefs)) next
    
    fc <- tryCatch(
      forecast_m4_dy(full_data, train_end_idx = train_end_idx, test_idx = test_idx, coefs = coefs),
      error = function(e) NULL
    )
    if (is.null(fc)) next
    
    fc_ok <- fc %>% tidyr::drop_na(dy_obs, dy_hat)
    if (nrow(fc_ok) < 3) next
    
    rmse_test <- Metrics::rmse(fc_ok$dy_obs, fc_ok$dy_hat)
    
    tab <- dplyr::bind_rows(
      tab,
      tibble::tibble(
        lag = L,
        RMSE_test_dy = rmse_test,
        n_test = nrow(fc_ok)
      )
    )
    
    if (is.null(best_obj) || rmse_test < best_obj$RMSE_test_dy) {
      best_obj <- list(
        lag = L,
        RMSE_test_dy = rmse_test,
        fit_asy = fit_asy,
        coefs = coefs
      )
    }
  }
  
  if (nrow(tab) == 0 || is.null(best_obj)) return(NULL)
  
  tab <- tab %>% dplyr::arrange(RMSE_test_dy)
  
  list(
    table = tab,
    best  = best_obj
  )
}
