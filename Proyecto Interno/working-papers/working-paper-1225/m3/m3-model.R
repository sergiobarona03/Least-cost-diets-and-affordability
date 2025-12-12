############################################################
## Metodología III (ECM Asimétrico) - Estimación + Predicción
## Ciudad fija (cod_mun) | Por alimento_sipsa
## Output: predictions (test sample) + model summary
############################################################

library(tidyverse)
library(lubridate)
library(readxl)
library(readr)

# -----------------------
# Settings
# -----------------------
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

date_tag  <- "121225"
city_code <- "76001"   # <- CAMBIA AQUÍ si usas otra ciudad
use_logs  <- TRUE      # TRUE recomendado (consistente con SARIMAX)

# Lags (mensual). Ajusta si quieres.
L1 <- 6   # lags de Δ+/- Wholesale (incluye i=0)
L2 <- 6   # lags de Δ+/- Retail (incluye i=1)

infile <- paste0(
  "working-papers\\working-paper-1225\\mapeo ipc-sipsa\\output\\",
  date_tag, "_dataset_ipc_sipsa.xlsx"
)

out_dir <- "working-papers\\working-paper-1225\\m3\\output\\"

# -----------------------
# Helpers
# -----------------------
pos <- function(x) pmax(x, 0)
neg <- function(x) pmin(x, 0)

# Construye el dataset ECM (train) dado R y W (series en niveles o logs)
build_ecm_df <- function(R, W, L1, L2, gamma0, gamma1) {
  # R, W: numeric vectors length N (aligned)
  N <- length(R)
  dR <- c(NA, diff(R))
  dW <- c(NA, diff(W))
  z  <- R - (gamma0 + gamma1 * W)
  
  t_start <- max(L1, L2) + 2  # para que existan diferencias y rezagos
  
  rows <- vector("list", length = N - t_start + 1)
  idx <- 1
  
  for (t in t_start:N) {
    row <- list(dR = dR[t])
    
    # ΔW lags i = 0..L1
    for (i in 0:L1) {
      val <- dW[t - i]
      row[[paste0("dW_pos_L", i)]] <- pos(val)
      row[[paste0("dW_neg_L", i)]] <- neg(val)
    }
    
    # ΔR lags i = 1..L2
    for (i in 1:L2) {
      val <- dR[t - i]
      row[[paste0("dR_pos_L", i)]] <- pos(val)
      row[[paste0("dR_neg_L", i)]] <- neg(val)
    }
    
    # Error correction term at t-1
    row[["z_pos_L1"]] <- pos(z[t - 1])
    row[["z_neg_L1"]] <- neg(z[t - 1])
    
    rows[[idx]] <- row
    idx <- idx + 1
  }
  
  bind_rows(rows)
}

# Pronóstico dinámico recursivo con ECM en diferencias
forecast_ecm <- function(R_train, W_full, dates_full, cutoff,
                         L1, L2, gamma0, gamma1, coef_vec, sigma_eps) {
  
  # R_hat guarda serie completa: train observado + test predicho
  N <- length(W_full)
  R_hat <- rep(NA_real_, N)
  R_hat[1:cutoff] <- R_train
  
  # para bandas (en R, no en ΔR): var acumulada ~ h*sigma^2
  alpha <- 0.05
  zcrit <- qnorm(1 - alpha/2)
  
  pred_mean <- rep(NA_real_, N)
  pred_lo95 <- rep(NA_real_, N)
  pred_hi95 <- rep(NA_real_, N)
  
  # helper diff aligned
  d <- function(x) c(NA, diff(x))
  
  for (t in (cutoff + 1):N) {
    dW_full <- d(W_full)
    dR_hat  <- d(R_hat)
    
    # construir X_t en el mismo orden que en lm
    x <- c()
    
    # Intercept (si existe)
    if ("(Intercept)" %in% names(coef_vec)) x <- c(x, 1)
    
    # ΔW lags i=0..L1
    for (i in 0:L1) {
      val <- dW_full[t - i]
      x <- c(x,
             pos(val),  # dW_pos_Li
             neg(val))  # dW_neg_Li
    }
    
    # ΔR lags i=1..L2
    for (i in 1:L2) {
      val <- dR_hat[t - i]
      x <- c(x,
             pos(val),  # dR_pos_Li
             neg(val))  # dR_neg_Li
    }
    
    # z_{t-1}
    z_tm1 <- R_hat[t - 1] - (gamma0 + gamma1 * W_full[t - 1])
    x <- c(x, pos(z_tm1), neg(z_tm1))
    
    # asegurar nombres consistentes
    # (x debe alinearse con coef_vec excluyendo posibles alias)
    b <- coef_vec
    # orden esperado de coeficientes (como se crea el df)
    expected_names <- c()
    
    if ("(Intercept)" %in% names(b)) expected_names <- c(expected_names, "(Intercept)")
    
    for (i in 0:L1) {
      expected_names <- c(expected_names,
                          paste0("dW_pos_L", i),
                          paste0("dW_neg_L", i))
    }
    for (i in 1:L2) {
      expected_names <- c(expected_names,
                          paste0("dR_pos_L", i),
                          paste0("dR_neg_L", i))
    }
    expected_names <- c(expected_names, "z_pos_L1", "z_neg_L1")
    
    # construir vector x nombrado
    x_named <- setNames(x, expected_names)
    
    # asegurar que todos los coeficientes estén presentes
    # (si falta alguno por colinealidad, lo tratamos como 0)
    missing <- setdiff(expected_names, names(b))
    if (length(missing) > 0) {
      b <- c(b, setNames(rep(0, length(missing)), missing))
    }
    b <- b[expected_names]
    
    dR_pred <- sum(b * x_named)
    
    # actualizar serie
    R_hat[t] <- R_hat[t - 1] + dR_pred
    
    # guardar predicciones para el período t
    h <- t - cutoff
    se_R <- sqrt(h) * sigma_eps
    
    pred_mean[t] <- R_hat[t]
    pred_lo95[t] <- R_hat[t] - zcrit * se_R
    pred_hi95[t] <- R_hat[t] + zcrit * se_R
  }
  
  tibble(
    date = dates_full,
    Rhat = pred_mean,
    Rlo95 = pred_lo95,
    Rhi95 = pred_hi95
  )
}

# -----------------------
# Load + prepare data (city fixed)
# -----------------------
data_merged <- read_excel(infile)

df0 <- data_merged %>%
  filter(cod_mun == city_code, precio_ipc > 0, precio_sipsa > 0) %>%
  mutate(date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  arrange(alimento_sipsa, date)

# Transformations (logs or levels)
df0 <- df0 %>%
  mutate(
    R = if (use_logs) log(precio_ipc) else precio_ipc,
    W = if (use_logs) log(precio_sipsa) else precio_sipsa
  )

foods <- sort(unique(df0$alimento_sipsa))

pred_list <- list()
model_list <- list()

# -----------------------
# Loop per food
# -----------------------
for (f in foods) {
  print(f)
  df_f <- df0 %>%
    filter(alimento_sipsa == f) %>%
    arrange(date) %>%
    select(alimento_sipsa, date, precio_ipc, precio_sipsa, R, W) %>%
    drop_na(R, W)
  
  n <- nrow(df_f)
  if (n < 36) next
  if (n <= max(L1, L2) + 5) next
  
  cutoff <- floor(0.7 * n)
  
  train <- df_f[1:cutoff, ]
  test  <- df_f[(cutoff + 1):n, ]
  
  # 1) Long-run regression (Engle–Granger step 1)
  lr_fit <- tryCatch(
    lm(R ~ W, data = train),
    error = function(e) NULL
  )
  if (is.null(lr_fit)) next
  
  gamma0 <- coef(lr_fit)[1]
  gamma1 <- coef(lr_fit)[2]
  
  # 2) Build ECM training df (step 2)
  ecm_df <- build_ecm_df(
    R = train$R,
    W = train$W,
    L1 = L1,
    L2 = L2,
    gamma0 = gamma0,
    gamma1 = gamma1
  )
  
  # add names to match expected predictors
  # (lm will use these names)
  # Build formula explicitly
  rhs <- c()
  
  for (i in 0:L1) rhs <- c(rhs, paste0("dW_pos_L", i), paste0("dW_neg_L", i))
  for (i in 1:L2) rhs <- c(rhs, paste0("dR_pos_L", i), paste0("dR_neg_L", i))
  rhs <- c(rhs, "z_pos_L1", "z_neg_L1")
  
  fml <- as.formula(paste("dR ~", paste(rhs, collapse = " + ")))
  
  ecm_fit <- tryCatch(
    lm(fml, data = ecm_df),
    error = function(e) NULL
  )
  if (is.null(ecm_fit)) next
  
  sigma_eps <- sd(residuals(ecm_fit), na.rm = TRUE)
  b <- coef(ecm_fit)
  
  # 3) Forecast dynamically on full series using actual W (known) and predicted R
  fc_R <- forecast_ecm(
    R_train = train$R,
    W_full = df_f$W,
    dates_full = df_f$date,
    cutoff = cutoff,
    L1 = L1,
    L2 = L2,
    gamma0 = gamma0,
    gamma1 = gamma1,
    coef_vec = b,
    sigma_eps = sigma_eps
  )
  
  # Keep only test period predictions
  fc_test <- fc_R %>%
    slice((cutoff + 1):n) %>%
    mutate(alimento_sipsa = f)
  
  # Convert back to levels for evaluation/plots
  if (use_logs) {
    fc_test <- fc_test %>%
      mutate(
        pred_mean = exp(Rhat),
        pred_lo95 = exp(Rlo95),
        pred_hi95 = exp(Rhi95)
      )
  } else {
    fc_test <- fc_test %>%
      mutate(
        pred_mean = Rhat,
        pred_lo95 = Rlo95,
        pred_hi95 = Rhi95
      )
  }
  
  pred_list[[f]] <- fc_test %>%
    mutate(
      observed = test$precio_ipc,
      n_train = cutoff,
      n_test = n - cutoff,
      cod_mun = city_code
    ) %>%
    select(cod_mun, alimento_sipsa, date, observed, pred_mean, pred_lo95, pred_hi95, n_train, n_test)
  
  model_list[[f]] <- tibble(
    cod_mun = city_code,
    alimento_sipsa = f,
    n_total = n,
    n_train = cutoff,
    n_test  = n - cutoff,
    use_logs = use_logs,
    L1 = L1,
    L2 = L2,
    gamma0 = gamma0,
    gamma1 = gamma1,
    sigma_eps = sigma_eps
  )
}

predictions_by_food <- bind_rows(pred_list)
models_summary <- bind_rows(model_list)

# -----------------------
# Save outputs
# -----------------------
write_csv(
  predictions_by_food,
  paste0(out_dir, date_tag, "_m3_predictions_by_food_city_", city_code, ".csv")
)

write_csv(
  models_summary,
  paste0(out_dir, date_tag, "_m3_models_summary_city_", city_code, ".csv")
)


