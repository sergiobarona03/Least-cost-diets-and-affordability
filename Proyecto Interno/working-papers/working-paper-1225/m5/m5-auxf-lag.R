########################################################
## Funciones auxiliares: selección de rezagos óptimos ECM
## (AIC/BIC) para tu M5
## - Long run: log_ipc ~ log_sipsa + mes
## - ECT_{t-1} = resid(lr_model)_{t-1}
## - ECM: dlog_ipc ~ ect_l1 + lag(dlog_ipc) + lag(dlog_sipsa) + mes
########################################################

# -----------------------------
# 1) Funciones auxiliares básicas
# -----------------------------

# Safe name (para archivos)
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# -----------------------------
# 2) Construir dataset ECM con rezagos
# -----------------------------
build_ecm_lagged_data <- function(train, test, p = 0, q = 0) {
  
  # Ecuación de largo plazo (EG) con estacionalidad mensual
  lr_model <- lm(log_ipc ~ log_sipsa + mes, data = train)
  
  # Dataset combinado (train+test) para construir ECT y diferencias
  comb <- dplyr::bind_rows(
    train %>% dplyr::mutate(split = "train"),
    test  %>% dplyr::mutate(split = "test")
  ) %>%
    dplyr::arrange(fecha) %>%
    dplyr::mutate(
      # Ajuste largo plazo
      lr_fit = stats::predict(lr_model, newdata = .),
      # Error de equilibrio (residuo)
      ect    = log_ipc - lr_fit,
      # Rezago del ECT
      ect_l1 = dplyr::lag(ect),
      # Primeras diferencias
      dlog_ipc   = log_ipc   - dplyr::lag(log_ipc),
      dlog_sipsa = log_sipsa - dplyr::lag(log_sipsa)
    )
  
  # Rezagos de Δlog_ipc: 1..p
  if (p > 0) {
    for (i in 1:p) {
      comb[[paste0("dlog_ipc_l", i)]] <- dplyr::lag(comb$dlog_ipc, i)
    }
  }
  
  # Rezagos de Δlog_sipsa: 0..q (incluye contemporáneo)
  for (j in 0:q) {
    comb[[paste0("dlog_sipsa_l", j)]] <- dplyr::lag(comb$dlog_sipsa, j)
  }
  
  # Submuestras listas para ECM
  train_ecm <- comb %>%
    dplyr::filter(split == "train") %>%
    tidyr::drop_na(dlog_ipc, ect_l1, mes)
  
  test_ecm <- comb %>%
    dplyr::filter(split == "test") %>%
    tidyr::drop_na(ect_l1, mes)
  
  list(
    lr_model  = lr_model,
    comb      = comb,
    train_ecm = train_ecm,
    test_ecm  = test_ecm
  )
}

# -----------------------------
# 3) Ajustar ECM (p,q) y devolver criterio
# -----------------------------
fit_ecm_lags <- function(train, test, p = 0, q = 0,
                         criterion = c("AIC", "BIC"),
                         min_train_ecm = 18) {
  
  criterion <- match.arg(criterion)
  
  tmp <- build_ecm_lagged_data(train, test, p = p, q = q)
  d <- tmp$train_ecm
  
  # Filtro mínimo de obs
  if (nrow(d) < min_train_ecm) return(NULL)
  
  # Armar RHS del ECM:
  # dlog_ipc ~ ect_l1 + dlog_ipc_l1..lp + dlog_sipsa_l0..lq + mes
  rhs <- c("ect_l1")
  if (p > 0) rhs <- c(rhs, paste0("dlog_ipc_l", 1:p))
  rhs <- c(rhs, paste0("dlog_sipsa_l", 0:q))
  rhs <- c(rhs, "mes")
  
  fml <- stats::as.formula(paste("dlog_ipc ~", paste(rhs, collapse = " + ")))
  
  # Estimar ECM
  m <- tryCatch(stats::lm(fml, data = d), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  
  # Calcular criterio
  crit <- if (criterion == "AIC") stats::AIC(m) else stats::BIC(m)
  
  list(
    model = m,
    tmp   = tmp,
    p     = p,
    q     = q,
    criterion = criterion,
    value = crit
  )
}

# -----------------------------
# 4) Selección de rezagos óptimos (grid search)
# -----------------------------
select_ecm_lags <- function(train, test,
                            max_p = 6, max_q = 6,
                            criterion = c("AIC", "BIC"),
                            min_train_ecm = 18) {
  
  criterion <- match.arg(criterion)
  
  grid <- expand.grid(p = 0:max_p, q = 0:max_q)
  results <- vector("list", nrow(grid))
  
  for (k in seq_len(nrow(grid))) {
    p <- grid$p[k]
    q <- grid$q[k]
    
    results[[k]] <- tryCatch(
      fit_ecm_lags(train, test, p = p, q = q,
                   criterion = criterion,
                   min_train_ecm = min_train_ecm),
      error = function(e) NULL
    )
  }
  
  # Limpiar NULLs
  results <- results[!sapply(results, is.null)]
  if (length(results) == 0) return(NULL)
  
  # Tabla ranking
  tab <- tibble::tibble(
    p = sapply(results, `[[`, "p"),
    q = sapply(results, `[[`, "q"),
    crit = sapply(results, `[[`, "value")
  ) %>% dplyr::arrange(crit)
  
  # Mejor modelo
  best <- results[[which.min(sapply(results, `[[`, "value"))]]
  
  list(
    table = tab,
    best  = best
  )
}
