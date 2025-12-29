########################################################
## ecm_aux.R
## Helpers: build ECM data + select optimal lags (FULL sample)
## Enders-style restriction:
##   - k >= 1
##   - same lag length for Δy and Δx: p = q = k
##   - NO contemporaneous Δx_t in ECM
## Long-run: log_ipc ~ log_sipsa
## ECM: dlog_ipc ~ ect_l1 + Σ_{i=1..k} Δy_{t-i} + Σ_{j=1..k} Δx_{t-j} + mes
## Selection over k = 1..max_k using AIC/BIC (NO split)
########################################################

library(tidyverse)

build_ecm_lagged_full <- function(data, k = 1) {
  
  if (k < 1) stop("k must be >= 1")
  
  # Long run (NO month dummies)
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
  
  # Lags of Δlog_ipc: 1..k
  for (i in 1:k) {
    comb[[paste0("dlog_ipc_l", i)]] <- lag(comb$dlog_ipc, i)
  }
  
  # Lags of Δlog_sipsa: 1..k  (NO l0)
  for (j in 1:k) {
    comb[[paste0("dlog_sipsa_l", j)]] <- lag(comb$dlog_sipsa, j)
  }
  
  # Dataset for ECM estimation
  # Needs dlog_ipc, ect_l1, mes, and the constructed lags
  ecm_df <- comb %>% drop_na(dlog_ipc, ect_l1, mes)
  
  list(lr_model = lr_model, comb = comb, ecm_df = ecm_df)
}

fit_ecm_lag_k_full <- function(data, k = 1,
                               criterion = c("AIC","BIC"),
                               min_ecm_obs = 24) {
  
  criterion <- match.arg(criterion)
  if (k < 1) return(NULL)
  
  tmp <- build_ecm_lagged_full(data, k = k)
  d <- tmp$ecm_df
  if (nrow(d) < min_ecm_obs) return(NULL)
  
  rhs <- c("ect_l1")
  rhs <- c(rhs, paste0("dlog_ipc_l", 1:k))
  rhs <- c(rhs, paste0("dlog_sipsa_l", 1:k))  # NO l0
  rhs <- c(rhs, "mes")
  
  fml <- as.formula(paste("dlog_ipc ~", paste(rhs, collapse = " + ")))
  
  m <- tryCatch(lm(fml, data = d), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  
  crit <- if (criterion == "AIC") AIC(m) else BIC(m)
  
  list(model = m, tmp = tmp, k = k, criterion = criterion, value = crit)
}

select_ecm_lag_k_full <- function(data, max_k = 6,
                                  criterion = c("AIC","BIC"),
                                  min_ecm_obs = 24) {
  
  criterion <- match.arg(criterion)
  
  ks <- 1:max_k
  results <- vector("list", length(ks))
  
  for (idx in seq_along(ks)) {
    k <- ks[idx]
    results[[idx]] <- tryCatch(
      fit_ecm_lag_k_full(data, k = k,
                         criterion = criterion,
                         min_ecm_obs = min_ecm_obs),
      error = function(e) NULL
    )
  }
  
  results <- results[!sapply(results, is.null)]
  if (length(results) == 0) return(NULL)
  
  tab <- tibble(
    k = sapply(results, `[[`, "k"),
    crit = sapply(results, `[[`, "value")
  ) %>% arrange(crit)
  
  best <- results[[which.min(sapply(results, `[[`, "value"))]]
  
  list(table = tab, best = best)
}
