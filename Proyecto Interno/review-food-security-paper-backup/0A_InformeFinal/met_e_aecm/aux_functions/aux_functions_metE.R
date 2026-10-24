########################################################
## aux_functions_metE.R
## Funciones auxiliares para el A-ECM / ECM M-TAR (Metodología E)
##
## Reutiliza del código original de Enders-Siklos:
##   - ic_enders(), build_mtar_base(), build_mtar_df(),
##     estimate_mtar_given_tau(), search_tau_AIC_LB(),
##     estimate_linear_attractor_level(),
##     estimate_linear_attractor_momentum()
##
## ADAPTA para el caso univariado precio mayorista-minorista:
##   - estimate_ecm_mtar_prices(): una sola ecuación (Δln P_min_t),
##     con Δln P_may_{t-j} como regresor exógeno (no segunda ecuación
##     del sistema bivariado original de Enders-Siklos)
##
## NUEVA:
##   - predict_aecm_dynamic(): predicción dinámica/recursiva del A-ECM,
##     usando mu+ y mu- construidos con valores predichos acumulados
########################################################

library(tidyverse)
library(lubridate)

# -----------------------------
# 1. Criterios de información Enders-Granger
#    AIC = T*log(SSR) + 2k  |  BIC = T*log(SSR) + k*log(T)
# -----------------------------
ic_enders <- function(ssr, Tn, n_par) {
  list(
    AIC = Tn * log(ssr) + 2 * n_par,
    BIC = Tn * log(ssr) + n_par * log(Tn)
  )
}

# -----------------------------
# 2. TAR nivel: indicador sobre y_{t-1} >= 0
# -----------------------------
build_tar_df_level <- function(y, p = 0) {
  y  <- as.numeric(y)
  Tn <- length(y)
  dy   <- c(NA, diff(y))
  y_l1 <- c(NA, y[-Tn])
  I    <- ifelse(y_l1 >= 0, 1, 0)
  z1   <- I * y_l1
  z2   <- (1 - I) * y_l1
  dfm  <- data.frame(dy = dy, z1 = z1, z2 = z2)
  if (p > 0) for (j in 1:p) dfm[[paste0("dy_l", j)]] <- c(rep(NA, j), dy[1:(Tn - j)])
  dfm[complete.cases(dfm), , drop = FALSE]
}

estimate_linear_attractor_level <- function(y, p = 0) {
  dfm <- build_tar_df_level(y, p = p)
  rhs <- c("z1", "z2"); if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  fml <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))
  mod <- lm(fml, data = dfm); s <- summary(mod)
  SSR_U <- sum(resid(mod)^2); Tn_m <- nrow(dfm); kU <- length(coef(mod))
  fml_R <- if (p > 0) as.formula(paste("dy ~ 0 +", paste(paste0("dy_l", 1:p), collapse = " + "))) else dy ~ 0
  SSR_R <- sum(resid(lm(fml_R, data = dfm))^2)
  Phi_mu <- ((SSR_R - SSR_U) / 2) / (SSR_U / (Tn_m - kU))
  dfm$z_sum <- dfm$z1 + dfm$z2
  rhs_eq <- c("z_sum"); if (p > 0) rhs_eq <- c(rhs_eq, paste0("dy_l", 1:p))
  an <- anova(lm(as.formula(paste("dy ~ 0 +", paste(rhs_eq, collapse = " + "))), data = dfm), mod)
  ic <- ic_enders(SSR_U, Tn_m, kU)
  Q4 <- Box.test(resid(mod), lag = 4, type = "Ljung-Box", fitdf = kU)
  coefs <- coef(s)
  list(model = mod, p = p,
       rho1 = coefs["z1","Estimate"], t_rho1 = coefs["z1","t value"],
       rho2 = coefs["z2","Estimate"], t_rho2 = coefs["z2","t value"],
       Phi_mu = Phi_mu, F_equal = an$F[2], p_equal = an$`Pr(>F)`[2],
       AIC = ic$AIC, BIC = ic$BIC,
       Q4 = as.numeric(Q4$statistic), p_Q4 = Q4$p.value)
}

# -----------------------------
# 3. TAR momentum: indicador sobre Δy_{t-1} >= 0
# -----------------------------
build_tar_df_momentum <- function(y, p = 0) {
  y  <- as.numeric(y)
  Tn <- length(y)
  dy    <- c(NA, diff(y))
  y_l1  <- c(NA, y[-Tn])
  dy_l1 <- c(NA, diff(y_l1))
  I     <- ifelse(dy_l1 >= 0, 1, 0)
  z1    <- I * y_l1; z2 <- (1 - I) * y_l1
  dfm   <- data.frame(dy = dy, z1 = z1, z2 = z2)
  if (p > 0) for (j in 1:p) dfm[[paste0("dy_l", j)]] <- c(rep(NA, j), dy[1:(Tn - j)])
  dfm[complete.cases(dfm), , drop = FALSE]
}

estimate_linear_attractor_momentum <- function(y, p = 0) {
  dfm <- build_tar_df_momentum(y, p = p)
  rhs <- c("z1", "z2"); if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  fml <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))
  mod <- lm(fml, data = dfm); s <- summary(mod)
  SSR_U <- sum(resid(mod)^2); Tn_m <- nrow(dfm); kU <- length(coef(mod))
  fml_R <- if (p > 0) as.formula(paste("dy ~ 0 +", paste(paste0("dy_l", 1:p), collapse = " + "))) else dy ~ 0
  SSR_R <- sum(resid(lm(fml_R, data = dfm))^2)
  Phi_mu <- ((SSR_R - SSR_U) / 2) / (SSR_U / (Tn_m - kU))
  dfm$z_sum <- dfm$z1 + dfm$z2
  rhs_eq <- c("z_sum"); if (p > 0) rhs_eq <- c(rhs_eq, paste0("dy_l", 1:p))
  an <- anova(lm(as.formula(paste("dy ~ 0 +", paste(rhs_eq, collapse = " + "))), data = dfm), mod)
  ic <- ic_enders(SSR_U, Tn_m, kU)
  Q4 <- Box.test(resid(mod), lag = 4, type = "Ljung-Box", fitdf = kU)
  coefs <- coef(s)
  list(model = mod, p = p,
       rho1 = coefs["z1","Estimate"], t_rho1 = coefs["z1","t value"],
       rho2 = coefs["z2","Estimate"], t_rho2 = coefs["z2","t value"],
       Phi_mu = Phi_mu, F_equal = an$F[2], p_equal = an$`Pr(>F)`[2],
       AIC = ic$AIC, BIC = ic$BIC,
       Q4 = as.numeric(Q4$statistic), p_Q4 = Q4$p.value)
}

# -----------------------------
# 4. M-TAR: búsqueda de umbral tau sobre Δμ_{t-1}, criterio AIC + Ljung-Box
# -----------------------------
build_mtar_base <- function(y, p = 0) {
  y  <- as.numeric(y); Tn <- length(y)
  dy    <- c(NA, diff(y))
  y_l1  <- c(NA, y[-Tn])
  dy_l1 <- c(NA, diff(y_l1))
  dfb   <- data.frame(dy = dy, y_l1 = y_l1, dy_l1 = dy_l1)
  if (p > 0) for (j in 1:p) dfb[[paste0("dy_l", j)]] <- c(rep(NA, j), dy[1:(Tn - j)])
  dfb <- dfb[complete.cases(dfb), , drop = FALSE]
  if (nrow(dfb) < 20) stop("Insufficient observations after lagging.")
  dfb
}

build_mtar_df <- function(base_df, p = 0, tau = 0, trim = 0.15) {
  dfb   <- base_df
  dy    <- dfb$dy; y_l1 <- dfb$y_l1; dy_l1 <- dfb$dy_l1
  I     <- ifelse(dy_l1 >= tau, 1, 0)
  if (mean(I == 1) < trim || mean(I == 0) < trim) return(NULL)
  z1 <- I * y_l1; z2 <- (1 - I) * y_l1
  dfm <- data.frame(dy = dy, y_l1 = y_l1, dy_l1 = dy_l1, z1 = z1, z2 = z2)
  if (p > 0) for (j in 1:p) dfm[[paste0("dy_l", j)]] <- dfb[[paste0("dy_l", j)]]
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20 || all(dfm$z1 == 0) || all(dfm$z2 == 0)) return(NULL)
  if (var(dfm$z1) == 0 || var(dfm$z2) == 0) return(NULL)
  dfm
}

estimate_mtar_given_tau <- function(base_df, p = 0, tau = 0, trim = 0.15) {
  dfm <- build_mtar_df(base_df, p = p, tau = tau, trim = trim)
  if (is.null(dfm)) return(list(tau = tau, model = NULL, p = p, rho1 = NA, t_rho1 = NA,
                                rho2 = NA, t_rho2 = NA, Phi_mu = NA, F_equal = NA, p_equal = NA,
                                AIC = Inf, BIC = Inf, Q4 = NA, p_Q4 = NA, SSR = Inf))
  rhs <- c("z1", "z2"); if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  fml <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))
  mod <- lm(fml, data = dfm); s <- summary(mod)
  SSR_U <- sum(resid(mod)^2); Tn_m <- nrow(dfm); kU <- length(coef(mod))
  fml_R <- if (p > 0) as.formula(paste("dy ~ 0 +", paste(paste0("dy_l", 1:p), collapse = " + "))) else dy ~ 1
  SSR_R <- sum(resid(lm(fml_R, data = dfm))^2)
  Phi_mu <- ((SSR_R - SSR_U) / 2) / (SSR_U / (Tn_m - kU))
  dfm$z_sum <- dfm$z1 + dfm$z2
  rhs_eq <- c("z_sum"); if (p > 0) rhs_eq <- c(rhs_eq, paste0("dy_l", 1:p))
  an <- anova(lm(as.formula(paste("dy ~ 0 +", paste(rhs_eq, collapse = " + "))), data = dfm), mod)
  coefs <- coef(s)
  ic <- ic_enders(SSR_U, Tn_m, kU)
  Q4 <- Box.test(resid(mod), lag = 4, type = "Ljung-Box", fitdf = kU)
  list(tau = tau, model = mod, p = p,
       rho1 = coefs["z1","Estimate"], t_rho1 = coefs["z1","t value"],
       rho2 = coefs["z2","Estimate"], t_rho2 = coefs["z2","t value"],
       Phi_mu = Phi_mu, F_equal = an$F[2], p_equal = an$`Pr(>F)`[2],
       AIC = ic$AIC, BIC = ic$BIC,
       Q4 = as.numeric(Q4$statistic), p_Q4 = Q4$p.value, SSR = SSR_U)
}

search_tau_AIC_LB <- function(y, p = 0, trim = 0.15, alpha_Q = 0.05) {
  base_df <- build_mtar_base(y, p = p)
  z       <- sort(base_df$dy_l1)
  T_eff   <- length(z)
  m       <- floor(trim * T_eff)
  grid    <- unique(z[(m + 1):(T_eff - m)])
  
  n_g     <- length(grid)
  aic_vec <- numeric(n_g)
  pQ_vec  <- numeric(n_g)
  fits    <- vector("list", n_g)
  
  for (j in seq_along(grid)) {
    fit_j      <- estimate_mtar_given_tau(base_df, p = p, tau = grid[j], trim = trim)
    aic_vec[j] <- fit_j$AIC
    pQ_vec[j]  <- if (is.na(fit_j$p_Q4)) 0 else fit_j$p_Q4
    fits[[j]]  <- fit_j
  }
  
  valid <- which(pQ_vec >= alpha_Q & is.finite(aic_vec))
  j_min <- if (length(valid) > 0) valid[which.min(aic_vec[valid])] else {
    fi <- which(is.finite(aic_vec))
    if (length(fi) == 0) 1 else fi[which.min(aic_vec[fi])]
  }
  
  best <- fits[[j_min]]
  best$grid_info <- tibble(tau = grid, AIC = aic_vec, p_Q4 = pQ_vec)
  best
}

# -----------------------------
# 5. ECM M-TAR UNIVARIADO para precios (NUEVA — adapta build_ecm_mtar_df)
#
#    A diferencia del original (sistema bivariado Enders-Siklos), aquí
#    solo hay UNA ecuación: Δln(P_min_t), donde Δln(P_may_{t-j}) entra
#    como regresor exógeno observado (no como segunda variable endógena).
#
#    Especificación:
#    Δln(P_min_t) = c + Σ_j beta_j*Δln(P_min_{t-j})
#                     + Σ_j gamma_j*Δln(P_may_{t-j})
#                     + beta+*mu+_{t-1} + beta-*mu-_{t-1}
#                     + Σ_m delta_m*S_mt + u_t
#
#    donde mu+_{t-1} = max(mu_{t-1}, 0) * I(Δmu_{t-1} >= tau)
#          mu-_{t-1} = min(mu_{t-1}, 0) * (1 - I(Δmu_{t-1} >= tau))
# -----------------------------
build_ecm_mtar_prices_df <- function(mu_hat, log_ipc, log_sipsa, mes,
                                     tau_x, p_lags = 1) {
  Tn <- length(mu_hat)
  
  dlog_ipc   <- c(NA, diff(log_ipc))
  dlog_sipsa <- c(NA, diff(log_sipsa))
  d_mu       <- c(NA, diff(mu_hat))
  d_mu_l1    <- c(NA, d_mu[-length(d_mu)])
  mu_l1      <- c(NA, mu_hat[-length(mu_hat)])
  
  # Indicador M-TAR: basado en Δμ_{t-1} vs umbral tau_x
  M_t         <- ifelse(d_mu_l1 >= tau_x, 1, 0)
  mu_plus_l1  <- M_t       * mu_l1   # error por encima: velocidad de ajuste beta+
  mu_minus_l1 <- (1 - M_t) * mu_l1   # error por debajo: velocidad de ajuste beta-
  
  df_ecm <- tibble(
    dlog_ipc   = dlog_ipc,
    mu_plus_l1 = mu_plus_l1,
    mu_minus_l1 = mu_minus_l1,
    mes        = mes
  )
  
  for (j in 1:p_lags) {
    df_ecm[[paste0("dlog_ipc_l",   j)]] <- c(rep(NA, j), dlog_ipc[1:(Tn - j)])
    df_ecm[[paste0("dlog_sipsa_l", j)]] <- c(rep(NA, j), dlog_sipsa[1:(Tn - j)])
  }
  
  df_ecm %>% drop_na()
}

estimate_ecm_mtar_prices <- function(df_ecm, p_lags = 1) {
  
  rhs_lags <- c(paste0("dlog_ipc_l",   1:p_lags),
                paste0("dlog_sipsa_l",  1:p_lags))
  rhs_all  <- c(rhs_lags, "mu_plus_l1", "mu_minus_l1", "mes")
  
  fml <- as.formula(paste("dlog_ipc ~ 1 +", paste(rhs_all, collapse = " + ")))
  mod <- tryCatch(lm(fml, data = df_ecm), error = function(e) NULL)
  if (is.null(mod)) return(NULL)
  
  cs <- summary(mod)$coefficients
  
  # Coeficientes de corrección de error asimétrica
  beta_plus  <- if ("mu_plus_l1"  %in% rownames(cs)) cs["mu_plus_l1",  "Estimate"] else NA_real_
  beta_minus <- if ("mu_minus_l1" %in% rownames(cs)) cs["mu_minus_l1", "Estimate"] else NA_real_
  t_plus     <- if ("mu_plus_l1"  %in% rownames(cs)) cs["mu_plus_l1",  "t value"]  else NA_real_
  t_minus    <- if ("mu_minus_l1" %in% rownames(cs)) cs["mu_minus_l1", "t value"]  else NA_real_
  
  # Test de simetría: H0: beta+ = beta-
  test_sym <- tryCatch({
    mod_r <- lm(update(fml, . ~ . - mu_plus_l1 - mu_minus_l1 +
                         I(mu_plus_l1 + mu_minus_l1)), data = df_ecm)
    an    <- anova(mod_r, mod)
    list(F = an$F[2], p = an$`Pr(>F)`[2])
  }, error = function(e) list(F = NA_real_, p = NA_real_))
  
  sigma2_step <- sum(resid(mod)^2) / df.residual(mod)
  
  list(
    model       = mod,
    beta_plus   = beta_plus,
    t_plus      = t_plus,
    beta_minus  = beta_minus,
    t_minus     = t_minus,
    F_sym       = test_sym$F,
    p_sym       = test_sym$p,
    r2          = summary(mod)$r.squared,
    sigma2_step = sigma2_step,
    n_obs       = nrow(df_ecm)
  )
}

# -----------------------------
# 6. Predicción dinámica/recursiva del A-ECM (por fecha real, como en D)
#    mu_hat se recalcula en cada paso con log_ipc_hat predicho
# -----------------------------
predict_aecm_dynamic <- function(aecm_fit, lr_model, mtar_fit,
                                 df_history, df_eval, p_lags) {
  
  m       <- aecm_fit$model
  tau_x   <- mtar_fit$tau
  sigma2  <- aecm_fit$sigma2_step
  
  full_dates <- bind_rows(
    df_history %>% select(fecha, log_sipsa, log_ipc, mes),
    df_eval    %>% select(fecha, log_sipsa, log_ipc, mes)
  ) %>%
    distinct(fecha, .keep_all = TRUE) %>%
    arrange(fecha)
  
  full_dates$log_ipc_hat <- NA_real_
  full_dates$var_log_hat <- NA_real_
  full_dates$mu_hat      <- NA_real_
  
  hist_dates <- df_history$fecha
  full_dates$log_ipc_hat[full_dates$fecha %in% hist_dates] <-
    full_dates$log_ipc[full_dates$fecha %in% hist_dates]
  full_dates$var_log_hat[full_dates$fecha %in% hist_dates] <- 0
  
  # μ̂_t = log_ipc_hat_t - (alpha + beta * log_sipsa_t)
  lr_coefs <- coef(lr_model)
  full_dates$mu_hat <- with(full_dates,
                            log_ipc_hat - (lr_coefs[1] + lr_coefs[2] * log_sipsa))
  
  full_dates <- full_dates %>%
    mutate(dlog_sipsa_real = log_sipsa - lag(log_sipsa, default = NA_real_))
  
  get_row_at <- function(d) {
    idx <- which(full_dates$fecha == d)
    if (length(idx) != 1) NA_integer_ else idx
  }
  
  for (target_date in sort(df_eval$fecha)) {
    target_date <- as.Date(target_date, origin = "1970-01-01")
    idx <- get_row_at(target_date)
    if (is.na(idx)) next
    
    lag_dates <- target_date %m-% months(1:p_lags)
    
    # Δlog_ipc rezagados (usando valores predichos acumulados)
    dlog_ipc_lags <- sapply(seq_along(lag_dates), function(li) {
      d_j  <- lag_dates[li]; d_j1 <- d_j %m-% months(1)
      j <- get_row_at(d_j); j1 <- get_row_at(d_j1)
      if (is.na(j) || is.na(j1)) return(NA_real_)
      full_dates$log_ipc_hat[j] - full_dates$log_ipc_hat[j1]
    })
    
    dlog_sipsa_lags <- sapply(seq_along(lag_dates), function(li) {
      j <- get_row_at(lag_dates[li])
      if (is.na(j)) return(NA_real_)
      full_dates$dlog_sipsa_real[j]
    })
    
    if (any(is.na(dlog_ipc_lags)) || any(is.na(dlog_sipsa_lags))) next
    
    # ECT asimétrico: mu_{t-1} descompuesto según tau_x
    date_l1 <- target_date %m-% months(1)
    j1 <- get_row_at(date_l1)
    if (is.na(j1) || is.na(full_dates$log_ipc_hat[j1])) next
    
    mu_l1    <- full_dates$mu_hat[j1]
    mu_l1_l1 <- if (j1 > 1) full_dates$mu_hat[j1 - 1] else NA_real_
    if (is.na(mu_l1) || is.na(mu_l1_l1)) next
    
    d_mu_l1     <- mu_l1 - mu_l1_l1
    M_t         <- as.numeric(d_mu_l1 >= tau_x)
    mu_plus_l1  <- M_t       * mu_l1
    mu_minus_l1 <- (1 - M_t) * mu_l1
    
    newdata <- as_tibble(setNames(as.list(dlog_ipc_lags),   paste0("dlog_ipc_l",   1:p_lags)))
    newdata <- bind_cols(newdata,
                         as_tibble(setNames(as.list(dlog_sipsa_lags), paste0("dlog_sipsa_l", 1:p_lags))))
    newdata$mu_plus_l1  <- mu_plus_l1
    newdata$mu_minus_l1 <- mu_minus_l1
    newdata$mes         <- full_dates$mes[idx]
    
    dlog_pred <- tryCatch(
      suppressWarnings(predict(m, newdata = newdata)),
      error = function(e) NA_real_
    )
    if (is.na(dlog_pred)) next
    
    j1_var <- full_dates$var_log_hat[j1]
    j1_var <- if (is.na(j1_var)) 0 else j1_var
    
    full_dates$log_ipc_hat[idx] <- full_dates$log_ipc_hat[j1] + dlog_pred
    full_dates$var_log_hat[idx] <- j1_var + sigma2
    # Actualizar mu_hat con el nuevo log_ipc_hat predicho
    full_dates$mu_hat[idx] <- full_dates$log_ipc_hat[idx] -
      (lr_coefs[1] + lr_coefs[2] * full_dates$log_sipsa[idx])
  }
  
  full_dates %>%
    filter(fecha %in% df_eval$fecha) %>%
    transmute(
      fecha,
      price_pred     = exp(log_ipc_hat),
      price_pred_lwr = exp(log_ipc_hat - 1.96 * sqrt(pmax(var_log_hat, 0))),
      price_pred_upr = exp(log_ipc_hat + 1.96 * sqrt(pmax(var_log_hat, 0))),
      price_obs      = exp(log_ipc)
    )
}