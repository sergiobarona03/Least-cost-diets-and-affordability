############################################################
## ADF + Engle–Granger for foods (2013:1–2018:3)
## Replicates Enders–Siklos script structure, but:
## - Runs by (city, food-pair)
## - Chooses lag length by IC (BIC default) rather than fixing it
## - Produces publication tables via kable/kableExtra
############################################################

library(tidyverse)
library(readxl)
library(urca)      # ur.df
library(zoo)
library(lubridate)
library(knitr)
library(kableExtra)

##----------------------------------------------------------
## 0. Load data (selected foods dataset)
##----------------------------------------------------------

# Set working directory
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

foods <- read_excel(
  "working-papers/working-paper-aecm/input/261225_selected_foods_dataset.xlsx"
)

# Columns:
# cod_mun, alimento_sipsa, Year, Month, precio_sipsa, articulo_ipc, precio_ipc

foods <- foods %>%
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", as.integer(Year), as.integer(Month))),
    precio_ipc   = as.numeric(precio_ipc),
    precio_sipsa = as.numeric(precio_sipsa),
    log_ipc   = log(precio_ipc),
    log_sipsa = log(precio_sipsa)
  ) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa, date)

##----------------------------------------------------------
## Helper 1: build monthly ts from a vector, with start = min(Y,M)
##----------------------------------------------------------
as_monthly_ts <- function(x, year0, month0) {
  ts(as.numeric(x), start = c(year0, month0), frequency = 12)
}

##----------------------------------------------------------
## Helper 2: choose ADF lag by IC (AIC/BIC) for ur.df
## - Compute IC from the underlying ADF regression residuals:
##   AIC = T log(SSR) + 2n ; BIC = T log(SSR) + n log(T)
## - drift -> tau2 is the main stat; none -> tau1
##----------------------------------------------------------
adf_select_lag_ic <- function(y_ts,
                              type = c("drift", "none"),
                              max_lag = 6,
                              ic = c("BIC", "AIC")) {
  type <- match.arg(type)
  ic   <- match.arg(ic)
  
  fits <- vector("list", max_lag + 1)
  tab  <- tibble(lag = 0:max_lag, AIC = NA_real_, BIC = NA_real_)
  
  for (k in 0:max_lag) {
    fit_k <- ur.df(y_ts, type = type, lags = k)
    
    # underlying regression used by ur.df
    reg <- fit_k@testreg
    e   <- resid(reg)
    SSR <- sum(e^2)
    Tn  <- length(e)
    n   <- length(coef(reg))
    
    AIC_k <- Tn * log(SSR) + 2 * n
    BIC_k <- Tn * log(SSR) + n * log(Tn)
    
    fits[[k + 1]] <- fit_k
    tab$AIC[tab$lag == k] <- AIC_k
    tab$BIC[tab$lag == k] <- BIC_k
  }
  
  k_star <- tab$lag[which.min(tab[[ic]])]
  
  list(
    best_lag = k_star,
    best_fit = fits[[k_star + 1]],
    ic_table = tab
  )
}

##----------------------------------------------------------
## Helper 3: Engle–Granger block
## - ADF on log prices (drift), lags by IC
## - EG: log_ipc ~ 1 + log_sipsa
## - ADF on residual u_hat (none), lags by IC
## - Auxiliary regression: Δu_t on u_{t-1}, Δu_{t-1..t-p}
##----------------------------------------------------------
run_eg_block <- function(df_pair,
                         max_lag_adf = 6,
                         ic = "BIC") {
  
  df_pair <- df_pair %>%
    select(date, log_ipc, log_sipsa) %>%
    drop_na() %>%
    arrange(date)
  
  # Enough obs
  if (nrow(df_pair) < 30) return(NULL)
  
  y0 <- year(min(df_pair$date))
  m0 <- month(min(df_pair$date))
  
  y_ts <- as_monthly_ts(df_pair$log_ipc,   y0, m0)
  x_ts <- as_monthly_ts(df_pair$log_sipsa, y0, m0)
  
  ## (2) ADF on levels (drift), lag chosen by IC
  adf_y <- adf_select_lag_ic(y_ts, type = "drift", max_lag = max_lag_adf, ic = ic)
  adf_x <- adf_select_lag_ic(x_ts, type = "drift", max_lag = max_lag_adf, ic = ic)
  
  ## (6) Engle–Granger step 1: y on const + x
  eg_reg <- lm(log_ipc ~ log_sipsa, data = df_pair)
  u <- resid(eg_reg)
  
  u_ts <- as_monthly_ts(u, y0, m0)
  
  ## (7) ADF on EG residuals u_t (type="none"), lag chosen by IC
  adf_u <- adf_select_lag_ic(u_ts, type = "none", max_lag = max_lag_adf, ic = ic)
  p_aux <- adf_u$best_lag
  
  ## (8) Auxiliary regression
  u_vec <- as.numeric(u_ts)           # length T
  Tn_u  <- length(u_vec)
  
  du_vec   <- diff(u_vec)             # Δu_t, length T-1
  u_l1_vec <- u_vec[1:(Tn_u - 1)]     # u_{t-1}, aligned with Δu_t
  
  aux_df <- tibble(
    du   = du_vec,
    u_l1 = u_l1_vec
  )
  
  if (p_aux > 0) {
    for (j in 1:p_aux) {
      # Δu_{t-j}: shift du_vec down by j
      aux_df[[paste0("du_l", j)]] <- c(rep(NA, j), du_vec[1:(length(du_vec) - j)])
    }
  }
  
  aux_df <- aux_df %>% drop_na()
  
  rhs <- c("u_l1", if (p_aux > 0) paste0("du_l", 1:p_aux) else NULL)
  fml_aux <- as.formula(paste("du ~", paste(rhs, collapse = " + ")))
  
  aux_reg <- lm(fml_aux, data = aux_df)
  
  list(
    adf_log_ipc   = adf_y,
    adf_log_sipsa = adf_x,
    eg_reg        = eg_reg,
    mu_hat        = u,       # residuals for TAR/M-TAR
    adf_u         = adf_u,
    aux_reg       = aux_reg,
    p_aux         = p_aux
  )
}

##----------------------------------------------------------
## 1. Run by (city, food)
##----------------------------------------------------------
pairs <- foods %>%
  distinct(cod_mun, articulo_ipc, alimento_sipsa)

results <- vector("list", nrow(pairs))

for (i in seq_len(nrow(pairs))) {
  
  key <- pairs[i, ]
  
  df_pair <- foods %>%
    filter(
      cod_mun == key$cod_mun,
      articulo_ipc == key$articulo_ipc,
      alimento_sipsa == key$alimento_sipsa
    )
  
  results[[i]] <- run_eg_block(df_pair, max_lag_adf = 6, ic = "BIC")
  
  if (!is.null(results[[i]])) {
    cat("\n============================================\n")
    cat("Pair:", key$cod_mun, "|", key$articulo_ipc, "|", key$alimento_sipsa, "\n")
    cat("ADF(log IPC) lag*   =", results[[i]]$adf_log_ipc$best_lag, "\n")
    cat("ADF(log SIPSA) lag* =", results[[i]]$adf_log_sipsa$best_lag, "\n")
    cat("ADF(residual u) lag*=", results[[i]]$adf_u$best_lag, "\n")
    cat("Aux regression p     =", results[[i]]$p_aux, "\n")
  }
}

##----------------------------------------------------------
## 2. Build tables (ADF + EG + auxiliary)
##----------------------------------------------------------

for (i in seq_len(nrow(pairs))) {
  res <- results[[i]]
  if (is.null(res)) next
  fit <- res$adf_log_ipc$best_fit
  if (is.null(names(fit@teststat))) {
    cat("NO NAMES in teststat for pair i=", i, "\n")
    print(pairs[i,])
    break
  }
  if (!("tau2" %in% names(fit@teststat))) {
    cat("tau2 missing for drift in pair i=", i, "\n")
    print(pairs[i,])
    print(names(fit@teststat))
    break
  }
}

extract_adf_row <- function(adf_obj, series_name, key, type_label) {
  
  fit <- adf_obj$best_fit
  
  # --- Critical values matrix  ---
  cv_mat <- fit@cval
  
  # Decide which tau stat:
  # drift -> tau2 ; none -> tau1
  stat_name <- if (type_label == "drift") "tau2" else "tau1"
  
  # --- Extract tau from summary(fit)  ---
  sm <- summary(fit)
  
  ts_vec <- sm@teststat
  ts_names <- names(ts_vec)
  
  if (!is.null(ts_names) && stat_name %in% ts_names) {
    test_stat <- as.numeric(ts_vec[stat_name])
  } else {
    test_stat <- as.numeric(ts_vec)[1]
  }
  
  # --- Extract critical values from fit@cval ---
  cv_1  <- as.numeric(cv_mat[stat_name, "1pct"])
  cv_5  <- as.numeric(cv_mat[stat_name, "5pct"])
  cv_10 <- as.numeric(cv_mat[stat_name, "10pct"])
  
  stars <- ""
  if (is.finite(test_stat) && is.finite(cv_1) && is.finite(cv_5) && is.finite(cv_10)) {
    stars <- if (test_stat < cv_1) "***" else if (test_stat < cv_5) "**" else if (test_stat < cv_10) "*" else ""
  }
  
  tibble(
    cod_mun = key$cod_mun,
    articulo_ipc = key$articulo_ipc,
    alimento_sipsa = key$alimento_sipsa,
    series = series_name,
    adf_type = type_label,
    lag_star = adf_obj$best_lag,
    test_stat = test_stat,
    cv_1pct = cv_1,
    cv_5pct = cv_5,
    cv_10pct = cv_10,
    stars = stars
  )
}



extract_aux_row <- function(aux_fit, key, p_aux) {
  co <- summary(aux_fit)$coefficients
  regs <- rownames(co)
  
  tibble(
    cod_mun = key$cod_mun,
    articulo_ipc = key$articulo_ipc,
    alimento_sipsa = key$alimento_sipsa,
    p_aux = p_aux,
    term = regs,
    estimate = co[, "Estimate"],
    t_value  = co[, "t value"]
  )
}

adf_table <- tibble()
aux_table <- tibble()
eg_table  <- tibble()

for (i in seq_len(nrow(pairs))) {
  key <- pairs[i, ]
  res <- results[[i]]
  if (is.null(res)) next
  
  adf_table <- bind_rows(
    adf_table,
    extract_adf_row(res$adf_log_ipc,   "log_ipc",   key, "drift"),
    extract_adf_row(res$adf_log_sipsa, "log_sipsa", key, "drift"),
    extract_adf_row(res$adf_u,         "u_hat",     key, "none")
  )
  
  eg_sum <- summary(res$eg_reg)$coefficients
  eg_table <- bind_rows(
    eg_table,
    tibble(
      cod_mun = key$cod_mun,
      articulo_ipc = key$articulo_ipc,
      alimento_sipsa = key$alimento_sipsa,
      beta_hat = eg_sum["log_sipsa", "Estimate"],
      t_beta   = eg_sum["log_sipsa", "t value"],
      n_obs    = nobs(res$eg_reg)
    )
  )
  
  aux_table <- bind_rows(aux_table, extract_aux_row(res$aux_reg, key, res$p_aux))
}

##----------------------------------------------------------
## 3. Print tables (kable) — ready for your Rmd
##----------------------------------------------------------

adf_table_print <- adf_table %>%
  mutate(
    test_stat_fmt = sprintf("%.3f%s", test_stat, stars),
    cv1 = sprintf("%.3f", cv_1pct),
    cv5 = sprintf("%.3f", cv_5pct),
    cv10 = sprintf("%.3f", cv_10pct)
  ) %>%
  select(cod_mun, articulo_ipc, alimento_sipsa, series, lag_star,
         test_stat_fmt, cv1, cv5, cv10) %>%
  rename(
    City = cod_mun,
    `Retail item (IPC)` = articulo_ipc,
    `Wholesale item (SIPSA)` = alimento_sipsa,
    Series = series,
    `Lag* (IC)` = lag_star,
    `ADF tau` = test_stat_fmt,
    `CV 1%` = cv1,
    `CV 5%` = cv5,
    `CV 10%` = cv10
  )

kable(
  adf_table_print,
  booktabs = TRUE,
  caption = "ADF tests on log prices (IPC retail and SIPSA wholesale) and Engle–Granger residuals. Lag length selected by IC (BIC). Significance: *** 1%, ** 5%, * 10%."
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8)

cat("\n\n")

kable(
  eg_table,
  booktabs = TRUE,
  caption = "Engle–Granger cointegrating regression: log(IPC) on log(SIPSA). Reports slope estimate and t-statistic by city and pair."
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8)

cat("\n\n")

aux_table_print <- aux_table %>%
  mutate(
    estimate_fmt = sprintf("%.4f", estimate),
    t_fmt = sprintf("(%.3f)", t_value)
  ) %>%
  select(cod_mun, articulo_ipc, alimento_sipsa, p_aux, term, estimate_fmt, t_fmt) %>%
  rename(
    City = cod_mun,
    `Retail item (IPC)` = articulo_ipc,
    `Wholesale item (SIPSA)` = alimento_sipsa,
    `p (aux)` = p_aux,
    Term = term,
    Estimate = estimate_fmt,
    `t-stat` = t_fmt
  )

kable(
  aux_table_print,
  booktabs = TRUE,
  caption = "Engle–Granger auxiliary regression: Δu_t on u_{t-1} and lagged Δu. Uses p equal to the IC-selected lag length from the residual ADF (type='none')."
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8)
