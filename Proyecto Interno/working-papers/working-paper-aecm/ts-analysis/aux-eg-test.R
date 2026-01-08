###############################################################
## aux-eg-test.R
## Helpers for EG cointegration using aTSA::coint.test
## - X-13 seasonal adjustment (seasonal::seas) on log series
## - IC-based lag selection for nlag using residual ADF regression
##   * adf_type = "drift"  (intercept)
##   * adf_type = "trend"  (intercept + trend)
## - Inference uses aTSA::coint.test (MacKinnon-type p-values)
###############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
  library(lubridate)
  library(seasonal)
  library(urca)
  library(aTSA)
})

# -------------------------------------------------------------
# Helper: safe file name
# -------------------------------------------------------------
safe_name <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")
}

# -------------------------------------------------------------
# X-13 seasonal adjustment on monthly ts in LOG levels
# Returns numeric vector (same length) or NULL if fails
# -------------------------------------------------------------
sa_x13 <- function(x_ts) {
  fit <- try(
    seas(
      x_ts,
      x11 = "",
      transform.function = "none",   # already in logs
      regression.aictest = NULL,
      outlier = NULL
    ),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) return(NULL)
  as.numeric(final(fit))
}

# -------------------------------------------------------------
# Lag selection for residual ADF regression by IC (AIC/BIC)
# IMPORTANT: selection only; inference is via aTSA::coint.test
# Constraints for aTSA embed(): nlag must be >= 1 and <= n-2
# adf_type: "drift" or "trend"
# -------------------------------------------------------------
select_nlag_ic <- function(x, y,
                           max_lag = 12,
                           ic = c("BIC", "AIC"),
                           adf_type = c("drift", "trend")) {
  ic <- match.arg(ic)
  adf_type <- match.arg(adf_type)
  
  if (length(x) != length(y)) stop("x and y must have same length.")
  n <- length(x)
  
  if (n < 24) {
    return(tibble(
      lag = NA_integer_, ic = NA_real_, aic = NA_real_, bic = NA_real_,
      ic_used = ic, adf_type = adf_type
    ))
  }
  
  lag_max_feasible <- min(max_lag, n - 2)
  if (lag_max_feasible < 1) {
    return(tibble(
      lag = NA_integer_, ic = NA_real_, aic = NA_real_, bic = NA_real_,
      ic_used = ic, adf_type = adf_type
    ))
  }
  
  # EG first-stage residuals
  u <- residuals(lm(x ~ y))
  
  grid <- vector("list", lag_max_feasible)
  
  for (L in 1:lag_max_feasible) {
    adf <- ur.df(u, type = adf_type, lags = L)
    e  <- adf@testreg$residuals
    nn <- length(e)
    k  <- length(coef(adf@testreg))
    
    sigma2 <- sum(e^2) / nn
    aic_val <- nn * log(sigma2) + 2 * k
    bic_val <- nn * log(sigma2) + log(nn) * k
    ic_val  <- if (ic == "AIC") aic_val else bic_val
    
    grid[[L]] <- tibble(lag = L, ic = ic_val, aic = aic_val, bic = bic_val)
  }
  
  bind_rows(grid) %>%
    arrange(ic) %>%
    slice(1) %>%
    mutate(ic_used = ic, adf_type = adf_type)
}

# -------------------------------------------------------------
# Run aTSA cointegration test with IC-selected nlag
# - adf_type chooses how lag is selected (drift vs trend)
# - aTSA::coint.test uses that selected nlag
# Returns tidy table keeping type 1/2/3 rows from aTSA
# -------------------------------------------------------------
run_atsa_coint_ic <- function(x, y,
                              d = 0,
                              max_lag = 12,
                              ic = "BIC",
                              adf_type = "drift") {
  
  if (length(x) != length(y)) stop("x and y must have same length.")
  n <- length(x)
  
  if (n < 24) {
    return(tibble(
      type = NA_character_, lag = NA_integer_, EG = NA_real_, p.value = NA_real_,
      d = d, n_obs = n,
      ic_used = ic, adf_type = adf_type,
      ic_lag = NA_integer_, ic_value = NA_real_, aic = NA_real_, bic = NA_real_
    ))
  }
  
  best <- select_nlag_ic(x, y, max_lag = max_lag, ic = ic, adf_type = adf_type)
  
  if (is.na(best$lag)) {
    return(tibble(
      type = NA_character_, lag = NA_integer_, EG = NA_real_, p.value = NA_real_,
      d = d, n_obs = n,
      ic_used = ic, adf_type = adf_type,
      ic_lag = NA_integer_, ic_value = NA_real_, aic = NA_real_, bic = NA_real_
    ))
  }
  
  nlag_star <- best$lag
  nlag_star <- max(1, min(nlag_star, n - 2))  # final safety clamp
  
  ct <- try(aTSA::coint.test(x, y, d = d, nlag = nlag_star), silent = TRUE)
  
  if (inherits(ct, "try-error")) {
    return(tibble(
      type = NA_character_, lag = NA_integer_, EG = NA_real_, p.value = NA_real_,
      d = d, n_obs = n,
      ic_used = best$ic_used, adf_type = adf_type,
      ic_lag = nlag_star, ic_value = best$ic, aic = best$aic, bic = best$bic
    ))
  }
  
  as.data.frame(ct) %>%
    tibble::rownames_to_column("type") %>%
    as_tibble() %>%
    mutate(
      lag = as.integer(lag),
      EG = as.numeric(EG),
      p.value = as.numeric(p.value),
      d = d,
      n_obs = n,
      ic_used = best$ic_used,
      adf_type = adf_type,
      ic_lag = nlag_star,
      ic_value = best$ic,
      aic = best$aic,
      bic = best$bic
    ) %>%
    select(type, lag, EG, p.value, d, n_obs, ic_used, adf_type, ic_lag, ic_value, aic, bic)
}
