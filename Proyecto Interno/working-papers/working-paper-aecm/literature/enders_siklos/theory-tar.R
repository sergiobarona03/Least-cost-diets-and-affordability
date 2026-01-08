###############################################################
## tar_model.R
## Enders & Siklos (2001) threshold cointegration test (TAR)
## - Step 1: y ~ const + x  => residuals u_t
## - Step 2: Threshold-ADF on u_t:
##     Δu_t = ρ1 I_t u_{t-1} + ρ2 (1-I_t) u_{t-1} + Σ γ_i Δu_{t-i} + ε_t
##   where I_t = 1(u_{t-1} >= τ), else 0  (TAR indicator)
##
## Outputs:
## - selected lag p* via AIC/BIC (on the threshold regression)
## - threshold τ (0 or Chan-consistent estimate)
## - t-stats for ρ1, ρ2, and t-Max
## - Φ (F-test for H0: ρ1=ρ2=0), for comparison with Tables 1–2
###############################################################

# -----------------------------
# Optional packages (only for plotting)
# -----------------------------
have_ggplot <- requireNamespace("ggplot2", quietly = TRUE)

# -----------------------------
# Simulate cointegrated data (if you cannot / do not want to use paper data)
# Construct x_t as RW, and y_t = alpha + beta x_t + u_t
# where u_t follows a TAR adjustment process in differences.
# -----------------------------
simulate_tar_cointegration <- function(T = 240,
                                       alpha = 0,
                                       beta = 1,
                                       rho1 = -0.10,
                                       rho2 = -0.50,
                                       tau = 0,
                                       sd_u = 1,
                                       sd_x = 1,
                                       seed = 123) {
  set.seed(seed)
  
  x <- numeric(T)
  u <- numeric(T)
  du <- numeric(T)
  
  # random walk x
  for (t in 2:T) x[t] <- x[t-1] + rnorm(1, 0, sd_x)
  
  # TAR dynamics for u
  u[1] <- 0
  du[1] <- 0
  for (t in 2:T) {
    I_t <- as.integer(u[t-1] >= tau)
    du[t] <- (rho1 * I_t + rho2 * (1 - I_t)) * u[t-1] + rnorm(1, 0, sd_u)
    u[t] <- u[t-1] + du[t]
  }
  
  y <- alpha + beta * x + u
  data.frame(t = 1:T, y = y, x = x, u_true = u)
}

# -----------------------------
# Build regression dataset for TAR threshold-ADF
# du_t on (I*u_{t-1}), ((1-I)*u_{t-1}), and lagged du's
# No intercept, matching the paper’s presentation.
# -----------------------------
build_tar_reg_df <- function(u, p = 0, tau = 0) {
  T <- length(u)
  du <- c(NA_real_, diff(u))
  
  u_l1  <- c(NA_real_, u[1:(T-1)])
  z_l1  <- u_l1  # TAR: threshold variable is u_{t-1}
  I     <- ifelse(is.na(z_l1), NA_integer_, as.integer(z_l1 >= tau))
  
  df <- data.frame(
    du    = du,
    u_l1  = u_l1,
    Iu    = I * u_l1,
    I0u   = (1 - I) * u_l1
  )
  
  if (p > 0) {
    for (i in 1:p) {
      df[[paste0("du_l", i)]] <- dplyr::lag(df$du, i)
    }
  }
  
  # drop rows with NA induced by differencing/lags
  df <- df[stats::complete.cases(df), , drop = FALSE]
  df
}

# -----------------------------
# Fit TAR threshold regression for given (p, tau)
# Returns lm object + SSR + IC values
# -----------------------------
fit_tar_threshold <- function(u, p = 0, tau = 0) {
  df <- build_tar_reg_df(u, p = p, tau = tau)
  if (nrow(df) < (10 + 2 * p)) return(NULL)
  
  rhs <- c("Iu", "I0u")
  if (p > 0) rhs <- c(rhs, paste0("du_l", 1:p))
  fml <- stats::as.formula(paste("du ~ 0 +", paste(rhs, collapse = " + ")))
  
  m <- tryCatch(stats::lm(fml, data = df), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  
  ssr <- sum(stats::residuals(m)^2)
  list(model = m, df = df, ssr = ssr, aic = stats::AIC(m), bic = stats::BIC(m))
}

# -----------------------------
# Chan-style threshold selection (consistent threshold estimate):
# Search over middle (1-2*trim) of sorted threshold variable (here u_{t-1}).
# The paper discards 15% at each tail (middle 70%). 
# -----------------------------
select_tau_chan_tar <- function(u, p = 0, trim = 0.15) {
  df0 <- build_tar_reg_df(u, p = p, tau = 0)
  
  # candidate threshold variable is z_l1 = u_{t-1} (already embedded in df0 via u_l1)
  z <- df0$u_l1
  z <- z[is.finite(z)]
  n <- length(z)
  if (n < 30) return(list(tau = 0, fit = NULL))
  
  z_sorted <- sort(z)
  lo <- floor(trim * n) + 1
  hi <- ceiling((1 - trim) * n) - 1
  if (lo >= hi) return(list(tau = 0, fit = NULL))
  
  cand <- unique(z_sorted[lo:hi])
  
  best_fit <- NULL
  best_tau <- NA_real_
  best_ssr <- Inf
  
  for (tt in cand) {
    fit <- fit_tar_threshold(u, p = p, tau = tt)
    if (is.null(fit)) next
    if (fit$ssr < best_ssr) {
      best_ssr <- fit$ssr
      best_tau <- tt
      best_fit <- fit
    }
  }
  
  list(tau = best_tau, fit = best_fit)
}

# -----------------------------
# Choose p via IC (AIC or BIC), jointly with tau selection method
# - threshold = "zero": tau fixed at tau0
# - threshold = "chan": tau estimated for each p (Chan search), then IC evaluated
# -----------------------------
select_p_and_fit_tar <- function(u,
                                 max_p = 12,
                                 ic = c("BIC", "AIC"),
                                 threshold = c("zero", "chan"),
                                 tau0 = 0,
                                 trim = 0.15) {
  ic <- match.arg(ic)
  threshold <- match.arg(threshold)
  
  best <- NULL
  best_val <- Inf
  
  for (p in 0:max_p) {
    
    if (threshold == "zero") {
      fit <- fit_tar_threshold(u, p = p, tau = tau0)
      if (is.null(fit)) next
      val <- if (ic == "AIC") fit$aic else fit$bic
      tau_used <- tau0
      
    } else {
      tmp <- select_tau_chan_tar(u, p = p, trim = trim)
      fit <- tmp$fit
      if (is.null(fit)) next
      val <- if (ic == "AIC") fit$aic else fit$bic
      tau_used <- tmp$tau
    }
    
    if (is.finite(val) && val < best_val) {
      best_val <- val
      best <- list(p = p, tau = tau_used, fit = fit, ic = ic, ic_value = val,
                   threshold = threshold)
    }
  }
  
  best
}

# -----------------------------
# Compute Φ (F-test H0: rho1=rho2=0) and symmetry test H0: rho1=rho2
# NOTE: Φ has nonstandard critical values in the paper (Tables 1–2).
# -----------------------------
compute_tests_tar <- function(fit_obj) {
  m <- fit_obj$model
  df <- fit_obj$df
  
  co <- summary(m)$coefficients
  rho1_t <- unname(co["Iu", "t value"])
  rho2_t <- unname(co["I0u", "t value"])
  t_max  <- max(rho1_t, rho2_t)   # "larger" t statistic (closer to 0)
  
  # Φ: compare unrestricted vs restricted without Iu and I0u
  p <- fit_obj$p
  if (p > 0) {
    rhs_r <- paste(paste0("du_l", 1:p), collapse = " + ")
    fml_r <- stats::as.formula(paste("du ~ 0 +", rhs_r))
    m_r <- stats::lm(fml_r, data = df)
    ssr_r <- sum(residuals(m_r)^2)
  } else {
    # restricted is du ~ 0 : SSR = sum(du^2)
    ssr_r <- sum(df$du^2)
  }
  
  ssr_u <- sum(residuals(m)^2)
  q <- 2
  df_u <- df.residual(m)
  phi <- ((ssr_r - ssr_u) / q) / (ssr_u / df_u)
  
  # Symmetry test H0: rho1=rho2 => replace (Iu, I0u) with u_l1
  # Restricted: du ~ 0 + u_l1 + du_lags
  rhs_sym <- c("u_l1")
  if (p > 0) rhs_sym <- c(rhs_sym, paste0("du_l", 1:p))
  fml_sym <- stats::as.formula(paste("du ~ 0 +", paste(rhs_sym, collapse = " + ")))
  m_sym <- stats::lm(fml_sym, data = df)
  
  ssr_sym <- sum(residuals(m_sym)^2)
  q_sym <- 1
  df_u2 <- df.residual(m)
  F_sym <- ((ssr_sym - ssr_u) / q_sym) / (ssr_u / df_u2)
  p_sym <- stats::pf(F_sym, df1 = q_sym, df2 = df_u2, lower.tail = FALSE)
  
  list(
    rho1_t = rho1_t, rho2_t = rho2_t, t_max = t_max,
    phi = phi, phi_df1 = q, phi_df2 = df_u,
    F_sym = F_sym, p_sym = p_sym
  )
}

# -----------------------------
# Main TAR test wrapper
# -----------------------------
enders_siklos_tar <- function(y, x,
                              max_p = 12,
                              ic = c("BIC", "AIC"),
                              threshold = c("zero", "chan"),
                              tau0 = 0,
                              trim = 0.15) {
  ic <- match.arg(ic)
  threshold <- match.arg(threshold)
  
  # Step 1: cointegrating regression
  lr <- lm(y ~ x)
  u  <- residuals(lr)
  
  # Step 2: select p and tau, fit threshold regression
  sel <- select_p_and_fit_tar(u, max_p = max_p, ic = ic,
                              threshold = threshold, tau0 = tau0, trim = trim)
  if (is.null(sel)) stop("No feasible model could be estimated (check sample size / max_p).")
  
  sel$fit$p <- sel$p  # attach
  sel$fit$tau <- sel$tau
  
  tests <- compute_tests_tar(list(model = sel$fit$model, df = sel$fit$df, p = sel$p))
  
  # small summary
  out <- list(
    long_run = lr,
    residuals = u,
    threshold = sel$threshold,
    tau = sel$tau,
    p = sel$p,
    ic = sel$ic,
    ic_value = sel$ic_value,
    th_model = sel$fit$model,
    tests = tests
  )
  
  class(out) <- "es_tar"
  out
}

print.es_tar <- function(x, ...) {
  cat("\nEnders–Siklos TAR threshold cointegration step (TAR)\n")
  cat("Selected threshold method:", x$threshold, "\n")
  cat("Selected tau:", signif(x$tau, 5), "\n")
  cat("Selected lag p:", x$p, " | IC:", x$ic, "=", round(x$ic_value, 3), "\n\n")
  
  cat("Threshold regression coefficients (no intercept):\n")
  print(coef(summary(x$th_model)))
  
  cat("\nKey test statistics to compare with Enders–Siklos critical values:\n")
  cat("t_rho1 =", round(x$tests$rho1_t, 3),
      " | t_rho2 =", round(x$tests$rho2_t, 3),
      " | t-Max =", round(x$tests$t_max, 3), "\n")
  cat("Phi (F for rho1=rho2=0) =", round(x$tests$phi, 3),
      " with df(", x$tests$phi_df1, ",", x$tests$phi_df2, ")\n")
  
  cat("\nSymmetry test (rho1=rho2) [standard F approximation]:\n")
  cat("F =", round(x$tests$F_sym, 3), " | p =", signif(x$tests$p_sym, 4), "\n")
}

# -----------------------------
# Demo run (simulate)
# -----------------------------
demo <- simulate_tar_cointegration(T = 240, seed = 1)
res  <- enders_siklos_tar(demo$y, demo$x,
                          max_p = 12,
                          ic = "BIC",
                          threshold = "chan",  # "zero" or "chan"
                          tau0 = 0,
                          trim = 0.15)

print(res)

# Optional plot: residuals and threshold regime (needs ggplot2)
if (have_ggplot) {
  u <- res$residuals
  du <- c(NA_real_, diff(u))
  I <- as.integer(dplyr::lag(u, 1) >= res$tau)
  
  dfp <- data.frame(t = 1:length(u), u = u, I = I, du = du)
  
  p1 <- ggplot2::ggplot(dfp, ggplot2::aes(x = t, y = u)) +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::labs(title = "Cointegrating residual u_t (TAR demo)", x = NULL, y = "u_t") +
    ggplot2::theme_minimal(base_size = 11)
  
  print(p1)
}
