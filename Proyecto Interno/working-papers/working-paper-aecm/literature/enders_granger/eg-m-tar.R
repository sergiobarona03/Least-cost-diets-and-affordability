###############################################################
# mtar_enders_granger.R
# Enders & Granger (1998): M-TAR unit-root test with asymmetric adjustment
#
# Same regression as TAR, but indicator is momentum:
#   I_t = 1{ Δy_{t-1} >= 0 }     (Step 2: indicator per eq. (7))
#
# Lag selection by AIC/BIC; decision by Table-1 critical values.
###############################################################

# -----------------------------
# Table 1 critical values (Enders & Granger, 1998)
# Reject H0: unit root if Phi* > cv
#
# Panels:
#   det="none"  M-TAR -> Panel B
#   det="const" M-TAR -> Panel D
#   det="trend" M-TAR -> Panel F
# -----------------------------
.eg_cv_mtar <- list(
  none = data.frame(
    T = c(50, 100, 250, 1000),
    cv10 = c(2.98, 2.83, 2.68, 2.51),
    cv05 = c(3.81, 3.60, 3.41, 3.21),
    cv01 = c(5.79, 5.38, 5.10, 4.85)
  ),
  const = data.frame(
    T = c(50, 100, 250, 1000),
    cv10 = c(4.17, 4.11, 4.05, 4.05),
    cv05 = c(5.14, 5.02, 4.95, 4.95),
    cv01 = c(7.43, 7.10, 6.99, 6.91)
  ),
  trend = data.frame(
    T = c(50, 100, 250, 1000),
    cv10 = c(5.89, 5.74, 5.64, 5.60),
    cv05 = c(7.07, 6.83, 6.65, 6.57),
    cv01 = c(9.77, 9.21, 8.85, 8.74)
  )
)

.eg_get_cv <- function(Tn, det = c("none","const","trend")) {
  det <- match.arg(det)
  tab <- .eg_cv_mtar[[det]]
  j <- which.min(abs(tab$T - Tn))
  as.list(tab[j, c("cv10","cv05","cv01")])
}

.eg_detrend <- function(y, det = c("none","const","trend")) {
  det <- match.arg(det)
  y <- as.numeric(y)
  
  if (det == "none") return(y)
  
  t <- seq_along(y)
  if (det == "const") {
    fit <- lm(y ~ 1)
  } else {
    fit <- lm(y ~ t)
  }
  as.numeric(residuals(fit))
}

# -----------------------------
# Build regression data for M-TAR ADF-type regression
# -----------------------------
.eg_build_df_mtar <- function(ytilde, p = 0) {
  ytilde <- as.numeric(ytilde)
  Tn <- length(ytilde)
  if (Tn < (p + 4)) return(NULL)
  
  dy <- c(NA, diff(ytilde))
  y_l1 <- c(NA, ytilde[-Tn])
  
  # momentum uses Δy_{t-1} => dy_l1
  dy_l1 <- c(NA, dy[-length(dy)])
  
  I <- ifelse(dy_l1 >= 0, 1, 0)
  
  z1 <- I * y_l1
  z2 <- (1 - I) * y_l1
  
  df <- data.frame(
    dy = dy,
    z1 = z1,
    z2 = z2
  )
  
  if (p > 0) {
    for (i in 1:p) {
      df[[paste0("dy_l", i)]] <- c(rep(NA, i), dy[1:(length(dy) - i)])
    }
  }
  
  df <- df[complete.cases(df), , drop = FALSE]
  if (nrow(df) < 10) return(NULL)
  df
}

.eg_ic <- function(ssr, Tn, k, ic = c("AIC","BIC")) {
  ic <- match.arg(ic)
  base <- Tn * log(ssr / Tn)
  if (ic == "AIC") return(base + 2 * k)
  base + log(Tn) * k
}

.eg_fit_mtar_p <- function(ytilde, p = 0, ic = c("BIC","AIC")) {
  ic <- match.arg(ic)
  
  df <- .eg_build_df_mtar(ytilde, p = p)
  if (is.null(df)) return(NULL)
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  f_full <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))
  
  rhs_r <- character(0)
  if (p > 0) rhs_r <- paste0("dy_l", 1:p)
  if (length(rhs_r) == 0) {
    f_r <- as.formula("dy ~ 0 + 1")
  } else {
    f_r <- as.formula(paste("dy ~ 0 +", paste(rhs_r, collapse = " + ")))
  }
  
  fit_full <- lm(f_full, data = df)
  
  if (length(rhs_r) == 0) {
    ssr_r <- sum(df$dy^2)
    k_r <- 0
  } else {
    fit_r <- lm(f_r, data = df)
    ssr_r <- sum(residuals(fit_r)^2)
    k_r <- length(coef(fit_r))
  }
  
  ssr_u <- sum(residuals(fit_full)^2)
  df_u  <- df.residual(fit_full)
  k_u   <- length(coef(fit_full))
  
  q <- 2
  Phi_star <- ((ssr_r - ssr_u) / q) / (ssr_u / df_u)
  
  # symmetry test rho1=rho2
  df$y_l1_sym <- (df$z1 + df$z2)
  rhs_sym <- c("y_l1_sym")
  if (p > 0) rhs_sym <- c(rhs_sym, paste0("dy_l", 1:p))
  f_sym <- as.formula(paste("dy ~ 0 +", paste(rhs_sym, collapse = " + ")))
  fit_sym <- lm(f_sym, data = df)
  
  ssr_sym <- sum(residuals(fit_sym)^2)
  F_sym <- ((ssr_sym - ssr_u) / 1) / (ssr_u / df_u)
  
  ic_val <- .eg_ic(ssr_u, Tn = nrow(df), k = k_u, ic = ic)
  
  list(
    p = p,
    n_obs = nrow(df),
    fit = fit_full,
    Phi_star = as.numeric(Phi_star),
    F_sym = as.numeric(F_sym),
    ic = ic,
    ic_value = as.numeric(ic_val)
  )
}

eg_mtar_urtest <- function(y,
                           det = c("none","const","trend"),
                           max_lag = 12,
                           ic = c("BIC","AIC")) {
  
  det <- match.arg(det)
  ic  <- match.arg(ic)
  
  ytilde <- .eg_detrend(y, det = det)
  
  fits <- lapply(0:max_lag, function(p) .eg_fit_mtar_p(ytilde, p = p, ic = ic))
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (length(fits) == 0) stop("No feasible model (series too short for requested max_lag).")
  
  ic_vals <- vapply(fits, function(x) x$ic_value, numeric(1))
  best <- fits[[which.min(ic_vals)]]
  
  cv <- .eg_get_cv(best$n_obs, det = det)
  
  decision <- list(
    reject_10 = (best$Phi_star > cv$cv10),
    reject_05 = (best$Phi_star > cv$cv05),
    reject_01 = (best$Phi_star > cv$cv01)
  )
  
  list(
    model = "M-TAR",
    det = det,
    ic = ic,
    max_lag = max_lag,
    p_star = best$p,
    n_obs = best$n_obs,
    Phi_star = best$Phi_star,
    cv = cv,
    decision = decision,
    F_sym = best$F_sym,
    fit = best$fit,
    ic_table = data.frame(
      p = vapply(fits, `[[`, integer(1), "p"),
      n_obs = vapply(fits, `[[`, integer(1), "n_obs"),
      Phi_star = vapply(fits, `[[`, numeric(1), "Phi_star"),
      ic_value = vapply(fits, `[[`, numeric(1), "ic_value")
    )[order(vapply(fits, `[[`, numeric(1), "ic_value")), ]
  )
}

# -----------------------------
# Example (simulated data)
# -----------------------------
simulate_mtar_stationary <- function(n = 250, rho1 = -0.10, rho2 = -0.55, sigma = 1, y0 = 0) {
  y <- numeric(n)
  y[1] <- y0
  dy_prev <- 0
  for (t in 2:n) {
    I <- ifelse(dy_prev >= 0, 1, 0)
    dy <- (I * rho1 + (1 - I) * rho2) * y[t-1] + rnorm(1, 0, sigma)
    y[t] <- y[t-1] + dy
    dy_prev <- dy
  }
  y
}

if (sys.nframe() == 0) {
  set.seed(123)
  y <- simulate_mtar_stationary(n = 300)
  
  out <- eg_mtar_urtest(y, det = "const", max_lag = 8, ic = "BIC")
  
  cat("\n--- Enders-Granger M-TAR unit-root test ---\n")
  cat("deterministic:", out$det, " | IC:", out$ic, " | p* =", out$p_star, "\n")
  cat("Phi* =", round(out$Phi_star, 3), "\n")
  cat("CV (10%, 5%, 1%) =", out$cv$cv10, out$cv$cv05, out$cv$cv01, "\n")
  cat("Reject H0 at 5%? ", out$decision$reject_05, "\n")
  cat("Symmetry F (rho1=rho2):", round(out$F_sym, 3), "\n\n")
  
  print(head(out$ic_table, 10))
  print(summary(out$fit))
}
