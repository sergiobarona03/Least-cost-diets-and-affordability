###############################################################
# tar_enders_granger.R
# Enders & Granger (1998): TAR unit-root test with asymmetric adjustment
#
# Core regression (on detrended/demeaned residuals y~):
#   Δy_t = ρ1 I_t y_{t-1} + ρ2 (1-I_t) y_{t-1} + Σ_{i=1..p} γ_i Δy_{t-i} + ε_t
# TAR indicator: I_t = 1{ y_{t-1} >= 0 }  (Heaviside)
#
# Lag length p chosen by AIC/BIC, as suggested in the paper.  (Step 3)
# Decision uses Table-1 nonstandard critical values.          (Step 2)
###############################################################

# -----------------------------
# Table 1 critical values (Enders & Granger, 1998)
# Reject H0: unit root if Phi > cv
#
# Panels:
#   det="none"  TAR -> Panel A
#   det="const" TAR -> Panel C
#   det="trend" TAR -> Panel E
# -----------------------------
.eg_cv_tar <- list(
  none = data.frame(
    T = c(50, 100, 250, 1000),
    cv10 = c(3.30, 3.18, 3.10, 3.04),
    cv05 = c(4.12, 3.95, 3.82, 3.75),
    cv01 = c(6.09, 5.69, 5.53, 5.36)
  ),
  const = data.frame(
    T = c(50, 100, 250, 1000),
    cv10 = c(3.84, 3.79, 3.74, 3.74),
    cv05 = c(4.73, 4.64, 4.56, 4.56),
    cv01 = c(6.85, 6.57, 6.47, 6.41)
  ),
  trend = data.frame(
    T = c(50, 100, 250, 1000),
    cv10 = c(5.41, 5.27, 5.18, 5.15),
    cv05 = c(6.52, 6.30, 6.12, 6.08),
    cv01 = c(9.14, 8.58, 8.23, 8.12)
  )
)

.eg_get_cv <- function(Tn, det = c("none","const","trend")) {
  det <- match.arg(det)
  tab <- .eg_cv_tar[[det]]

  # nearest-T rule (simple + transparent)
  j <- which.min(abs(tab$T - Tn))
  as.list(tab[j, c("cv10","cv05","cv01")])
}

# -----------------------------
# Detrend/demean step (paper Step 2):
# regress on deterministic components and keep residuals.
#   det="none"  : y~ = y
#   det="const" : y~ = residuals(y ~ 1)
#   det="trend" : y~ = residuals(y ~ 1 + t)
# -----------------------------
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
# Build regression data for TAR ADF-type regression
# -----------------------------
.eg_build_df_tar <- function(ytilde, p = 0) {
  ytilde <- as.numeric(ytilde)
  Tn <- length(ytilde)
  if (Tn < (p + 3)) return(NULL)

  dy <- c(NA, diff(ytilde))
  y_l1 <- c(NA, ytilde[-Tn])

  # TAR indicator depends on y_{t-1}
  I <- ifelse(y_l1 >= 0, 1, 0)

  z1 <- I * y_l1
  z2 <- (1 - I) * y_l1

  df <- data.frame(
    dy = dy,
    z1 = z1,
    z2 = z2
  )

  # add lagged Δy terms: Δy_{t-1} ... Δy_{t-p}
  if (p > 0) {
    for (i in 1:p) {
      df[[paste0("dy_l", i)]] <- c(rep(NA, i), dy[1:(length(dy) - i)])
    }
  }

  # keep complete cases (no interpolation; just drop initial undefined rows)
  df <- df[complete.cases(df), , drop = FALSE]
  if (nrow(df) < 10) return(NULL)
  df
}

# -----------------------------
# IC the paper uses (see notes around their AIC/BIC discussion):
# use SSR and number of regressors. Ranking matches standard AIC/BIC.
# -----------------------------
.eg_ic <- function(ssr, Tn, k, ic = c("AIC","BIC")) {
  ic <- match.arg(ic)
  # "T * log(SSR/T)" style
  base <- Tn * log(ssr / Tn)
  if (ic == "AIC") return(base + 2 * k)
  base + log(Tn) * k
}

# -----------------------------
# Fit TAR regression for a given p; compute Phi (=F test rho1=rho2=0)
# and symmetry F test rho1=rho2 (optional)
# -----------------------------
.eg_fit_tar_p <- function(ytilde, p = 0, ic = c("BIC","AIC")) {
  ic <- match.arg(ic)

  df <- .eg_build_df_tar(ytilde, p = p)
  if (is.null(df)) return(NULL)

  # full model: dy ~ 0 + z1 + z2 + dy_l1 + ... + dy_lp
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  f_full <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))

  # restricted model for Phi: exclude z1 and z2 (i.e., rho1=rho2=0)
  rhs_r <- character(0)
  if (p > 0) rhs_r <- paste0("dy_l", 1:p)
  if (length(rhs_r) == 0) {
    f_r <- as.formula("dy ~ 0 + 1") # dummy; we will fit via lm.fit below
  } else {
    f_r <- as.formula(paste("dy ~ 0 +", paste(rhs_r, collapse = " + ")))
  }

  fit_full <- lm(f_full, data = df)

  # restricted: if no lagged Δy terms, SSR_r = sum(dy^2) from zero-regression
  if (length(rhs_r) == 0) {
    ssr_r <- sum(df$dy^2)
    df_r <- nrow(df)
    k_r <- 0
  } else {
    fit_r <- lm(f_r, data = df)
    ssr_r <- sum(residuals(fit_r)^2)
    df_r <- df.residual(fit_r)
    k_r <- length(coef(fit_r))
  }

  ssr_u <- sum(residuals(fit_full)^2)
  df_u  <- df.residual(fit_full)
  k_u   <- length(coef(fit_full))

  # Phi = F test for adding z1 and z2 (2 restrictions)
  q <- 2
  Phi <- ((ssr_r - ssr_u) / q) / (ssr_u / df_u)

  # Symmetry test: H0 rho1=rho2
  # Under symmetry: dy ~ 0 + y_l1 + dy_lags
  # Here, y_l1 is implicit through z1+z2, so build it explicitly:
  df$y_l1_sym <- (df$z1 + df$z2)

  rhs_sym <- c("y_l1_sym")
  if (p > 0) rhs_sym <- c(rhs_sym, paste0("dy_l", 1:p))
  f_sym <- as.formula(paste("dy ~ 0 +", paste(rhs_sym, collapse = " + ")))
  fit_sym <- lm(f_sym, data = df)

  ssr_sym <- sum(residuals(fit_sym)^2)
  df_sym  <- df.residual(fit_sym)

  # F for adding one extra parameter (difference between rho1 and rho2)
  q_sym <- 1
  F_sym <- ((ssr_sym - ssr_u) / q_sym) / (ssr_u / df_u)

  # IC (for lag selection)
  ic_val <- .eg_ic(ssr_u, Tn = nrow(df), k = k_u, ic = ic)

  list(
    p = p,
    n_obs = nrow(df),
    fit = fit_full,
    Phi = as.numeric(Phi),
    F_sym = as.numeric(F_sym),
    ic = ic,
    ic_value = as.numeric(ic_val)
  )
}

# -----------------------------
# Public function: TAR unit-root test
# -----------------------------
eg_tar_urtest <- function(y,
                          det = c("none","const","trend"),
                          max_lag = 12,
                          ic = c("BIC","AIC")) {

  det <- match.arg(det)
  ic  <- match.arg(ic)

  ytilde <- .eg_detrend(y, det = det)

  fits <- lapply(0:max_lag, function(p) .eg_fit_tar_p(ytilde, p = p, ic = ic))
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (length(fits) == 0) stop("No feasible model (series too short for requested max_lag).")

  # select p* by IC
  ic_vals <- vapply(fits, function(x) x$ic_value, numeric(1))
  best <- fits[[which.min(ic_vals)]]

  # critical values from Table 1 (nearest-T)
  cv <- .eg_get_cv(best$n_obs, det = det)

  decision <- list(
    reject_10 = (best$Phi > cv$cv10),
    reject_05 = (best$Phi > cv$cv05),
    reject_01 = (best$Phi > cv$cv01)
  )

  list(
    model = "TAR",
    det = det,
    ic = ic,
    max_lag = max_lag,
    p_star = best$p,
    n_obs = best$n_obs,
    Phi = best$Phi,
    cv = cv,
    decision = decision,
    F_sym = best$F_sym,
    fit = best$fit,
    ic_table = data.frame(
      p = vapply(fits, `[[`, integer(1), "p"),
      n_obs = vapply(fits, `[[`, integer(1), "n_obs"),
      Phi = vapply(fits, `[[`, numeric(1), "Phi"),
      ic_value = vapply(fits, `[[`, numeric(1), "ic_value")
    )[order(vapply(fits, `[[`, numeric(1), "ic_value")), ]
  )
}

# -----------------------------
# Example (simulated data, so you can run it immediately)
# -----------------------------
simulate_tar_stationary <- function(n = 250, rho1 = -0.15, 
                                    rho2 = -0.60, sigma = 1, y0 = 0) {
  y <- numeric(n)
  y[1] <- y0
  for (t in 2:n) {
    I <- ifelse(y[t-1] >= 0, 1, 0)
    dy <- (I * rho1 + (1 - I) * rho2) * y[t-1] + rnorm(1, 0, sigma)
    y[t] <- y[t-1] + dy
  }
  y
}

if (sys.nframe() == 0) {
  set.seed(123)
  y <- simulate_tar_stationary(n = 300)

  out <- eg_tar_urtest(y, det = "const", max_lag = 8, ic = "BIC")

  cat("\n--- Enders-Granger TAR unit-root test ---\n")
  cat("deterministic:", out$det, " | IC:", out$ic, " | p* =", out$p_star, "\n")
  cat("Phi =", round(out$Phi, 3), "\n")
  cat("CV (10%, 5%, 1%) =", out$cv$cv10, out$cv$cv05, out$cv$cv01, "\n")
  cat("Reject H0 at 5%? ", out$decision$reject_05, "\n")
  cat("Symmetry F (rho1=rho2):", round(out$F_sym, 3), "\n\n")

  print(head(out$ic_table, 10))
  print(summary(out$fit))
}
