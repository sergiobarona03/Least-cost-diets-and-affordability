###############################################################
## mtar_model.R
## Enders & Siklos (2001) threshold cointegration test (M-TAR)
## Same as TAR, except the indicator uses Δu_{t-1}:
##   M_t = 1(Δu_{t-1} >= τ), else 0   (momentum threshold)
###############################################################

have_ggplot <- requireNamespace("ggplot2", quietly = TRUE)

simulate_mtar_cointegration <- function(T = 240,
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
  
  for (t in 2:T) x[t] <- x[t-1] + rnorm(1, 0, sd_x)
  
  u[1] <- 0
  du[1] <- 0
  for (t in 2:T) {
    # M-TAR: indicator depends on previous change
    M_t <- as.integer(du[t-1] >= tau)
    du[t] <- (rho1 * M_t + rho2 * (1 - M_t)) * u[t-1] + rnorm(1, 0, sd_u)
    u[t] <- u[t-1] + du[t]
  }
  
  y <- alpha + beta * x + u
  data.frame(t = 1:T, y = y, x = x, u_true = u)
}

build_mtar_reg_df <- function(u, p = 0, tau = 0) {
  T <- length(u)
  du <- c(NA_real_, diff(u))
  
  u_l1  <- c(NA_real_, u[1:(T-1)])
  du_l1 <- c(NA_real_, du[1:(T-1)])  # Δu_{t-1}
  
  # M-TAR: threshold variable is Δu_{t-1}
  z_l1 <- du_l1
  M    <- ifelse(is.na(z_l1), NA_integer_, as.integer(z_l1 >= tau))
  
  df <- data.frame(
    du    = du,
    u_l1  = u_l1,
    Mu    = M * u_l1,
    M0u   = (1 - M) * u_l1
  )
  
  if (p > 0) {
    for (i in 1:p) {
      df[[paste0("du_l", i)]] <- dplyr::lag(df$du, i)
    }
  }
  
  df <- df[stats::complete.cases(df), , drop = FALSE]
  df
}

fit_mtar_threshold <- function(u, p = 0, tau = 0) {
  df <- build_mtar_reg_df(u, p = p, tau = tau)
  if (nrow(df) < (10 + 2 * p)) return(NULL)
  
  rhs <- c("Mu", "M0u")
  if (p > 0) rhs <- c(rhs, paste0("du_l", 1:p))
  fml <- stats::as.formula(paste("du ~ 0 +", paste(rhs, collapse = " + ")))
  
  m <- tryCatch(stats::lm(fml, data = df), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  
  ssr <- sum(stats::residuals(m)^2)
  list(model = m, df = df, ssr = ssr, aic = stats::AIC(m), bic = stats::BIC(m))
}

select_tau_chan_mtar <- function(u, p = 0, trim = 0.15) {
  df0 <- build_mtar_reg_df(u, p = p, tau = 0)
  
  # candidate threshold variable is Δu_{t-1} (embedded via du_l1 -> affects Mu/M0u)
  # We reconstruct Δu_{t-1} from the current df0: we can use du and its lags:
  # In df0, du is Δu_t; so Δu_{t-1} is lag(du,1) in aligned time.
  # But easiest: rebuild from u here:
  du_full <- c(NA_real_, diff(u))
  du_l1_full <- c(NA_real_, du_full[1:(length(u)-1)])
  # align with df0 rows (complete cases)
  idx <- as.integer(rownames(df0))
  z <- du_l1_full[idx]
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
    fit <- fit_mtar_threshold(u, p = p, tau = tt)
    if (is.null(fit)) next
    if (fit$ssr < best_ssr) {
      best_ssr <- fit$ssr
      best_tau <- tt
      best_fit <- fit
    }
  }
  
  list(tau = best_tau, fit = best_fit)
}

select_p_and_fit_mtar <- function(u,
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
      fit <- fit_mtar_threshold(u, p = p, tau = tau0)
      if (is.null(fit)) next
      val <- if (ic == "AIC") fit$aic else fit$bic
      tau_used <- tau0
      
    } else {
      tmp <- select_tau_chan_mtar(u, p = p, trim = trim)
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

compute_tests_mtar <- function(fit_obj) {
  m <- fit_obj$model
  df <- fit_obj$df
  
  co <- summary(m)$coefficients
  rho1_t <- unname(co["Mu", "t value"])
  rho2_t <- unname(co["M0u", "t value"])
  t_max  <- max(rho1_t, rho2_t)
  
  p <- fit_obj$p
  if (p > 0) {
    rhs_r <- paste(paste0("du_l", 1:p), collapse = " + ")
    fml_r <- stats::as.formula(paste("du ~ 0 +", rhs_r))
    m_r <- stats::lm(fml_r, data = df)
    ssr_r <- sum(residuals(m_r)^2)
  } else {
    ssr_r <- sum(df$du^2)
  }
  
  ssr_u <- sum(residuals(m)^2)
  q <- 2
  df_u <- df.residual(m)
  phi <- ((ssr_r - ssr_u) / q) / (ssr_u / df_u)
  
  # symmetry: rho1=rho2 => du ~ 0 + u_l1 + du_lags
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

enders_siklos_mtar <- function(y, x,
                               max_p = 12,
                               ic = c("BIC", "AIC"),
                               threshold = c("zero", "chan"),
                               tau0 = 0,
                               trim = 0.15) {
  ic <- match.arg(ic)
  threshold <- match.arg(threshold)
  
  lr <- lm(y ~ x)
  u  <- residuals(lr)
  
  sel <- select_p_and_fit_mtar(u, max_p = max_p, ic = ic,
                               threshold = threshold, tau0 = tau0, trim = trim)
  if (is.null(sel)) stop("No feasible model could be estimated (check sample size / max_p).")
  
  sel$fit$p <- sel$p
  sel$fit$tau <- sel$tau
  
  tests <- compute_tests_mtar(list(model = sel$fit$model, df = sel$fit$df, p = sel$p))
  
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
  
  class(out) <- "es_mtar"
  out
}

print.es_mtar <- function(x, ...) {
  cat("\nEnders–Siklos TAR threshold cointegration step (M-TAR)\n")
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
demo <- simulate_mtar_cointegration(T = 240, seed = 2)
res  <- enders_siklos_mtar(demo$y, demo$x,
                           max_p = 12,
                           ic = "BIC",
                           threshold = "chan",
                           tau0 = 0,
                           trim = 0.15)

print(res)

if (have_ggplot) {
  u <- res$residuals
  du <- c(NA_real_, diff(u))
  M <- as.integer(dplyr::lag(du, 1) >= res$tau)
  
  dfp <- data.frame(t = 1:length(u), u = u, M = M, du = du)
  
  p1 <- ggplot2::ggplot(dfp, ggplot2::aes(x = t, y = u)) +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::labs(title = "Cointegrating residual u_t (M-TAR demo)", x = NULL, y = "u_t") +
    ggplot2::theme_minimal(base_size = 11)
  
  print(p1)
}
