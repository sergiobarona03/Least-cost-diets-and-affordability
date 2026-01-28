############################################################
## M-TAR (tau = tau_hat) on EG residuals: mu_hat
## - Momentum TAR (Enders & Siklos)
## - Indicator based on change: I_t = 1{ Δmu_{t-1} >= tau }
## - tau_hat estimated by grid search (AIC), subject to LB
## - Selection by Enders IC, subject to Ljung-Box Q(1)..Q(h_max)
##   (EViews-style). We report Q(h_max) and its p-value.
############################################################

library(tidyverse)
library(knitr)
library(kableExtra)

##----------------------------------------------------------
## Enders–Granger AIC/BIC:
##   AIC = T log(SSR) + 2n
##   BIC = T log(SSR) + n log(T)
##----------------------------------------------------------
ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

##----------------------------------------------------------
## Ljung–Box diagnostics (EViews-style):
## - Compute Q(h) for h = 1..h_max on residuals
## - Return:
##     * Q_last / p_last: Q(h_max) and p-value (main reporting)
##     * p_min: min p-value over 1..h_max (conservative)
##----------------------------------------------------------
ljung_box_grid <- function(e, h_max = 8) {
  out <- tibble(h = 1:h_max, Q = NA_real_, p = NA_real_)
  for (hh in 1:h_max) {
    bt <- Box.test(e, lag = hh, type = "Ljung-Box")
    out$Q[out$h == hh] <- as.numeric(bt$statistic)
    out$p[out$h == hh] <- bt$p.value
  }
  list(
    table  = out,
    Q_last = out$Q[out$h == h_max],
    p_last = out$p[out$h == h_max],
    p_min  = suppressWarnings(min(out$p, na.rm = TRUE))
  )
}

##----------------------------------------------------------
## (1) Build base MTAR variables:
##     Δmu_t, mu_{t-1}, Δmu_{t-1} (threshold regressor)
##----------------------------------------------------------
build_mtar_base <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu    <- c(NA, diff(mu_hat))        # Δmu_t
  mu_l1  <- c(NA, mu_hat[-Tn])         # mu_{t-1}
  dmu_l1 <- c(NA, diff(mu_l1))         # Δmu_{t-1}
  
  dfb <- data.frame(dmu = dmu, mu_l1 = mu_l1, dmu_l1 = dmu_l1)
  
  if (p > 0) {
    for (j in 1:p) {
      dfb[[paste0("dmu_l", j)]] <- c(rep(NA, j), dmu[1:(Tn - j)])
    }
  }
  
  dfb <- dfb[complete.cases(dfb), , drop = FALSE]
  if (nrow(dfb) < 20) stop("Insufficient observations after lagging.")
  
  dfb
}

##----------------------------------------------------------
## (2) Build MTAR regression data for a given tau:
##     I_t = 1{ Δmu_{t-1} >= tau }
##----------------------------------------------------------
build_mtar_df <- function(base_df, p = 0, tau = 0, trim = 0.15) {
  
  I <- ifelse(base_df$dmu_l1 >= tau, 1, 0)
  
  ## Require minimum observations per regime (trim)
  if (mean(I == 1) < trim || mean(I == 0) < trim) return(NULL)
  
  z1 <- I * base_df$mu_l1
  z2 <- (1 - I) * base_df$mu_l1
  
  dfm <- data.frame(dmu = base_df$dmu, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dmu_l", j)]] <- base_df[[paste0("dmu_l", j)]]
    }
  }
  
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20) return(NULL)
  
  ## Avoid degenerate regimes
  if (var(dfm$z1) == 0 || var(dfm$z2) == 0) return(NULL)
  
  dfm
}

##----------------------------------------------------------
## (3) Estimate MTAR for given tau:
##     - rho1, rho2, Phi_mu, Equality test
##     - Enders AIC/BIC
##     - Ljung–Box Q(1)..Q(h_max) on residuals (EViews-style)
##     Note: we REPORT Q(h_max) and its p-value as Q4/p_Q4 columns
##----------------------------------------------------------
estimate_mtar_given_tau <- function(base_df, p = 0, tau = 0,
                                    trim = 0.15,
                                    lb_hmax = 8) {
  
  dfm <- build_mtar_df(base_df, p = p, tau = tau, trim = trim)
  if (is.null(dfm)) {
    return(list(tau = tau, AIC = Inf, BIC = Inf, p_Q4 = NA))
  }
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dmu_l", 1:p))
  fml <- as.formula(paste("dmu ~ 0 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  
  ## Phi_mu: H0 rho1 = rho2 = 0
  if (p > 0) {
    fml_R <- as.formula(paste("dmu ~ 0 +", paste0("dmu_l", 1:p, collapse = " + ")))
  } else {
    fml_R <- dmu ~ 0
  }
  mod_R <- lm(fml_R, data = dfm)
  SSR_R <- sum(resid(mod_R)^2)
  
  Phi_mu <- ((SSR_R - SSR_U) / 2) / (SSR_U / (Tn_m - kU))
  
  ## Equality test: H0 rho1 = rho2
  dfm_eq <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  mod_eq <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + "))),
               data = dfm_eq)
  an <- anova(mod_eq, mod)
  
  ## Information criteria
  ic <- ic_enders(SSR_U, Tn_m, kU)
  
  ## Ljung–Box Q(1)..Q(h_max) on residuals (EViews-style)
  lb <- ljung_box_grid(resid(mod), h_max = lb_hmax)
  
  list(
    tau      = tau,
    rho1     = coef(s)["z1", "Estimate"],
    t_rho1   = coef(s)["z1", "t value"],
    rho2     = coef(s)["z2", "Estimate"],
    t_rho2   = coef(s)["z2", "t value"],
    Phi_mu   = Phi_mu,
    F_equal  = an$F[2],
    p_equal  = an$`Pr(>F)`[2],
    AIC      = ic$AIC,
    BIC      = ic$BIC,
    Q4       = lb$Q_last,   # this is Q(lb_hmax)
    p_Q4     = lb$p_last,   # p-value for Q(lb_hmax)
    p_Qmin   = lb$p_min,    # min p-value over 1..lb_hmax (optional)
    lb_table = lb$table     # full LB table (optional)
  )
}

##----------------------------------------------------------
## (4) Grid search for tau (tau_hat):
##     - tau_grid = trimmed unique values of Δmu_{t-1}
##     - Selection: minimum AIC subject to Ljung–Box p_Q(h_max) >= alpha
##     - If none pass, choose global minimum AIC (warn)
##----------------------------------------------------------
search_tau_mtar <- function(mu_hat,
                            p = 0,
                            trim = 0.15,
                            alpha_Q = 0.05,
                            lb_hmax = 8) {
  
  base_df <- build_mtar_base(mu_hat, p = p)
  
  z <- sort(base_df$dmu_l1)
  T_eff <- length(z)
  m <- floor(trim * T_eff)
  if (2*m >= T_eff) stop("Trim too large for effective sample.")
  
  tau_grid <- unique(z[(m + 1):(T_eff - m)])
  
  res <- map_dfr(tau_grid, ~{
    fit <- estimate_mtar_given_tau(base_df, p = p, tau = .x,
                                   trim = trim, lb_hmax = lb_hmax)
    tibble(
      tau  = .x,
      AIC  = fit$AIC,
      BIC  = fit$BIC,
      p_Q  = fit$p_Q4   # p-value for Q(lb_hmax)
    )
  })
  
  ok <- res %>% filter(is.finite(AIC), !is.na(p_Q), p_Q >= alpha_Q)
  
  if (nrow(ok) > 0) {
    tau_hat <- ok$tau[which.min(ok$AIC)]
    note <- paste0("tau_hat selected by min AIC among candidates with Ljung–Box p_Q(",
                   lb_hmax, ") >= ", alpha_Q, ".")
  } else {
    tau_hat <- res$tau[which.min(res$AIC)]
    note <- paste0("WARNING: no tau satisfies Ljung–Box p_Q(",
                   lb_hmax, ") >= ", alpha_Q,
                   "; tau_hat selected by min AIC over the full grid.")
  }
  
  list(tau_hat = tau_hat, grid = res, note = note, lb_hmax = lb_hmax)
}

############################################################
## (5) MTAR (tau = tau_hat) lag selection table (like TAR)
##     - Step 1: estimate tau_hat by grid search (AIC + LB)
##     - Step 2: for each p=0..p_max estimate MTAR at tau_hat
##     - Step 3: select p* by IC subject to Ljung–Box p_Q(h_max) >= alpha
##     - Step 4: print kable table
############################################################

## Stars helper
t_stars <- function(t) {
  ifelse(abs(t) >= 2.58, "***",
         ifelse(abs(t) >= 1.96, "**",
                ifelse(abs(t) >= 1.65, "*", "")))
}

## Select p given tau_hat
select_p_mtar_given_tau <- function(mu_hat,
                                    tau_hat,
                                    p_max = 6,
                                    ic = c("BIC", "AIC"),
                                    trim = 0.15,
                                    alpha_Q = 0.05,
                                    lb_hmax = 8) {
  
  ic <- match.arg(ic)
  
  fits <- vector("list", p_max + 1)
  
  tab <- tibble(
    p = 0:p_max,
    tau = tau_hat,
    rho1 = NA_real_, t_rho1 = NA_real_,
    rho2 = NA_real_, t_rho2 = NA_real_,
    Phi_mu = NA_real_,
    F_equal = NA_real_, p_equal = NA_real_,
    AIC = NA_real_, BIC = NA_real_,
    Q4 = NA_real_, p_Q4 = NA_real_
  )
  
  for (pp in 0:p_max) {
    base_df <- build_mtar_base(mu_hat, p = pp)
    
    fit_pp <- estimate_mtar_given_tau(
      base_df, p = pp, tau = tau_hat,
      trim = trim, lb_hmax = lb_hmax
    )
    
    fits[[pp + 1]] <- fit_pp
    
    tab[pp + 1, c("rho1","t_rho1","rho2","t_rho2","Phi_mu",
                  "F_equal","p_equal","AIC","BIC","Q4","p_Q4")] <-
      list(fit_pp$rho1, fit_pp$t_rho1,
           fit_pp$rho2, fit_pp$t_rho2,
           fit_pp$Phi_mu,
           fit_pp$F_equal, fit_pp$p_equal,
           fit_pp$AIC, fit_pp$BIC,
           fit_pp$Q4, fit_pp$p_Q4)
  }
  
  ok <- tab %>% filter(!is.na(p_Q4), p_Q4 >= alpha_Q)
  
  if (nrow(ok) > 0) {
    p_star <- ok$p[which.min(ok[[ic]])]
    note <- paste0("Selected p* by ", ic,
                   " among models with Ljung–Box p_Q(", lb_hmax, ") >= ", alpha_Q, ".")
  } else {
    p_star <- tab$p[which.min(tab[[ic]])]
    note <- paste0("WARNING: no model passed Ljung–Box (p_Q(", lb_hmax, ") >= ", alpha_Q,
                   "); selected p* by ", ic, " anyway.")
  }
  
  list(
    tau_hat = tau_hat,
    p_star  = p_star,
    best    = fits[[p_star + 1]],
    table   = tab,
    note    = note,
    lb_hmax = lb_hmax
  )
}

## Main wrapper (tau_hat + p-selection table)
run_mtar_table <- function(mu_hat,
                           p_tau = 0,          # p used during tau search
                           p_max = 6,
                           ic_p  = "BIC",       # IC to select p*
                           trim = 0.15,
                           alpha_Q = 0.05,
                           lb_hmax = 8) {
  
  tau_out <- search_tau_mtar(mu_hat, p = p_tau, trim = trim,
                             alpha_Q = alpha_Q, lb_hmax = lb_hmax)
  
  out <- select_p_mtar_given_tau(
    mu_hat = mu_hat,
    tau_hat = tau_out$tau_hat,
    p_max = p_max,
    ic = ic_p,
    trim = trim,
    alpha_Q = alpha_Q,
    lb_hmax = lb_hmax
  )
  
  out$tau_note <- tau_out$note
  out$tau_grid <- tau_out$grid
  out
}

##-----------------------------------------------------------
## Print kable table (same style as TAR)
## Note: Q(4) column is actually Q(lb_hmax) to match EViews-style
##-----------------------------------------------------------
print_mtar_kable <- function(out_mtar, ic_label = "BIC", alpha_Q = 0.05) {
  
  tab_print <- out_mtar$table %>%
    mutate(
      Selected = if_else(p == out_mtar$p_star, "p*", ""),
      tau_fmt  = sprintf("%.5f", tau),
      rho1_fmt = sprintf("%.4f%s", rho1, t_stars(t_rho1)),
      trho1_fmt = sprintf("(%.3f)", t_rho1),
      rho2_fmt = sprintf("%.4f%s", rho2, t_stars(t_rho2)),
      trho2_fmt = sprintf("(%.3f)", t_rho2),
      Phi_mu_fmt = sprintf("%.3f", Phi_mu),
      Feq_fmt    = sprintf("%.3f", F_equal),
      peq_fmt    = sprintf("%.3f", p_equal),
      AIC_fmt    = sprintf("%.2f", AIC),
      BIC_fmt    = sprintf("%.2f", BIC),
      Q_fmt      = sprintf("%.3f", Q4),
      pQ_fmt     = if_else(is.na(p_Q4), "NA", sprintf("%.3f", p_Q4))
    ) %>%
    select(
      p, Selected, tau_fmt,
      rho1_fmt, trho1_fmt,
      rho2_fmt, trho2_fmt,
      Phi_mu_fmt, Feq_fmt, peq_fmt,
      AIC_fmt, BIC_fmt, Q_fmt, pQ_fmt
    ) %>%
    rename(
      `p` = p,
      `Selected` = Selected,
      `tau_hat` = tau_fmt,
      `rho1` = rho1_fmt,
      `t(rho1)` = trho1_fmt,
      `rho2` = rho2_fmt,
      `t(rho2)` = trho2_fmt,
      `Phi_mu` = Phi_mu_fmt,
      `F_equal` = Feq_fmt,
      `p_equal` = peq_fmt,
      `AIC` = AIC_fmt,
      `BIC` = BIC_fmt,
      `Q(h_max)` = Q_fmt,
      `p_Q(h_max)` = pQ_fmt
    )
  
  kable(
    tab_print,
    booktabs = TRUE,
    caption = paste0(
      "M-TAR (tau = tau_hat) lag selection over p = 0..", max(out_mtar$table$p),
      ". tau_hat estimated by grid search (AIC) with Ljung–Box filter. ",
      "Selected p* by ", ic_label, " subject to Ljung–Box p_Q(",
      out_mtar$lb_hmax, ") >= ", alpha_Q, " (white-noise residuals)."
    )
  ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8)
}

############################################################
## Example run
############################################################
mu_hat <- res$mu_hat
out_mtar <- run_mtar_table(mu_hat, p_tau = 0, p_max = 6, ic_p = "BIC", trim = 0.15, alpha_Q = 0.05, lb_hmax = 8)
out_mtar$tau_note
print_mtar_kable(out_mtar, ic_label = "BIC", alpha_Q = 0.05)


