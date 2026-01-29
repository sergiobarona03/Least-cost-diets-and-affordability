############################################################
## Asymmetric cointegration pipeline (Enders–Siklos style)
## - Runs by group: city x food  (12 models = 3 cities x 4 foods)
## - For each group builds a "Table 7"-style summary:
##   (1) Engle–Granger (linear ECM)
##   (2) TAR level (tau=0)
##   (3) M-TAR momentum (tau=0)
##   (4) M-TAR momentum-consistent (tau=tau_hat by grid search)
##
## Notes:
## - Uses Enders information criteria: AIC = T log(SSR) + 2n, BIC = T log(SSR) + n log(T)
## - Uses Ljung-Box "EViews-style": compute Q(1..h_max) without fitdf to avoid NaNs
## - Produces one table per group (city-food). You get 12 tables.
############################################################

library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)
library(openxlsx)

##==========================================================
## 0) Load + prepare your dataset (adjust paths if needed)
##==========================================================

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

foods <- read_excel("working-papers/working-paper-aecm/input/261225_selected_foods_dataset.xlsx")

# Expected columns:
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

##==========================================================
## 1) Utilities
##==========================================================

# Enders-style IC
ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

# Stars for t-stats (optional display)
t_stars <- function(t) {
  ifelse(abs(t) >= 2.58, "***",
         ifelse(abs(t) >= 1.96, "**",
                ifelse(abs(t) >= 1.65, "*", "")))
}

# Ljung-Box "EViews-style" to avoid NaNs from fitdf issues
ljung_box_eviews <- function(e, h_max = 8) {
  out <- tibble(h = 1:h_max, Q = NA_real_, p = NA_real_)
  for (hh in 1:h_max) {
    bt <- Box.test(e, lag = hh, type = "Ljung-Box")
    out$Q[out$h == hh] <- as.numeric(bt$statistic)
    out$p[out$h == hh] <- bt$p.value
  }
  list(
    table  = out,
    Q_last = out$Q[out$h == h_max],
    p_last = out$p[out$h == h_max]
  )
}

##==========================================================
## 2) (A) Engle–Granger: cointegration regression + linear ECM
##==========================================================

# Build EG residuals mu_hat from cointegration regression:
# log_sipsa_t = a0 + a1 log_ipc_t + mu_hat_t
get_eg_residuals <- function(df_g) {
  reg <- lm(log_sipsa ~ 1 + log_ipc, data = df_g)
  mu_hat <- resid(reg)
  list(reg = reg, mu_hat = mu_hat)
}

# Linear ECM for Engle–Granger column:
# Δlog_sipsa_t = c + rho * mu_{t-1} + gamma1 Δlog_sipsa_{t-1} + gamma2 Δlog_ipc_{t-1} + u_t
# (This is a common ECM spec; adjust if your paper/pipeline uses another.)
estimate_ecm_eg <- function(df_g, mu_hat, p_lags = 1, h_max = 8) {
  
  y <- df_g$log_sipsa
  x <- df_g$log_ipc
  
  dy <- c(NA, diff(y))
  dx <- c(NA, diff(x))
  mu_l1 <- c(NA, mu_hat[-length(mu_hat)])
  
  dyl1 <- c(NA, dy[-length(dy)])
  dxl1 <- c(NA, dx[-length(dx)])
  
  df_ecm <- tibble(
    dy = dy,
    mu_l1 = mu_l1,
    dy_l1 = dyl1,
    dx_l1 = dxl1
  ) %>% drop_na()
  
  # Basic 1-lag ECM (as in many EG implementations)
  mod <- lm(dy ~ 1 + mu_l1 + dy_l1 + dx_l1, data = df_ecm)
  s <- summary(mod)
  
  SSR <- sum(resid(mod)^2)
  Tn_m <- nrow(df_ecm)
  kU <- length(coef(mod))
  
  ic <- ic_enders(SSR, Tn_m, kU)
  lb <- ljung_box_eviews(resid(mod), h_max = h_max)
  
  # Return EG-like pieces for "Table 7" column
  list(
    model = mod,
    rho1 = coef(s)["mu_l1", "Estimate"],         # in EG column this is the single adjustment rho
    t_rho1 = coef(s)["mu_l1", "t value"],
    rho2 = NA_real_,
    t_rho2 = NA_real_,
    gamma1 = coef(s)["dy_l1", "Estimate"],
    t_gamma1 = coef(s)["dy_l1", "t value"],
    gamma2 = coef(s)["dx_l1", "Estimate"],
    t_gamma2 = coef(s)["dx_l1", "t value"],
    AIC = ic$AIC,
    BIC = ic$BIC,
    Phi_mu = NA_real_,
    F_equal = NA_real_,
    p_equal = NA_real_,
    Qh = lb$Q_last,
    p_Qh = lb$p_last
  )
}

##==========================================================
## 3) (B) TAR level (tau = 0) on EG residuals mu_hat
##==========================================================

build_tar_level_df_tau0 <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu   <- c(NA, diff(mu_hat))
  mu_l1 <- c(NA, mu_hat[-Tn])
  
  I <- ifelse(mu_l1 >= 0, 1, 0)
  z1 <- I * mu_l1
  z2 <- (1 - I) * mu_l1
  
  dfm <- tibble(dmu = dmu, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dmu_l", j)]] <- c(rep(NA, j), dmu[1:(Tn - j)])
    }
  }
  
  dfm <- dfm %>% drop_na()
  if (nrow(dfm) < 20) stop("Insufficient observations after lagging (TAR level).")
  dfm
}

estimate_tar_level_tau0 <- function(mu_hat, p = 0, h_max = 8) {
  
  dfm <- build_tar_level_df_tau0(mu_hat, p = p)
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dmu_l", 1:p))
  fml <- as.formula(paste("dmu ~ 0 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  
  # Phi_mu: H0 rho1=rho2=0
  if (p > 0) {
    rhs_R <- paste0("dmu_l", 1:p, collapse = " + ")
    fml_R <- as.formula(paste("dmu ~ 0 +", rhs_R))
  } else {
    fml_R <- dmu ~ 0
  }
  mod_R <- lm(fml_R, data = dfm)
  SSR_R <- sum(resid(mod_R)^2)
  
  Phi_mu <- ((SSR_R - SSR_U) / 2) / (SSR_U / (Tn_m - kU))
  
  # Equality: H0 rho1=rho2
  dfm_eq <- dfm %>% mutate(z_sum = z1 + z2)
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  mod_eq <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + "))),
               data = dfm_eq)
  an <- anova(mod_eq, mod)
  
  ic <- ic_enders(SSR_U, Tn_m, kU)
  lb <- ljung_box_eviews(resid(mod), h_max = h_max)
  
  list(
    model = mod,
    rho1  = coef(s)["z1", "Estimate"],
    t_rho1 = coef(s)["z1", "t value"],
    rho2  = coef(s)["z2", "Estimate"],
    t_rho2 = coef(s)["z2", "t value"],
    gamma1 = if (p >= 1) coef(mod)[paste0("dmu_l", 1)] else NA_real_,
    t_gamma1 = if (p >= 1) summary(mod)$coefficients[paste0("dmu_l", 1), "t value"] else NA_real_,
    gamma2 = if (p >= 2) coef(mod)[paste0("dmu_l", 2)] else NA_real_,
    t_gamma2 = if (p >= 2) summary(mod)$coefficients[paste0("dmu_l", 2), "t value"] else NA_real_,
    AIC = ic$AIC,
    BIC = ic$BIC,
    Phi_mu = Phi_mu,
    F_equal = an$F[2],
    p_equal = an$`Pr(>F)`[2],
    Qh = lb$Q_last,
    p_Qh = lb$p_last
  )
}

select_p_tar_level_tau0 <- function(mu_hat,
                                    p_max = 6,
                                    ic = c("BIC","AIC"),
                                    alpha_Q = 0.05,
                                    h_max = 8) {
  ic <- match.arg(ic)
  
  fits <- vector("list", p_max + 1)
  tab <- tibble(
    p = 0:p_max,
    AIC = NA_real_, BIC = NA_real_,
    p_Qh = NA_real_
  )
  
  for (p in 0:p_max) {
    fit_p <- estimate_tar_level_tau0(mu_hat, p = p, h_max = h_max)
    fits[[p + 1]] <- fit_p
    tab[p + 1, c("AIC","BIC","p_Qh")] <- list(fit_p$AIC, fit_p$BIC, fit_p$p_Qh)
  }
  
  ok <- tab %>% filter(p_Qh >= alpha_Q)
  if (nrow(ok) > 0) {
    p_star <- ok$p[which.min(ok[[ic]])]
  } else {
    p_star <- tab$p[which.min(tab[[ic]])]
  }
  
  list(p_star = p_star, best = fits[[p_star + 1]])
}

##==========================================================
## 4) (C) M-TAR momentum (tau = 0) on EG residuals mu_hat
##==========================================================

build_mtar_mom_df_tau0 <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu   <- c(NA, diff(mu_hat))
  mu_l1 <- c(NA, mu_hat[-Tn])
  dmu_l1 <- c(NA, diff(mu_l1))  # Δmu_{t-1}
  
  I <- ifelse(dmu_l1 >= 0, 1, 0)  # tau = 0 on momentum
  z1 <- I * mu_l1
  z2 <- (1 - I) * mu_l1
  
  dfm <- tibble(dmu = dmu, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dmu_l", j)]] <- c(rep(NA, j), dmu[1:(Tn - j)])
    }
  }
  
  dfm <- dfm %>% drop_na()
  if (nrow(dfm) < 20) stop("Insufficient observations after lagging (MTAR momentum).")
  dfm
}

estimate_mtar_mom_tau0 <- function(mu_hat, p = 0, h_max = 8) {
  
  dfm <- build_mtar_mom_df_tau0(mu_hat, p = p)
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dmu_l", 1:p))
  mod <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs, collapse = " + "))), data = dfm)
  s <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m <- nrow(dfm)
  kU <- length(coef(mod))
  
  # Phi_mu: H0 rho1=rho2=0
  if (p > 0) {
    rhs_R <- paste0("dmu_l", 1:p, collapse = " + ")
    mod_R <- lm(as.formula(paste("dmu ~ 0 +", rhs_R)), data = dfm)
  } else {
    mod_R <- lm(dmu ~ 0, data = dfm)
  }
  SSR_R <- sum(resid(mod_R)^2)
  Phi_mu <- ((SSR_R - SSR_U) / 2) / (SSR_U / (Tn_m - kU))
  
  # Equality: H0 rho1=rho2
  dfm_eq <- dfm %>% mutate(z_sum = z1 + z2)
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  mod_eq <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + "))), data = dfm_eq)
  an <- anova(mod_eq, mod)
  
  ic <- ic_enders(SSR_U, Tn_m, kU)
  lb <- ljung_box_eviews(resid(mod), h_max = h_max)
  
  list(
    model = mod,
    rho1  = coef(s)["z1", "Estimate"],
    t_rho1 = coef(s)["z1", "t value"],
    rho2  = coef(s)["z2", "Estimate"],
    t_rho2 = coef(s)["z2", "t value"],
    gamma1 = if (p >= 1) coef(mod)[paste0("dmu_l", 1)] else NA_real_,
    t_gamma1 = if (p >= 1) summary(mod)$coefficients[paste0("dmu_l", 1), "t value"] else NA_real_,
    gamma2 = if (p >= 2) coef(mod)[paste0("dmu_l", 2)] else NA_real_,
    t_gamma2 = if (p >= 2) summary(mod)$coefficients[paste0("dmu_l", 2), "t value"] else NA_real_,
    AIC = ic$AIC,
    BIC = ic$BIC,
    Phi_mu = Phi_mu,
    F_equal = an$F[2],
    p_equal = an$`Pr(>F)`[2],
    Qh = lb$Q_last,
    p_Qh = lb$p_last
  )
}

select_p_mtar_mom_tau0 <- function(mu_hat,
                                   p_max = 6,
                                   ic = c("BIC","AIC"),
                                   alpha_Q = 0.05,
                                   h_max = 8) {
  ic <- match.arg(ic)
  
  fits <- vector("list", p_max + 1)
  tab <- tibble(p = 0:p_max, AIC = NA_real_, BIC = NA_real_, p_Qh = NA_real_)
  
  for (p in 0:p_max) {
    fit_p <- estimate_mtar_mom_tau0(mu_hat, p = p, h_max = h_max)
    fits[[p + 1]] <- fit_p
    tab[p + 1, c("AIC","BIC","p_Qh")] <- list(fit_p$AIC, fit_p$BIC, fit_p$p_Qh)
  }
  
  ok <- tab %>% filter(p_Qh >= alpha_Q)
  if (nrow(ok) > 0) {
    p_star <- ok$p[which.min(ok[[ic]])]
  } else {
    p_star <- tab$p[which.min(tab[[ic]])]
  }
  
  list(p_star = p_star, best = fits[[p_star + 1]])
}

##==========================================================
## 5) (D) M-TAR momentum-consistent: tau estimated by grid
##==========================================================

# Base MTAR variables for tau grid
build_mtar_base <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu    <- c(NA, diff(mu_hat))
  mu_l1  <- c(NA, mu_hat[-Tn])
  dmu_l1 <- c(NA, diff(mu_l1))  # threshold regressor
  
  dfb <- tibble(dmu = dmu, mu_l1 = mu_l1, dmu_l1 = dmu_l1)
  
  if (p > 0) {
    for (j in 1:p) {
      dfb[[paste0("dmu_l", j)]] <- c(rep(NA, j), dmu[1:(Tn - j)])
    }
  }
  
  dfb <- dfb %>% drop_na()
  if (nrow(dfb) < 20) stop("Insufficient observations after lagging (MTAR base).")
  dfb
}

# Build MTAR df for a given tau (trim regimes)
build_mtar_df <- function(base_df, p = 0, tau = 0, trim = 0.15) {
  I <- ifelse(base_df$dmu_l1 >= tau, 1, 0)
  
  if (mean(I == 1) < trim || mean(I == 0) < trim) return(NULL)
  
  z1 <- I * base_df$mu_l1
  z2 <- (1 - I) * base_df$mu_l1
  
  dfm <- tibble(dmu = base_df$dmu, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dmu_l", j)]] <- base_df[[paste0("dmu_l", j)]]
    }
  }
  
  dfm <- dfm %>% drop_na()
  if (nrow(dfm) < 20) return(NULL)
  if (var(dfm$z1) == 0 || var(dfm$z2) == 0) return(NULL)
  
  dfm
}

estimate_mtar_given_tau <- function(base_df, p = 0, tau = 0, trim = 0.15, h_max = 8) {
  
  dfm <- build_mtar_df(base_df, p = p, tau = tau, trim = trim)
  if (is.null(dfm)) {
    return(list(tau=tau, AIC=Inf, BIC=Inf, p_Qh=NA, SSR=Inf))
  }
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dmu_l", 1:p))
  mod <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs, collapse = " + "))), data = dfm)
  s <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m <- nrow(dfm)
  kU <- length(coef(mod))
  
  # Phi_mu
  if (p > 0) {
    rhs_R <- paste0("dmu_l", 1:p, collapse = " + ")
    mod_R <- lm(as.formula(paste("dmu ~ 0 +", rhs_R)), data = dfm)
  } else {
    mod_R <- lm(dmu ~ 0, data = dfm)
  }
  SSR_R <- sum(resid(mod_R)^2)
  Phi_mu <- ((SSR_R - SSR_U) / 2) / (SSR_U / (Tn_m - kU))
  
  # Equality test
  dfm_eq <- dfm %>% mutate(z_sum = z1 + z2)
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  mod_eq <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + "))), data = dfm_eq)
  an <- anova(mod_eq, mod)
  
  ic <- ic_enders(SSR_U, Tn_m, kU)
  lb <- ljung_box_eviews(resid(mod), h_max = h_max)
  
  list(
    tau = tau,
    model = mod,
    rho1  = coef(s)["z1", "Estimate"],
    t_rho1 = coef(s)["z1", "t value"],
    rho2  = coef(s)["z2", "Estimate"],
    t_rho2 = coef(s)["z2", "t value"],
    gamma1 = if (p >= 1) coef(mod)[paste0("dmu_l", 1)] else NA_real_,
    t_gamma1 = if (p >= 1) summary(mod)$coefficients[paste0("dmu_l", 1), "t value"] else NA_real_,
    gamma2 = if (p >= 2) coef(mod)[paste0("dmu_l", 2)] else NA_real_,
    t_gamma2 = if (p >= 2) summary(mod)$coefficients[paste0("dmu_l", 2), "t value"] else NA_real_,
    Phi_mu = Phi_mu,
    F_equal = an$F[2],
    p_equal = an$`Pr(>F)`[2],
    AIC = ic$AIC,
    BIC = ic$BIC,
    Qh = lb$Q_last,
    p_Qh = lb$p_last,
    SSR = SSR_U
  )
}

# Tau grid (trimmed unique values)
build_tau_grid <- function(base_df, trim = 0.15) {
  z <- sort(base_df$dmu_l1)
  T_eff <- length(z)
  m <- floor(trim * T_eff)
  if (2*m >= T_eff) stop("Trim too large for effective sample.")
  unique(z[(m + 1):(T_eff - m)])
}

# Search tau_hat by AIC among those passing Ljung-Box p>=alpha
search_tau_AIC_LB <- function(mu_hat, p = 0, trim = 0.15, alpha_Q = 0.05, h_max = 8) {
  base_df <- build_mtar_base(mu_hat, p = p)
  grid <- build_tau_grid(base_df, trim = trim)
  
  fits <- vector("list", length(grid))
  aic_vec <- rep(Inf, length(grid))
  pQ_vec  <- rep(NA_real_, length(grid))
  
  for (j in seq_along(grid)) {
    fit_j <- estimate_mtar_given_tau(base_df, p = p, tau = grid[j], trim = trim, h_max = h_max)
    fits[[j]] <- fit_j
    aic_vec[j] <- fit_j$AIC
    pQ_vec[j]  <- fit_j$p_Qh
  }
  
  valid <- which(pQ_vec >= alpha_Q & is.finite(aic_vec))
  if (length(valid) > 0) {
    j_min <- valid[which.min(aic_vec[valid])]
  } else {
    j_min <- which.min(aic_vec)
  }
  
  fits[[j_min]]
}

# Given tau_hat, select p* by IC among those passing LB
select_p_mtar_given_tau <- function(mu_hat, tau_hat,
                                    p_max = 6, ic = c("BIC","AIC"),
                                    trim = 0.15, alpha_Q = 0.05, h_max = 8) {
  ic <- match.arg(ic)
  
  fits <- vector("list", p_max + 1)
  tab <- tibble(p = 0:p_max, AIC=NA_real_, BIC=NA_real_, p_Qh=NA_real_)
  
  for (p in 0:p_max) {
    base_df <- build_mtar_base(mu_hat, p = p)
    fit_p <- estimate_mtar_given_tau(base_df, p = p, tau = tau_hat, trim = trim, h_max = h_max)
    fits[[p + 1]] <- fit_p
    tab[p + 1, c("AIC","BIC","p_Qh")] <- list(fit_p$AIC, fit_p$BIC, fit_p$p_Qh)
  }
  
  ok <- tab %>% filter(p_Qh >= alpha_Q)
  if (nrow(ok) > 0) {
    p_star <- ok$p[which.min(ok[[ic]])]
  } else {
    p_star <- tab$p[which.min(tab[[ic]])]
  }
  
  list(p_star = p_star, best = fits[[p_star + 1]])
}

##==========================================================
## 6) Build a "Table 7"-style table for one city-food group
##==========================================================

fmt_coef <- function(b, t) {
  if (is.na(b) || is.na(t)) return(NA_character_)
  sprintf("%.4f (%.3f)", b, t)
}

make_table7_like <- function(eg_fit, tar_level_fit, mtar_mom_fit, mtar_cons_fit) {
  # This replicates the rows you showed (rho1, rho2, gamma1, gamma2, AIC, Phi, rho1=rho2)
  tibble(
    Row = c("rho1", "rho2", "gamma1", "gamma2", "AIC", "Phi", "rho1 = rho2"),
    `Engle–Granger` = c(
      fmt_coef(eg_fit$rho1, eg_fit$t_rho1),
      "NA",
      fmt_coef(eg_fit$gamma1, eg_fit$t_gamma1),
      fmt_coef(eg_fit$gamma2, eg_fit$t_gamma2),
      sprintf("%.3f", eg_fit$AIC),
      "NA",
      "NA"
    ),
    `Threshold` = c(
      fmt_coef(tar_level_fit$rho1, tar_level_fit$t_rho1),
      fmt_coef(tar_level_fit$rho2, tar_level_fit$t_rho2),
      fmt_coef(tar_level_fit$gamma1, tar_level_fit$t_gamma1),
      fmt_coef(tar_level_fit$gamma2, tar_level_fit$t_gamma2),
      sprintf("%.3f", tar_level_fit$AIC),
      sprintf("%.3f", tar_level_fit$Phi_mu),
      sprintf("%.3f (%.3f)", tar_level_fit$F_equal, tar_level_fit$p_equal)
    ),
    `Momentum` = c(
      fmt_coef(mtar_mom_fit$rho1, mtar_mom_fit$t_rho1),
      fmt_coef(mtar_mom_fit$rho2, mtar_mom_fit$t_rho2),
      fmt_coef(mtar_mom_fit$gamma1, mtar_mom_fit$t_gamma1),
      fmt_coef(mtar_mom_fit$gamma2, mtar_mom_fit$t_gamma2),
      sprintf("%.3f", mtar_mom_fit$AIC),
      sprintf("%.3f", mtar_mom_fit$Phi_mu),
      sprintf("%.3f (%.3f)", mtar_mom_fit$F_equal, mtar_mom_fit$p_equal)
    ),
    `Momentum-consistent` = c(
      fmt_coef(mtar_cons_fit$rho1, mtar_cons_fit$t_rho1),
      fmt_coef(mtar_cons_fit$rho2, mtar_cons_fit$t_rho2),
      fmt_coef(mtar_cons_fit$gamma1, mtar_cons_fit$t_gamma1),
      fmt_coef(mtar_cons_fit$gamma2, mtar_cons_fit$t_gamma2),
      sprintf("%.3f", mtar_cons_fit$AIC),
      sprintf("%.3f", mtar_cons_fit$Phi_mu),
      sprintf("%.3f (%.3f)", mtar_cons_fit$F_equal, mtar_cons_fit$p_equal)
    )
  )
}

##==========================================================
## 7) Main runner for ONE group
##==========================================================

run_asym_coint_one_group <- function(df_g,
                                     p_max = 6,
                                     ic_sel = "BIC",
                                     alpha_Q = 0.05,
                                     h_max = 8,
                                     trim = 0.15,
                                     p_tau = 0,
                                     ecm_lags = 1) {
  
  # 1) EG residuals
  eg <- get_eg_residuals(df_g)
  mu_hat <- eg$mu_hat
  
  # 2) EG ECM (linear)
  eg_ecm <- estimate_ecm_eg(df_g, mu_hat, p_lags = ecm_lags, h_max = h_max)
  
  # 3) TAR level: choose p*
  tar_sel <- select_p_tar_level_tau0(mu_hat, p_max = p_max, ic = ic_sel, alpha_Q = alpha_Q, h_max = h_max)
  tar_best <- tar_sel$best
  
  # 4) MTAR momentum tau=0: choose p*
  mom_sel <- select_p_mtar_mom_tau0(mu_hat, p_max = p_max, ic = ic_sel, alpha_Q = alpha_Q, h_max = h_max)
  mom_best <- mom_sel$best
  
  # 5) MTAR consistent: estimate tau_hat by grid (using p_tau), then choose p*
  tau_fit <- search_tau_AIC_LB(mu_hat, p = p_tau, trim = trim, alpha_Q = alpha_Q, h_max = h_max)
  tau_hat <- tau_fit$tau
  
  cons_sel <- select_p_mtar_given_tau(mu_hat, tau_hat, p_max = p_max, ic = ic_sel,
                                      trim = trim, alpha_Q = alpha_Q, h_max = h_max)
  cons_best <- cons_sel$best
  
  # 6) Table 7-like
  tab7 <- make_table7_like(eg_ecm, tar_best, mom_best, cons_best)
  
  list(
    mu_hat = mu_hat,
    eg_ecm = eg_ecm,
    tar = list(p_star = tar_sel$p_star, best = tar_best),
    mtar_momentum = list(p_star = mom_sel$p_star, best = mom_best),
    mtar_consistent = list(tau_hat = tau_hat, p_star = cons_sel$p_star, best = cons_best),
    table7 = tab7
  )
}

##==========================================================
## 8) Run ALL groups (3 cities x 4 foods = 12) and export
##==========================================================

# Choose which "food identifier" to use:
# - If each of the 4 foods is uniquely identified by articulo_ipc, use that.
# - If you need both, keep both in the grouping keys.
group_keys <- c("cod_mun", "articulo_ipc")

tables_12 <- foods %>%
  group_by(across(all_of(group_keys))) %>%
  group_split() %>%
  set_names(foods %>%
              group_by(across(all_of(group_keys))) %>%
              group_keys() %>%
              pmap_chr(~ paste(group_keys, c(...), sep="=", collapse="__"))) %>%
  map(~{
    df_g <- .x %>% arrange(date) %>% drop_na(log_sipsa, log_ipc)
    
    # Guardrails: need enough obs
    if (nrow(df_g) < 50) return(NULL)
    
    run_asym_coint_one_group(
      df_g,
      p_max = 6,
      ic_sel = "BIC",
      alpha_Q = 0.05,
      h_max = 8,
      trim = 0.15,
      p_tau = 0,
      ecm_lags = 1
    )
  })

# Keep only successful runs
tables_12 <- tables_12[!map_lgl(tables_12, is.null)]

# Example: print one table in the console
names(tables_12)[1]
tables_12[[1]]$table7


# Produce HTML tables (kable) for each group
 for (nm in names(tables_12)) {
   cat("\n\n### Table 7 - ", nm, "\n\n")
   print(kable(tables_12[[nm]]$table7, booktabs = TRUE) %>%
           kable_styling(full_width = FALSE))
   }
