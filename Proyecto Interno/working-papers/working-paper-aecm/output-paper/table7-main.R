############################################################
## Asymmetric cointegration on YOUR foods dataset
## - EG residuals: mu_hat from log_sipsa ~ 1 + log_ipc
## - TAR level (tau = 0): I_t = 1{ mu_{t-1} >= 0 }
## - M-TAR consistent (tau = tau_hat): I_t = 1{ Δmu_{t-1} >= tau_hat }
##   tau_hat by trimmed grid search (AIC) + Ljung-Box filter
## - M-TAR momentum (tau = 0): I_t = 1{ Δmu_{t-1} >= 0 }
## - "EViews-style" Ljung-Box: Q(1)..Q(h_max), report Q(h_max)
############################################################

library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)

#-----------------------------------------------------------
# 0) Load your dataset
#-----------------------------------------------------------

setwd("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/")

foods <- read_excel("working-papers/working-paper-aecm/input/261225_selected_foods_dataset.xlsx")

foods <- foods %>%
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", as.integer(Year), as.integer(Month))),
    precio_ipc   = as.numeric(precio_ipc),
    precio_sipsa = as.numeric(precio_sipsa),
    log_ipc   = log(precio_ipc),
    log_sipsa = log(precio_sipsa)
  ) %>%
  arrange(cod_mun, articulo_ipc, alimento_sipsa, date)

#-----------------------------------------------------------
# Helpers: Enders IC + stars + Ljung-Box EViews-style
#-----------------------------------------------------------

ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

t_stars <- function(t) {
  ifelse(is.na(t), "",
         ifelse(abs(t) >= 2.58, "***",
                ifelse(abs(t) >= 1.96, "**",
                       ifelse(abs(t) >= 1.65, "*", ""))))
}

ljung_box_eviews <- function(e, h_max = 8) {
  out <- tibble(h = 1:h_max, Q = NA_real_, p = NA_real_)
  for (hh in 1:h_max) {
    bt <- Box.test(e, lag = hh, type = "Ljung-Box")
    out$Q[out$h == hh] <- as.numeric(bt$statistic)
    out$p[out$h == hh] <- bt$p.value
  }
  list(table = out,
       Q_last = out$Q[out$h == h_max],
       p_last = out$p[out$h == h_max])
}

#-----------------------------------------------------------
# 1) EG residuals for one group
#-----------------------------------------------------------

get_mu_hat <- function(df_group) {
  df_group <- df_group %>% arrange(date)
  reg <- lm(log_sipsa ~ 1 + log_ipc, data = df_group)
  mu_hat <- resid(reg)
  list(reg = reg, mu_hat = mu_hat)
}

#-----------------------------------------------------------
# 2) TAR level (tau = 0): I_t = 1{mu_{t-1} >= 0}
#     with EViews-style Ljung-Box Q(h_max)
#-----------------------------------------------------------

build_tar_level_df_tau0 <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu   <- c(NA, diff(mu_hat))
  mu_l1 <- c(NA, mu_hat[-Tn])
  
  I  <- ifelse(mu_l1 >= 0, 1, 0)
  z1 <- I * mu_l1
  z2 <- (1 - I) * mu_l1
  
  dfm <- data.frame(dmu = dmu, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dmu_l", j)]] <- c(rep(NA, j), dmu[1:(Tn - j)])
    }
  }
  
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20) stop("Insufficient observations after lagging.")
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
  
  # Phi_mu: H0 rho1 = rho2 = 0
  if (p > 0) {
    rhs_R <- paste0("dmu_l", 1:p, collapse = " + ")
    fml_R <- as.formula(paste("dmu ~ 0 +", rhs_R))
  } else {
    fml_R <- dmu ~ 0
  }
  mod_R <- lm(fml_R, data = dfm)
  SSR_R <- sum(resid(mod_R)^2)
  
  Phi_mu <- ((SSR_R - SSR_U)/2) / (SSR_U/(Tn_m - kU))
  
  # Equality: H0 rho1 = rho2
  dfm_eq <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  mod_eq <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + "))), data = dfm_eq)
  an <- anova(mod_eq, mod)
  
  coefs <- coef(s)
  rho1  <- coefs["z1", "Estimate"]; trho1 <- coefs["z1", "t value"]
  rho2  <- coefs["z2", "Estimate"]; trho2 <- coefs["z2", "t value"]
  
  ic <- ic_enders(SSR_U, Tn_m, kU)
  
  lb <- ljung_box_eviews(resid(mod), h_max = h_max)
  
  list(p = p,
       rho1 = rho1, t_rho1 = trho1,
       rho2 = rho2, t_rho2 = trho2,
       Phi_mu = Phi_mu,
       F_equal = an$F[2], p_equal = an$`Pr(>F)`[2],
       AIC = ic$AIC, BIC = ic$BIC,
       Qh = lb$Q_last, p_Qh = lb$p_last)
}

select_p_tar_level_tau0 <- function(mu_hat, p_max = 6, ic = c("BIC","AIC"),
                                    alpha_Q = 0.05, h_max = 8) {
  ic <- match.arg(ic)
  
  tab <- tibble(
    p = 0:p_max,
    rho1 = NA_real_, t_rho1 = NA_real_,
    rho2 = NA_real_, t_rho2 = NA_real_,
    Phi_mu = NA_real_,
    F_equal = NA_real_, p_equal = NA_real_,
    AIC = NA_real_, BIC = NA_real_,
    Qh = NA_real_, p_Qh = NA_real_
  )
  
  fits <- vector("list", p_max + 1)
  
  for (pp in 0:p_max) {
    fit <- estimate_tar_level_tau0(mu_hat, p = pp, h_max = h_max)
    fits[[pp + 1]] <- fit
    
    tab[pp + 1, c("rho1","t_rho1","rho2","t_rho2","Phi_mu",
                  "F_equal","p_equal","AIC","BIC","Qh","p_Qh")] <-
      list(fit$rho1, fit$t_rho1, fit$rho2, fit$t_rho2, fit$Phi_mu,
           fit$F_equal, fit$p_equal, fit$AIC, fit$BIC, fit$Qh, fit$p_Qh)
  }
  
  ok <- tab %>% filter(p_Qh >= alpha_Q)
  p_star <- if (nrow(ok) > 0) ok$p[which.min(ok[[ic]])] else tab$p[which.min(tab[[ic]])]
  
  list(p_star = p_star, best = fits[[p_star + 1]], table = tab)
}

#-----------------------------------------------------------
# 3) M-TAR consistent: tau estimated by grid search
#     I_t = 1{Δmu_{t-1} >= tau_hat}
#-----------------------------------------------------------

build_mtar_base <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu    <- c(NA, diff(mu_hat))
  mu_l1  <- c(NA, mu_hat[-Tn])
  dmu_l1 <- c(NA, diff(mu_l1))  # threshold regressor
  
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

build_mtar_df <- function(base_df, p = 0, tau = 0, trim = 0.15) {
  
  I <- ifelse(base_df$dmu_l1 >= tau, 1, 0)
  
  # Regime size check
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
  if (var(dfm$z1) == 0 || var(dfm$z2) == 0) return(NULL)
  
  dfm
}

estimate_mtar_given_tau <- function(base_df, p = 0, tau = 0, trim = 0.15, h_max = 8) {
  
  dfm <- build_mtar_df(base_df, p = p, tau = tau, trim = trim)
  if (is.null(dfm)) {
    return(list(tau = tau, AIC = Inf, BIC = Inf, p_Qh = NA))
  }
  
  rhs <- c("z1","z2")
  if (p > 0) rhs <- c(rhs, paste0("dmu_l", 1:p))
  mod <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs, collapse = " + "))), data = dfm)
  s   <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  
  # Phi_mu: H0 rho1=rho2=0
  if (p > 0) {
    fml_R <- as.formula(paste("dmu ~ 0 +", paste0("dmu_l", 1:p, collapse = " + ")))
  } else {
    fml_R <- dmu ~ 0
  }
  mod_R <- lm(fml_R, data = dfm)
  SSR_R <- sum(resid(mod_R)^2)
  
  Phi_mu <- ((SSR_R - SSR_U)/2) / (SSR_U/(Tn_m - kU))
  
  # Equality: H0 rho1=rho2
  dfm_eq <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  mod_eq <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + "))), data = dfm_eq)
  an <- anova(mod_eq, mod)
  
  coefs <- coef(s)
  rho1  <- coefs["z1", "Estimate"]; trho1 <- coefs["z1", "t value"]
  rho2  <- coefs["z2", "Estimate"]; trho2 <- coefs["z2", "t value"]
  
  ic <- ic_enders(SSR_U, Tn_m, kU)
  
  lb <- ljung_box_eviews(resid(mod), h_max = h_max)
  
  list(
    tau = tau,
    p = p,
    rho1 = rho1, t_rho1 = trho1,
    rho2 = rho2, t_rho2 = trho2,
    Phi_mu = Phi_mu,
    F_equal = an$F[2], p_equal = an$`Pr(>F)`[2],
    AIC = ic$AIC, BIC = ic$BIC,
    Qh = lb$Q_last, p_Qh = lb$p_last
  )
}

search_tau_mtar <- function(mu_hat, p = 0, trim = 0.15, alpha_Q = 0.05, h_max = 8) {
  
  base_df <- build_mtar_base(mu_hat, p = p)
  z <- sort(base_df$dmu_l1)
  
  T_eff <- length(z)
  m <- floor(trim * T_eff)
  tau_grid <- unique(z[(m + 1):(T_eff - m)])
  
  grid_res <- map_dfr(tau_grid, ~{
    fit <- estimate_mtar_given_tau(base_df, p = p, tau = .x, trim = trim, h_max = h_max)
    tibble(tau = .x, AIC = fit$AIC, p_Qh = fit$p_Qh)
  })
  
  ok <- grid_res %>% filter(p_Qh >= alpha_Q, is.finite(AIC))
  tau_hat <- if (nrow(ok) > 0) ok$tau[which.min(ok$AIC)] else grid_res$tau[which.min(grid_res$AIC)]
  
  tau_hat
}

run_mtar_consistent_table <- function(mu_hat, p_tau = 0, p_max = 6, ic_p = c("BIC","AIC"),
                                      trim = 0.15, alpha_Q = 0.05, h_max = 8) {
  
  ic_p <- match.arg(ic_p)
  
  tau_hat <- search_tau_mtar(mu_hat, p = p_tau, trim = trim, alpha_Q = alpha_Q, h_max = h_max)
  
  tab <- tibble(
    p = 0:p_max,
    tau_hat = tau_hat,
    rho1 = NA_real_, t_rho1 = NA_real_,
    rho2 = NA_real_, t_rho2 = NA_real_,
    Phi_mu = NA_real_,
    F_equal = NA_real_, p_equal = NA_real_,
    AIC = NA_real_, BIC = NA_real_,
    Qh = NA_real_, p_Qh = NA_real_
  )
  
  fits <- vector("list", p_max + 1)
  
  for (pp in 0:p_max) {
    base_df <- build_mtar_base(mu_hat, p = pp)
    fit <- estimate_mtar_given_tau(base_df, p = pp, tau = tau_hat, trim = trim, h_max = h_max)
    fits[[pp + 1]] <- fit
    
    tab[pp + 1, c("rho1","t_rho1","rho2","t_rho2","Phi_mu",
                  "F_equal","p_equal","AIC","BIC","Qh","p_Qh")] <-
      list(fit$rho1, fit$t_rho1, fit$rho2, fit$t_rho2, fit$Phi_mu,
           fit$F_equal, fit$p_equal, fit$AIC, fit$BIC, fit$Qh, fit$p_Qh)
  }
  
  ok <- tab %>% filter(p_Qh >= alpha_Q)
  p_star <- if (nrow(ok) > 0) ok$p[which.min(ok[[ic_p]])] else tab$p[which.min(tab[[ic_p]])]
  
  list(tau_hat = tau_hat, p_star = p_star, best = fits[[p_star + 1]], table = tab)
}

#-----------------------------------------------------------
# 4) M-TAR momentum (tau = 0): I_t = 1{Δmu_{t-1} >= 0}
#     (same as TAR but indicator uses Δmu_{t-1})
#-----------------------------------------------------------

build_mtar_momentum_df_tau0 <- function(mu_hat, p = 0) {
  mu_hat <- as.numeric(mu_hat)
  Tn <- length(mu_hat)
  
  dmu   <- c(NA, diff(mu_hat))
  mu_l1 <- c(NA, mu_hat[-Tn])
  dmu_l1 <- c(NA, diff(mu_l1))
  
  I <- ifelse(dmu_l1 >= 0, 1, 0)
  
  z1 <- I * mu_l1
  z2 <- (1 - I) * mu_l1
  
  dfm <- data.frame(dmu = dmu, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dmu_l", j)]] <- c(rep(NA, j), dmu[1:(Tn - j)])
    }
  }
  
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20) stop("Insufficient observations after lagging.")
  dfm
}

estimate_mtar_momentum_tau0 <- function(mu_hat, p = 0, h_max = 8) {
  
  dfm <- build_mtar_momentum_df_tau0(mu_hat, p = p)
  
  rhs <- c("z1","z2")
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
  Phi_mu <- ((SSR_R - SSR_U)/2) / (SSR_U/(Tn_m - kU))
  
  # Equality
  dfm_eq <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dmu_l", 1:p))
  mod_eq <- lm(as.formula(paste("dmu ~ 0 +", paste(rhs_eq, collapse = " + "))), data = dfm_eq)
  an <- anova(mod_eq, mod)
  
  coefs <- coef(s)
  rho1 <- coefs["z1","Estimate"]; trho1 <- coefs["z1","t value"]
  rho2 <- coefs["z2","Estimate"]; trho2 <- coefs["z2","t value"]
  
  ic <- ic_enders(SSR_U, Tn_m, kU)
  lb <- ljung_box_eviews(resid(mod), h_max = h_max)
  
  list(
    p = p, tau = 0,
    rho1 = rho1, t_rho1 = trho1,
    rho2 = rho2, t_rho2 = trho2,
    Phi_mu = Phi_mu,
    F_equal = an$F[2], p_equal = an$`Pr(>F)`[2],
    AIC = ic$AIC, BIC = ic$BIC,
    Qh = lb$Q_last, p_Qh = lb$p_last
  )
}

select_p_mtar_momentum_tau0 <- function(mu_hat, p_max = 6, ic = c("BIC","AIC"),
                                        alpha_Q = 0.05, h_max = 8) {
  ic <- match.arg(ic)
  
  tab <- tibble(
    p = 0:p_max,
    rho1 = NA_real_, t_rho1 = NA_real_,
    rho2 = NA_real_, t_rho2 = NA_real_,
    Phi_mu = NA_real_,
    F_equal = NA_real_, p_equal = NA_real_,
    AIC = NA_real_, BIC = NA_real_,
    Qh = NA_real_, p_Qh = NA_real_
  )
  
  fits <- vector("list", p_max + 1)
  
  for (pp in 0:p_max) {
    fit <- estimate_mtar_momentum_tau0(mu_hat, p = pp, h_max = h_max)
    fits[[pp + 1]] <- fit
    
    tab[pp + 1, c("rho1","t_rho1","rho2","t_rho2","Phi_mu",
                  "F_equal","p_equal","AIC","BIC","Qh","p_Qh")] <-
      list(fit$rho1, fit$t_rho1, fit$rho2, fit$t_rho2, fit$Phi_mu,
           fit$F_equal, fit$p_equal, fit$AIC, fit$BIC, fit$Qh, fit$p_Qh)
  }
  
  ok <- tab %>% filter(p_Qh >= alpha_Q)
  p_star <- if (nrow(ok) > 0) ok$p[which.min(ok[[ic]])] else tab$p[which.min(tab[[ic]])]
  
  list(p_star = p_star, best = fits[[p_star + 1]], table = tab)
}

#-----------------------------------------------------------
# 5) One-stop runner for one group: EG + TAR + M-TARs
#-----------------------------------------------------------

run_asym_coint_one_group <- function(df_group,
                                     p_max = 6,
                                     ic_lag = "BIC",
                                     alpha_Q = 0.05,
                                     h_max = 8,
                                     trim = 0.15,
                                     p_tau = 0) {
  
  # EG residuals
  eg <- get_mu_hat(df_group)
  mu_hat <- eg$mu_hat
  
  # TAR level (tau=0)
  tar_level <- select_p_tar_level_tau0(mu_hat, p_max = p_max, ic = ic_lag,
                                       alpha_Q = alpha_Q, h_max = h_max)
  
  # M-TAR consistent (tau estimated)
  mtar_cons <- run_mtar_consistent_table(mu_hat, p_tau = p_tau, p_max = p_max,
                                         ic_p = ic_lag, trim = trim,
                                         alpha_Q = alpha_Q, h_max = h_max)
  
  # M-TAR momentum (tau=0)
  mtar_mom <- select_p_mtar_momentum_tau0(mu_hat, p_max = p_max, ic = ic_lag,
                                          alpha_Q = alpha_Q, h_max = h_max)
  
  list(
    eg_reg = eg$reg,
    mu_hat = mu_hat,
    tar_level = tar_level,
    mtar_consistent = mtar_cons,
    mtar_momentum = mtar_mom
  )
}

#-----------------------------------------------------------
# 6) Example: run for ONE city + ONE item mapping
#    (pick any existing combo from your foods)
#-----------------------------------------------------------

one_df <- foods %>%
  filter(cod_mun == first(cod_mun),
         articulo_ipc == first(articulo_ipc)) %>%
  arrange(date)

out_one <- run_asym_coint_one_group(one_df, p_max = 6, ic_lag = "BIC", h_max = 8)

# Quick check:
out_one$tar_level$p_star
out_one$mtar_consistent$tau_hat
out_one$mtar_consistent$p_star
out_one$mtar_momentum$p_star
