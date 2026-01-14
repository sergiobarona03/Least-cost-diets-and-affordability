############################################################
## replicate_enders_siklos.R
## Replicate Enders–Granger / Enders–Siklos results
## using auxiliary functions
############################################################

rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)

#-----------------------------------------------------------
# 0. Source auxiliary functions
#-----------------------------------------------------------

setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/")

source("working-papers/working-paper-aecm/literature/enders_siklos/aux-functions/aux-utils.R")
source("working-papers/working-paper-aecm/literature/enders_siklos/aux-functions/aux-tar-level.R")
source("working-papers/working-paper-aecm/literature/enders_siklos/aux-functions/aux-tar-momentum.R")
source("working-papers/working-paper-aecm/literature/enders_siklos/aux-functions/aux-mtar-threshold.R")
source("working-papers/working-paper-aecm/literature/enders_siklos/aux-functions/aux-ecm-mtar.R")

#-----------------------------------------------------------
# 1. Load data and cointegration residuals
#-----------------------------------------------------------


path_rates <- "working-papers/working-paper-aecm/literature/enders_siklos/rates.xlsx"

df_rates <- load_rates_data(
  path_rates  = path_rates,
  start_date  = as.Date("1979-10-01")
)

coint_obj <- get_coint_residual(df_rates,
                                dep_var = "fed_funds",
                                reg_var = "g_10")

y  <- coint_obj$y
Tn <- coint_obj$Tn
reg_det <- coint_obj$reg

cat("Cointegration regression:\n")
print(summary(reg_det))

#-----------------------------------------------------------
# 2. Linear-attractor TAR: indicator on y_{t−1}  (Script 1)
#-----------------------------------------------------------

tar_level_p0 <- estimate_linear_attractor_level(y, p = 0)
tar_level_p2 <- estimate_linear_attractor_level(y, p = 2)

results_tar_level <- tibble(
  model  = c("TAR level (p=0)", "TAR level (p=2)"),
  rho1   = c(tar_level_p0$rho1,  tar_level_p2$rho1),
  t_rho1 = c(tar_level_p0$t_rho1,tar_level_p2$t_rho1),
  rho2   = c(tar_level_p0$rho2,  tar_level_p2$rho2),
  t_rho2 = c(tar_level_p0$t_rho2,tar_level_p2$t_rho2),
  Phi_mu = c(tar_level_p0$Phi_mu,tar_level_p2$Phi_mu),
  F_equal = c(tar_level_p0$F_equal, tar_level_p2$F_equal),
  p_equal = c(tar_level_p0$p_equal, tar_level_p2$p_equal),
  AIC    = c(tar_level_p0$AIC,    tar_level_p2$AIC),
  BIC    = c(tar_level_p0$BIC,    tar_level_p2$BIC),
  Q4     = c(tar_level_p0$Q4,     tar_level_p2$Q4),
  p_Q4   = c(tar_level_p0$p_Q4,   tar_level_p2$p_Q4)
)

cat("\n===== Linear-attractor TAR (level indicator) =====\n")
print(results_tar_level, digits = 4)

#-----------------------------------------------------------
# 3. Linear-attractor TAR: momentum indicator (Script 2)
#-----------------------------------------------------------

tar_mom_p0 <- estimate_linear_attractor_momentum(y, p = 0)
tar_mom_p2 <- estimate_linear_attractor_momentum(y, p = 2)

results_tar_mom <- tibble(
  model  = c("TAR momentum (p=0)", "TAR momentum (p=2)"),
  rho1   = c(tar_mom_p0$rho1,  tar_mom_p2$rho1),
  t_rho1 = c(tar_mom_p0$t_rho1,tar_mom_p2$t_rho1),
  rho2   = c(tar_mom_p0$rho2,  tar_mom_p2$rho2),
  t_rho2 = c(tar_mom_p0$t_rho2,tar_mom_p2$t_rho2),
  Phi_mu = c(tar_mom_p0$Phi_mu,tar_mom_p2$Phi_mu),
  F_equal = c(tar_mom_p0$F_equal, tar_mom_p2$F_equal),
  p_equal = c(tar_mom_p0$p_equal, tar_mom_p2$p_equal),
  AIC    = c(tar_mom_p0$AIC,    tar_mom_p2$AIC),
  BIC    = c(tar_mom_p0$BIC,    tar_mom_p2$BIC),
  Q4     = c(tar_mom_p0$Q4,     tar_mom_p2$Q4),
  p_Q4   = c(tar_mom_p0$p_Q4,   tar_mom_p2$p_Q4)
)

cat("\n===== Linear-attractor TAR (momentum indicator) =====\n")
print(results_tar_mom, digits = 4)

#-----------------------------------------------------------
# 4. M-TAR: threshold search by AIC + Ljung–Box (Script 3)
#-----------------------------------------------------------

mtar_p0 <- search_tau_AIC_LB(y,
                             p       = 0,
                             trim    = 0.15,
                             alpha_Q = 0.05)

mtar_p2 <- search_tau_AIC_LB(y,
                             p       = 2,
                             trim    = 0.15,
                             alpha_Q = 0.05)

results_mtar <- tibble(
  model  = c("M-TAR AIC+Q (p=0)", "M-TAR AIC+Q (p=2)"),
  tau    = c(mtar_p0$tau,         mtar_p2$tau),
  rho1   = c(mtar_p0$rho1,        mtar_p2$rho1),
  t_rho1 = c(mtar_p0$t_rho1,      mtar_p2$t_rho1),
  rho2   = c(mtar_p0$rho2,        mtar_p2$rho2),
  t_rho2 = c(mtar_p0$t_rho2,      mtar_p2$t_rho2),
  Phi_mu = c(mtar_p0$Phi_mu,      mtar_p2$Phi_mu),
  F_equal = c(mtar_p0$F_equal,    mtar_p2$F_equal),
  p_equal = c(mtar_p0$p_equal,    mtar_p2$p_equal),
  AIC    = c(mtar_p0$AIC,         mtar_p2$AIC),
  BIC    = c(mtar_p0$BIC,         mtar_p2$BIC),
  Q4     = c(mtar_p0$Q4,          mtar_p2$Q4),
  p_Q4   = c(mtar_p0$p_Q4,        mtar_p2$p_Q4)
)

cat("\n===== M-TAR threshold selection (AIC + Ljung-Box) =====\n")
print(results_mtar, digits = 4)

# For ECM step we need tau.x from MTAR (e.g. p=0)
tau.x <- mtar_p2$tau
cat("\nSelected tau.x for ECM M-TAR (from p=0):", round(tau.x, 4), "\n")

#-----------------------------------------------------------
# 5. ECM M-TAR (Script 4)
#-----------------------------------------------------------

df_rates <- df_rates %>%
  mutate(
    r_f  = log(fed_funds),
    r_10 = log(g_10)
  )

# Cointegrating regression for ECM (r_f ~ 1 + r_10)
reg_coint <- lm(r_f ~ 1 + r_10, data = df_rates)
cat("\nCointegration (ECM step):\n")
print(summary(reg_coint))

mu_hat <- resid(reg_coint)

df_ecm <- build_ecm_mtar_df(mu_hat = mu_hat,
                            r_f    = df_rates$r_f,
                            r_10   = df_rates$r_10,
                            tau_x  = tau.x,
                            p_lags = 1)

ecm_fit <- estimate_ecm_mtar(df_ecm, p_lags = 1)

cat("\n=== ECM for Δr_f,t (Fed Funds) ===\n")
print(summary(ecm_fit$mod_f))

cat("\n=== ECM for Δr_10,t (10-year bond) ===\n")
print(summary(ecm_fit$mod_10))

cat("\n=== F-tests A_ij(L) = 0 (with intercept) ===\n")
print(ecm_fit$F_tests, digits = 4)

cat("\n=== Asymmetric error-correction coefficients ===\n")
print(ecm_fit$ecm_results, digits = 4)
