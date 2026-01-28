############################################################
## aux-ecm-mtar.R
## Asymmetric ECM M-TAR for Fed funds and 10-year bond
############################################################

#-----------------------------------------------------------
# Build ECM M-TAR dataset given:
#   - mu_hat: cointegrating residual μ̂_t
#   - r_f:    short rate (log Fed funds)
#   - r_10:   long rate (log 10-year bond)
#   - tau_x:  threshold for Δμ̂_{t−1}
#   - p_lags: number of lags in A_ij(L)
#-----------------------------------------------------------

build_ecm_mtar_df <- function(mu_hat,
                              r_f,
                              r_10,
                              tau_x,
                              p_lags = 1) {
  Tn <- length(mu_hat)
  
  # Δμ̂_t, its lag and μ̂_{t−1}
  d_mu    <- c(NA, diff(mu_hat))
  d_mu_l1 <- c(NA, d_mu[-length(d_mu)])
  mu_l1   <- c(NA, mu_hat[-length(mu_hat)])
  
  # Heaviside indicator on Δμ̂_{t−1}
  M_t <- ifelse(d_mu_l1 >= tau_x, 1, 0)
  
  mu_minus_l1 <- M_t       * mu_l1
  mu_plus_l1  <- (1 - M_t) * mu_l1
  
  dr_f  <- c(NA, diff(r_f))
  dr_10 <- c(NA, diff(r_10))
  
  df_ecm <- tibble(
    dr_f        = dr_f,
    dr_10       = dr_10,
    mu_minus_l1 = mu_minus_l1,
    mu_plus_l1  = mu_plus_l1
  )
  
  for (j in 1:p_lags) {
    df_ecm[[paste0("dr_f_l", j)]]  <- c(rep(NA, j), dr_f[1:(Tn - j)])
    df_ecm[[paste0("dr_10_l", j)]] <- c(rep(NA, j), dr_10[1:(Tn - j)])
  }
  
  df_ecm <- df_ecm %>% drop_na()
  df_ecm
}

#-----------------------------------------------------------
# Estimate ECM M-TAR system:
#   Eq1: Δr_f,t
#   Eq2: Δr_10,t
#   Intercepts included, A_ij(L) with p_lags
#   Returns: models, F-tests, EC coefficients summary
#-----------------------------------------------------------

estimate_ecm_mtar <- function(df_ecm, p_lags = 1) {
  
  rhs_lags <- c(paste0("dr_f_l", 1:p_lags),
                paste0("dr_10_l", 1:p_lags))
  rhs_all <- c(rhs_lags, "mu_minus_l1", "mu_plus_l1")
  
  # Short rate equation
  fml_f <- as.formula(
    paste("dr_f ~ 1 +", paste(rhs_all, collapse = " + "))
  )
  mod_f <- lm(fml_f, data = df_ecm)
  
  # Long rate equation
  fml_10 <- as.formula(
    paste("dr_10 ~ 1 +", paste(rhs_all, collapse = " + "))
  )
  mod_10 <- lm(fml_10, data = df_ecm)
  
  # --- F-tests A_ij(L) = 0 ---
  # F_11: Δr_f lags in Δr_f eq.
  fml_f_noF <- update(
    fml_f,
    paste(". ~ 1 +",
          paste(c(paste0("dr_10_l", 1:p_lags),
                  "mu_minus_l1", "mu_plus_l1"),
                collapse = " + "))
  )
  mod_f_noF <- lm(fml_f_noF, data = df_ecm)
  an_f_11   <- anova(mod_f_noF, mod_f)
  F_11      <- an_f_11$F[2]
  p_11      <- an_f_11$`Pr(>F)`[2]
  
  # F_12: Δr_10 lags in Δr_f eq.
  fml_f_no10 <- update(
    fml_f,
    paste(". ~ 1 +",
          paste(c(paste0("dr_f_l", 1:p_lags),
                  "mu_minus_l1", "mu_plus_l1"),
                collapse = " + "))
  )
  mod_f_no10 <- lm(fml_f_no10, data = df_ecm)
  an_f_12    <- anova(mod_f_no10, mod_f)
  F_12       <- an_f_12$F[2]
  p_12       <- an_f_12$`Pr(>F)`[2]
  
  # F_21: Δr_f lags in Δr_10 eq.
  fml_10_noF <- update(
    fml_10,
    paste(". ~ 1 +",
          paste(c(paste0("dr_10_l", 1:p_lags),
                  "mu_minus_l1", "mu_plus_l1"),
                collapse = " + "))
  )
  mod_10_noF <- lm(fml_10_noF, data = df_ecm)
  an_10_21   <- anova(mod_10_noF, mod_10)
  F_21       <- an_10_21$F[2]
  p_21       <- an_10_21$`Pr(>F)`[2]
  
  # F_22: Δr_10 lags in Δr_10 eq.
  fml_10_no10 <- update(
    fml_10,
    paste(". ~ 1 +",
          paste(c(paste0("dr_f_l", 1:p_lags),
                  "mu_minus_l1", "mu_plus_l1"),
                collapse = " + "))
  )
  mod_10_no10 <- lm(fml_10_no10, data = df_ecm)
  an_10_22    <- anova(mod_10_no10, mod_10)
  F_22        <- an_10_22$F[2]
  p_22        <- an_10_22$`Pr(>F)`[2]
  
  coef_f  <- summary(mod_f)$coefficients
  coef_10 <- summary(mod_10)$coefficients
  
  ecm_results <- tibble(
    equation   = c("Δr_f,t", "Δr_f,t", "Δr_10,t", "Δr_10,t"),
    regressor  = c("mu_minus_l1", "mu_plus_l1",
                   "mu_minus_l1", "mu_plus_l1"),
    estimate   = c(coef_f["mu_minus_l1", "Estimate"],
                   coef_f["mu_plus_l1",  "Estimate"],
                   coef_10["mu_minus_l1","Estimate"],
                   coef_10["mu_plus_l1", "Estimate"]),
    t_value    = c(coef_f["mu_minus_l1", "t value"],
                   coef_f["mu_plus_l1",  "t value"],
                   coef_10["mu_minus_l1","t value"],
                   coef_10["mu_plus_l1", "t value"])
  )
  
  F_tests <- tibble(
    test = c("F_11", "F_12", "F_21", "F_22"),
    F    = c(F_11,   F_12,   F_21,   F_22),
    p    = c(p_11,   p_12,   p_21,   p_22)
  )
  
  list(
    mod_f       = mod_f,
    mod_10      = mod_10,
    F_tests     = F_tests,
    ecm_results = ecm_results
  )
}
