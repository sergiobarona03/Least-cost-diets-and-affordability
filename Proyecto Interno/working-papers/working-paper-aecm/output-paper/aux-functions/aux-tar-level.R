############################################################
## aux-tar-level.R
## Linear Attractor TAR (indicator on y_{t-1})
############################################################

# Requires: ic_enders() from aux-utils.R

#-----------------------------------------------------------
# Build TAR regression dataset (level-based indicator)
#   I_t = 1{ y_{t-1} >= 0 }
#-----------------------------------------------------------

build_tar_df_level <- function(y, p = 0) {
  y  <- as.numeric(y)
  Tn <- length(y)
  
  dy   <- c(NA, diff(y))
  y_l1 <- c(NA, y[-Tn])
  
  # Heaviside indicator on level:
  I <- ifelse(y_l1 >= 0, 1, 0)
  
  z1 <- I       * y_l1
  z2 <- (1 - I) * y_l1
  
  dfm <- data.frame(dy = dy, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dy_l", j)]] <- c(rep(NA, j), dy[1:(Tn - j)])
    }
  }
  
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20) stop("Insufficient observations after lagging.")
  
  dfm
}

#-----------------------------------------------------------
# Estimate linear attractor TAR (level-based indicator)
#   - Φ_μ: H0: ρ1 = ρ2 = 0
#   - Equality test: H0: ρ1 = ρ2
#   - IC, Ljung–Box Q(4)
#-----------------------------------------------------------

estimate_linear_attractor_level <- function(y, p = 0) {
  
  dfm <- build_tar_df_level(y, p = p)
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  fml <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  
  # (A) Φ_μ: H0: ρ1 = ρ2 = 0
  if (p > 0) {
    rhs_R <- paste0("dy_l", 1:p, collapse = " + ")
    fml_R <- as.formula(paste("dy ~ 0 +", rhs_R))
  } else {
    fml_R <- dy ~ 0
  }
  
  mod_R <- lm(fml_R, data = dfm)
  SSR_R <- sum(resid(mod_R)^2)
  
  q      <- 2
  Phi_mu <- ((SSR_R - SSR_U) / q) / (SSR_U / (Tn_m - kU))
  
  # (B) Equality test: H0: ρ1 = ρ2
  dfm_eq       <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq       <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dy_l", 1:p))
  fml_eq <- as.formula(paste("dy ~ 0 +", paste(rhs_eq, collapse = " + ")))
  mod_eq <- lm(fml_eq, data = dfm_eq)
  
  an       <- anova(mod_eq, mod)
  F_equal  <- an$F[2]
  pF_equal <- an$`Pr(>F)`[2]
  
  coefs <- coef(s)
  rho1  <- coefs["z1", "Estimate"]
  trho1 <- coefs["z1", "t value"]
  rho2  <- coefs["z2", "Estimate"]
  trho2 <- coefs["z2", "t value"]
  
  ic <- ic_enders(SSR_U, Tn_m, kU)
  
  Q4 <- Box.test(resid(mod), lag = 4, type = "Ljung-Box", fitdf = kU)
  
  list(
    model   = mod,
    p       = p,
    rho1    = rho1,
    t_rho1  = trho1,
    rho2    = rho2,
    t_rho2  = trho2,
    Phi_mu  = Phi_mu,
    F_equal = F_equal,
    p_equal = pF_equal,
    AIC     = ic$AIC,
    BIC     = ic$BIC,
    Q4      = as.numeric(Q4$statistic),
    p_Q4    = Q4$p.value
  )
}
