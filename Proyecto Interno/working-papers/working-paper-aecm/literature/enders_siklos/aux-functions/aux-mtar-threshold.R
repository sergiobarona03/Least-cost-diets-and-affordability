############################################################
## aux-mtar-threshold.R
## M-TAR threshold search with AIC + Ljung–Box
############################################################

# Requires: ic_enders() from aux-utils.R

#-----------------------------------------------------------
# Base MTAR variables: Δy_t, y_{t−1}, Δy_{t−1}
#-----------------------------------------------------------

build_mtar_base <- function(y, p = 0) {
  y  <- as.numeric(y)
  Tn <- length(y)
  
  dy    <- c(NA, diff(y))        # Δy_t
  y_l1  <- c(NA, y[-Tn])         # y_{t−1}
  dy_l1 <- c(NA, diff(y_l1))     # Δy_{t−1} (threshold var.)
  
  dfb <- data.frame(dy = dy, y_l1 = y_l1, dy_l1 = dy_l1)
  
  if (p > 0) {
    for (j in 1:p) {
      dfb[[paste0("dy_l", j)]] <- c(rep(NA, j), dy[1:(Tn - j)])
    }
  }
  
  dfb <- dfb[complete.cases(dfb), , drop = FALSE]
  if (nrow(dfb) < 20) stop("Insufficient observations after lagging.")
  
  dfb
}

#-----------------------------------------------------------
# MTAR regression df for given tau
#   I_t = 1{ Δy_{t−1} >= tau }
#   Δy_t = ρ1 I_t y_{t−1} + ρ2 (1−I_t) y_{t−1} + lags + ε_t
#-----------------------------------------------------------

build_mtar_df <- function(base_df, p = 0, tau = 0, trim = 0.15) {
  dfb <- base_df
  
  dy    <- dfb$dy
  y_l1  <- dfb$y_l1
  dy_l1 <- dfb$dy_l1
  
  I <- ifelse(dy_l1 >= tau, 1, 0)
  
  share_high <- mean(I == 1)
  share_low  <- mean(I == 0)
  if (share_high < trim || share_low < trim) {
    return(NULL)
  }
  
  z1 <- I       * y_l1
  z2 <- (1 - I) * y_l1
  
  dfm <- data.frame(dy = dy, y_l1 = y_l1, dy_l1 = dy_l1,
                    z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dy_l", j)]] <- dfb[[paste0("dy_l", j)]]
    }
  }
  
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20) return(NULL)
  
  if (all(dfm$z1 == 0) || all(dfm$z2 == 0)) return(NULL)
  if (var(dfm$z1) == 0 || var(dfm$z2) == 0) return(NULL)
  
  dfm
}

#-----------------------------------------------------------
# Estimate MTAR for given tau
#   criterion: AIC (Enders), + Φ_μ*, equality test, Q(4)
#-----------------------------------------------------------

estimate_mtar_given_tau <- function(base_df, p = 0, tau = 0, trim = 0.15) {
  
  dfm <- build_mtar_df(base_df, p = p, tau = tau, trim = trim)
  
  if (is.null(dfm)) {
    return(list(
      tau      = tau,
      model    = NULL,
      p        = p,
      rho1     = NA,
      t_rho1   = NA,
      rho2     = NA,
      t_rho2   = NA,
      Phi_mu   = NA,
      F_equal  = NA,
      p_equal  = NA,
      AIC      = Inf,
      BIC      = Inf,
      Q4       = NA,
      p_Q4     = NA,
      SSR      = Inf
    ))
  }
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  fml <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  
  # (A) Φ_μ*: H0: ρ1 = ρ2 = 0
  if (p > 0) {
    rhs_R <- paste0("dy_l", 1:p, collapse = " + ")
    fml_R <- as.formula(paste("dy ~ 0 +", rhs_R))
  } else {
    fml_R <- dy ~ 1
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
    tau      = tau,
    model    = mod,
    p        = p,
    rho1     = rho1,
    t_rho1   = trho1,
    rho2     = rho2,
    t_rho2   = trho2,
    Phi_mu   = Phi_mu,
    F_equal  = F_equal,
    p_equal  = pF_equal,
    AIC      = ic$AIC,
    BIC      = ic$BIC,
    Q4       = as.numeric(Q4$statistic),
    p_Q4     = Q4$p.value,
    SSR      = SSR_U
  )
}

#-----------------------------------------------------------
# Threshold grid based on Δy_{t−1} with trim
#-----------------------------------------------------------

build_tau_grid <- function(base_df, trim = 0.15) {
  z <- sort(base_df$dy_l1)
  T_eff <- length(z)
  m <- floor(trim * T_eff)
  if (2 * m >= T_eff) stop("Trim too large for effective sample.")
  
  idx <- (m + 1):(T_eff - m)
  unique(z[idx])
}

#-----------------------------------------------------------
# Search tau by AIC + Ljung–Box:
#   - for each tau in grid, estimate MTAR
#   - among p_Q4 >= alpha_Q, choose min AIC
#   - else choose global min AIC
#-----------------------------------------------------------

search_tau_AIC_LB <- function(y,
                              p        = 0,
                              trim     = 0.15,
                              alpha_Q  = 0.05) {
  base_df <- build_mtar_base(y, p = p)
  
  grid <- build_tau_grid(base_df, trim = trim)
  
  cat("Tau search (selectSETAR-style) for p =", p, "\n")
  cat("Grid size:", length(grid), "\n\n")
  
  n_g <- length(grid)
  ssr_vec <- numeric(n_g)
  aic_vec <- numeric(n_g)
  pQ_vec  <- numeric(n_g)
  fits    <- vector("list", n_g)
  
  for (j in seq_along(grid)) {
    tau_j <- grid[j]
    
    fit_j <- estimate_mtar_given_tau(base_df, p = p, tau = tau_j,
                                     trim = trim)
    ssr_vec[j] <- fit_j$SSR
    aic_vec[j] <- fit_j$AIC
    pQ_vec[j]  <- fit_j$p_Q4
    fits[[j]]  <- fit_j
    
    cat("tau =", round(tau_j, 5),
        "| AIC =", round(fit_j$AIC, 2),
        "| p_Q4 =", round(fit_j$p_Q4, 4), "\n")
  }
  
  valid <- which(pQ_vec >= alpha_Q & is.finite(aic_vec))
  
  if (length(valid) > 0) {
    j_min <- valid[which.min(aic_vec[valid])]
    best_fit <- fits[[j_min]]
    cat("\n>>> Selected tau by AIC among p_Q4 >=", alpha_Q,
        "for p =", p, "\n")
  } else {
    finite_idx <- which(is.finite(aic_vec))
    if (length(finite_idx) == 0) {
      warning("No finite AIC on grid; returning first fit for p = ", p)
      j_min <- 1
    } else {
      j_min <- finite_idx[which.min(aic_vec[finite_idx])]
    }
    best_fit <- fits[[j_min]]
    warning("No tau satisfies p_Q4 >= ", alpha_Q,
            " for p = ", p,
            ". Returning tau with minimum AIC on grid.")
  }
  
  best_fit$grid_info <- tibble(
    tau  = grid,
    SSR  = ssr_vec,
    AIC  = aic_vec,
    p_Q4 = pQ_vec
  )
  
  best_fit
}
