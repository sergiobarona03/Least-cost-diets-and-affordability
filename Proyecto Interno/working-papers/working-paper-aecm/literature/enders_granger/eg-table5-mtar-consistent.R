############################################################
## Enders & Granger (1998)
## M-TAR model with wide tau search + Ljung-Box selection
## Quarterly interest-rate differential 1958Q1–1994Q1
############################################################

library(tidyverse)
library(lubridate)
library(readxl)

##----------------------------------------------------------
## 0. Load data and build quarterly dates + spread
##----------------------------------------------------------

setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/")

dataset <- read_excel(
  "working-papers/working-paper-aecm/literature/enders_granger/dataset_enders.xlsx"
)

n_obs <- nrow(dataset)

dates_q <- seq.Date(
  from = as.Date("1958-01-01"),
  by   = "quarter",
  length.out = n_obs
)

df <- dataset %>%
  mutate(
    date   = dates_q,
    spread = r_10 - r_short          # r_Dt in the paper
  )

rD  <- df$spread
Tn  <- length(rD)

##----------------------------------------------------------
## 1. Demean spread for the EG unit-root framework
##    y_t = r_Dt - mean(r_Dt)
##----------------------------------------------------------

reg_det <- lm(rD ~ 1)
y <- resid(reg_det)               # demeaned series
mu_rD <- mean(rD)                 # sample mean of r_Dt (for back-transforming threshold)

##----------------------------------------------------------
## 2. Enders–Granger AIC/BIC
##----------------------------------------------------------

ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

##----------------------------------------------------------
## 3. M-TAR regression dataset for given tau
##    M-TAR: indicator based on Δy_{t-1}
##    Δy_t = α + ρ1 I_t (y_{t-1} - tau) + ρ2 (1−I_t)(y_{t-1} - tau)
##           + Σ δ_j Δy_{t-j} + ε_t
##    with I_t = 1{ Δy_{t-1} < 0 }  (your convention)
##----------------------------------------------------------

build_mtar_df <- function(y, p = 0, tau = 0) {
  y   <- as.numeric(y)
  Tn  <- length(y)
  
  dy   <- c(NA, diff(y))          # Δy_t
  y_l1 <- c(NA, y[-Tn])           # y_{t-1}
  
  # Momentum part: Δy_{t-1}
  dy_l1 <- c(NA, diff(y_l1))      # Δy_{t-1}
  
  # M-TAR indicator: I_t = 1{ Δy_{t-1} < 0 }
  I <- ifelse(dy_l1 < 0, 1, 0)
  
  z1 <- I       * (y_l1 - tau)
  z2 <- (1 - I) * (y_l1 - tau)
  
  dfm <- data.frame(dy = dy, y_l1 = y_l1, dy_l1 = dy_l1, z1 = z1, z2 = z2)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dy_l", j)]] <- c(rep(NA, j), dy[1:(Tn - j)])
    }
  }
  
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20) stop("Insufficient observations after lagging.")
  
  dfm
}

##----------------------------------------------------------
## 4. Base dataset to construct grids (no tau)
##    Gives y_{t-1} for the effective sample at each p
##----------------------------------------------------------

build_mtar_base <- function(y, p = 0) {
  y   <- as.numeric(y)
  Tn  <- length(y)
  
  dy   <- c(NA, diff(y))          # Δy_t
  y_l1 <- c(NA, y[-Tn])           # y_{t-1}
  dy_l1 <- c(NA, diff(y_l1))      # Δy_{t-1}
  
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

##----------------------------------------------------------
## 5. Given tau: estimate M-TAR and compute tests
##----------------------------------------------------------

estimate_mtar_given_tau <- function(y, p = 0, tau = 0) {
  
  dfm <- build_mtar_df(y, p = p, tau = tau)
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  fml <- as.formula(paste("dy ~ 1 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))      # includes constant
  
  ## (A) Φμ* (MTAR): H0: ρ1 = ρ2 = 0
  if (p > 0) {
    rhs_R <- paste0("dy_l", 1:p, collapse = " + ")
    fml_R <- as.formula(paste("dy ~ 1 +", rhs_R))
  } else {
    fml_R <- dy ~ 1
  }
  
  mod_R <- lm(fml_R, data = dfm)
  SSR_R <- sum(resid(mod_R)^2)
  
  q <- 2
  Phi_mu <- ((SSR_R - SSR_U) / q) / (SSR_U / (Tn_m - kU))
  
  ## (B) Equality test: H0: ρ1 = ρ2
  dfm_eq <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dy_l", 1:p))
  fml_eq <- as.formula(paste("dy ~ 1 +", paste(rhs_eq, collapse = " + ")))
  mod_eq <- lm(fml_eq, data = dfm_eq)
  
  an <- anova(mod_eq, mod)
  F_equal <- an$F[2]
  pF_equal <- an$`Pr(>F)`[2]
  
  ## Coefs, IC, Ljung–Box
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

##----------------------------------------------------------
## 6. Wide-grid search over tau with Ljung-Box constraint
##
##    - Build base df for this p
##    - Construct a VERY wide grid:
##        * Chan-style midpoints of y_{t-1} (trimmed)
##        * plus a dense seq() over an expanded range
##    - For each tau:
##        * estimate M-TAR
##        * store SSR and p_Q4
##        * print tau and p_Q4
##    - Among taus with p_Q4 >= alpha_Q, choose min SSR
##    - If none pass, choose global min-SSR tau (warn)
##----------------------------------------------------------

search_tau_wide_lb <- function(y,
                               p        = 0,
                               alpha_Q  = 0.05,
                               trim     = 0.15,
                               by_dense = 0.0001,
                               expand_factor = 0.5) {
  dfb <- build_mtar_base(y, p = p)
  y_l1 <- dfb$y_l1
  
  ## Basic range from y_{t-1} in effective sample
  rng   <- range(y_l1)
  width <- diff(rng)
  
  ## Expand range beyond observed support to be generous
  lower_expanded <- rng[1] - expand_factor * width
  upper_expanded <- rng[2] + expand_factor * width
  
  ## Chan-style midpoints on trimmed y_{t-1}
  y_sorted <- sort(y_l1)
  T_eff    <- length(y_sorted)
  m <- floor(trim * T_eff)
  idx <- (m + 1):(T_eff - m)
  
  if (length(idx) < 3) {
    warning("Trim too large for effective sample; falling back to full range midpoints.")
    q <- y_sorted
  } else {
    q <- y_sorted[idx]
  }
  
  chan_midpoints <- (q[-length(q)] + q[-1]) / 2
  
  ## Dense grid on expanded range
  dense_grid <- seq(lower_expanded, upper_expanded, by = by_dense)
  
  ## Union of chan midpoints and dense grid
  grid <- sort(unique(c(chan_midpoints, dense_grid)))
  
  cat("Wide tau search for p =", p, "\n")
  cat("Grid size:", length(grid), "\n")
  cat("Expanded range: [", round(lower_expanded, 4), ",",
      round(upper_expanded, 4), "]\n\n")
  
  n_g <- length(grid)
  ssr_vec <- numeric(n_g)
  pQ_vec  <- numeric(n_g)
  fits    <- vector("list", n_g)
  
  for (j in seq_along(grid)) {
    tau_j <- grid[j]
    
    fit_j <- estimate_mtar_given_tau(y, p = p, tau = tau_j)
    ssr_vec[j] <- fit_j$SSR
    pQ_vec[j]  <- fit_j$p_Q4
    fits[[j]]  <- fit_j
    
    cat("Evaluating tau =", round(tau_j, 4),
        "for p =", p,
        "=> p_Q4 =", round(fit_j$p_Q4, 4), "\n")
  }
  
  ## Filter by Ljung-Box condition
  valid <- which(pQ_vec >= alpha_Q)
  
  if (length(valid) > 0) {
    j_min <- valid[which.min(ssr_vec[valid])]
    best_fit <- fits[[j_min]]
    cat("\n>>> Found tau with p_Q4 >=", alpha_Q, "for p =", p, "\n")
  } else {
    j_min <- which.min(ssr_vec)
    best_fit <- fits[[j_min]]
    warning("No tau satisfies p_Q4 >= ", alpha_Q,
            " for p = ", p,
            ". Returning tau with minimum SSR (Chan-like) on wide grid.")
  }
  
  best_fit$grid_info <- tibble(
    tau  = grid,
    SSR  = ssr_vec,
    p_Q4 = pQ_vec
  )
  
  best_fit
}

##----------------------------------------------------------
## 7. Run wide search for p = 0 and p = 1
##    (you can adjust alpha_Q, by_dense, expand_factor)
##----------------------------------------------------------

mtar_p0 <- search_tau_wide_lb(y,
                              p        = 0,
                              alpha_Q  = 0.05,
                              trim     = 0.15,
                              by_dense = 0.001,
                              expand_factor = 2)

mtar_p1 <- search_tau_wide_lb(y,
                              p        = 1,
                              alpha_Q  = 0.05,
                              trim     = 0.15,
                              by_dense = 0.001,
                              expand_factor = 2)

## Threshold in demeaned units and original spread (r_Dt) units (for p=0)
tau_hat_y  <- mtar_p0$tau
tau_hat_rD <- tau_hat_y + mu_rD   # attractor in terms of r_Dt

cat("\nSelected tau (demeaned y, p=0): ", round(tau_hat_y, 3), "\n")
cat("Selected tau (r_Dt units, p=0):  ", round(tau_hat_rD, 3), "\n\n")

##----------------------------------------------------------
## 8. Collect results
##----------------------------------------------------------

results_mtar <- tibble(
  model  = c("M-TAR wide+Q (p=0)", "M-TAR wide+Q (p=1)"),
  tau_y  = c(mtar_p0$tau,           mtar_p1$tau),
  tau_rD = c(mtar_p0$tau + mu_rD,   mtar_p1$tau + mu_rD),
  rho1   = c(mtar_p0$rho1,          mtar_p1$rho1),
  t_rho1 = c(mtar_p0$t_rho1,        mtar_p1$t_rho1),
  rho2   = c(mtar_p0$rho2,          mtar_p1$rho2),
  t_rho2 = c(mtar_p0$t_rho2,        mtar_p1$t_rho2),
  Phi_mu = c(mtar_p0$Phi_mu,        mtar_p1$Phi_mu),
  F_equal = c(mtar_p0$F_equal,      mtar_p1$F_equal),
  p_equal = c(mtar_p0$p_equal,      mtar_p1$p_equal),
  AIC    = c(mtar_p0$AIC,           mtar_p1$AIC),
  BIC    = c(mtar_p0$BIC,           mtar_p1$BIC),
  Q4     = c(mtar_p0$Q4,            mtar_p1$Q4),
  p_Q4   = c(mtar_p0$p_Q4,          mtar_p1$p_Q4)
)

print(results_mtar, digits = 4)


