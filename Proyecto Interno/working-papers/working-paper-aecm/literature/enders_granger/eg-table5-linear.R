############################################################
## Enders & Granger (1998)
## Linear Attractor TAR model (Φμ and Equality tests)
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
    spread = r_10 - r_short   # long - short
  )

##----------------------------------------------------------
## 1. Step 1: Remove deterministic part (constant)
##    y_t = spread_t - a0
##----------------------------------------------------------

reg_det <- lm(spread ~ 1, data = df)
y <- resid(reg_det)   # series used for TAR
Tn <- length(y)

##----------------------------------------------------------
## 2. Build TAR regression dataset for lag p
##----------------------------------------------------------

build_tar_df <- function(y, p = 0) {
  y <- as.numeric(y)
  Tn <- length(y)
  
  dy   <- c(NA, diff(y))
  y_l1 <- c(NA, y[-Tn])
  
  # TAR indicator: I_t = 1{ y_{t-1} < 0 }
  I <- ifelse(y_l1 < 0, 1, 0)
  
  z1 <- I * y_l1
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

##----------------------------------------------------------
## 3. AIC/BIC as in Enders–Granger
##    AIC = T log(SSR) + 2n
##    BIC = T log(SSR) + n log(T)
##----------------------------------------------------------

ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

##----------------------------------------------------------
## 4. Estimate TAR model with tests:
##    - φ_μ (joint zero)
##    - Equality (ρ1 = ρ2)
##----------------------------------------------------------

estimate_linear_attractor <- function(y, p = 0) {
  
  dfm <- build_tar_df(y, p = p)
  
  rhs <- c("z1", "z2")
  if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  fml <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  # unrestricted residuals
  SSR_U <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  
  #--------------------------------------------------------
  # (A) Φμ statistic: H0: ρ1 = ρ2 = 0
  #--------------------------------------------------------
  
  if (p > 0) {
    rhs_R <- paste0("dy_l", 1:p, collapse = " + ")
    fml_R <- as.formula(paste("dy ~ 0 +", rhs_R))
  } else {
    fml_R <- dy ~ 0
  }
  
  mod_R <- lm(fml_R, data = dfm)
  SSR_R <- sum(resid(mod_R)^2)
  
  q <- 2     # number of restrictions
  Phi_mu <- ((SSR_R - SSR_U)/q) / (SSR_U/(Tn_m - kU))
  
  #--------------------------------------------------------
  # (B) Equality test: H0: ρ1 = ρ2
  #--------------------------------------------------------
  
  dfm_eq <- dfm
  dfm_eq$z_sum <- dfm_eq$z1 + dfm_eq$z2
  rhs_eq <- c("z_sum")
  if (p > 0) rhs_eq <- c(rhs_eq, paste0("dy_l", 1:p))
  fml_eq <- as.formula(paste("dy ~ 0 +", paste(rhs_eq, collapse = " + ")))
  mod_eq <- lm(fml_eq, data = dfm_eq)
  
  an <- anova(mod_eq, mod)
  F_equal <- an$F[2]
  pF_equal <- an$`Pr(>F)`[2]
  
  #--------------------------------------------------------
  # t-stats and IC
  #--------------------------------------------------------
  
  coefs <- coef(s)
  rho1  <- coefs["z1", "Estimate"]
  trho1 <- coefs["z1", "t value"]
  rho2  <- coefs["z2", "Estimate"]
  trho2 <- coefs["z2", "t value"]
  
  ic <- ic_enders(SSR_U, Tn_m, kU)
  
  # Ljung–Box Q(4)
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

##----------------------------------------------------------
## 5. Run TAR for p = 0 and p = 1
##----------------------------------------------------------

tar_p0 <- estimate_linear_attractor(y, p = 0)
tar_p1 <- estimate_linear_attractor(y, p = 1)

##----------------------------------------------------------
## 6. Display results (like Table 5)
##----------------------------------------------------------

results <- tibble(
  model = c("TAR (p=0)", "TAR (p=1)"),
  rho1  = c(tar_p0$rho1, tar_p1$rho1),
  t_rho1 = c(tar_p0$t_rho1, tar_p1$t_rho1),
  rho2  = c(tar_p0$rho2, tar_p1$rho2),
  t_rho2 = c(tar_p0$t_rho2, tar_p1$t_rho2),
  Phi_mu = c(tar_p0$Phi_mu, tar_p1$Phi_mu),
  F_equal = c(tar_p0$F_equal, tar_p1$F_equal),
  p_equal = c(tar_p0$p_equal, tar_p1$p_equal),
  AIC = c(tar_p0$AIC, tar_p1$AIC),
  BIC = c(tar_p0$BIC, tar_p1$BIC),
  Q4  = c(tar_p0$Q4, tar_p1$Q4),
  p_Q4 = c(tar_p0$p_Q4, tar_p1$p_Q4)
)

print(results, digits = 4)
