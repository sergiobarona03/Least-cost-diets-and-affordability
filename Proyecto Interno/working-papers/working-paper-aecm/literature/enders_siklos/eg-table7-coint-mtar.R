############################################################
## Enders & Siklos / Enders & Granger style ECM M-TAR
## Fed Funds (short rate) and 10-Year Bond (long rate)
## μ̂t from cointegration, M-TAR Heaviside on Δμ̂t−1
## First-order A_ij(L) polynomials, intercept included
############################################################

library(tidyverse)
library(lubridate)
library(readxl)

##----------------------------------------------------------
## 0. Load data and define sample
##----------------------------------------------------------

setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/")

dataset <- read_excel(
  "working-papers/working-paper-aecm/literature/enders_siklos/rates.xlsx"
)

colnames(dataset) <- c("date_raw", "fed_funds", "t_bill",
                       "prime", "g_10", "g3")

n_obs <- nrow(dataset)

dates_m <- seq.Date(
  from = as.Date("1964-01-01"),
  by   = "month",
  length.out = n_obs
)

df <- dataset %>%
  mutate(date = dates_m)

# Sample as in the EG / ES applications
start_date_eg <- as.Date("1979-10-01")

df <- df %>%
  filter(date >= start_date_eg) %>%
  mutate(
    r_f  = log(fed_funds),  # short rate (Fed Funds)
    r_10 = log(g_10)        # long rate (10-year bond)
  )

Tn <- nrow(df)

##----------------------------------------------------------
## 1. Cointegrating relation and μ̂_t
##   (here: log r_f on constant + log r_10)
##----------------------------------------------------------

reg_coint <- lm(r_f ~ 1 + r_10, data = df)
summary(reg_coint)

mu_hat <- resid(reg_coint)         # μ̂_t

df$mu_hat <- mu_hat

##----------------------------------------------------------
## 2. M-TAR Heaviside indicator M_t and μ_minus, μ_plus
##    M_t based on Δμ̂_{t−1} and threshold τ_x
##----------------------------------------------------------

# Threshold from your previous M-TAR step
tau.x <- -0.0261

# Δμ̂_t and its lag
d_mu    <- c(NA, diff(mu_hat))              # Δμ̂_t
d_mu_l1 <- c(NA, d_mu[-length(d_mu)])       # Δμ̂_{t−1}
mu_l1   <- c(NA, mu_hat[-length(mu_hat)])   # μ̂_{t−1}

# Heaviside indicator: M_t = 1{ Δμ̂_{t−1} >= τ_x }
M_t <- ifelse(d_mu_l1 >= tau.x, 1, 0)

# Asymmetric error-correction terms:
# μ_minus,t−1 = M_t * μ̂_{t−1}
# μ_plus,t−1  = (1 − M_t) * μ̂_{t−1}
mu_minus_l1 <- M_t       * mu_l1
mu_plus_l1  <- (1 - M_t) * mu_l1

##----------------------------------------------------------
## 3. First differences of r_f and r_10 and 1 lag (A_ij(L))
##----------------------------------------------------------

dr_f  <- c(NA, diff(df$r_f))
dr_10 <- c(NA, diff(df$r_10))

p_lags <- 1  # first-order polynomials A_ij(L)

df_ecm <- tibble(
  dr_f        = dr_f,
  dr_10       = dr_10,
  mu_minus_l1 = mu_minus_l1,
  mu_plus_l1  = mu_plus_l1
)

# Add lags of Δr_f,t and Δr_10,t
for (j in 1:p_lags) {
  df_ecm[[paste0("dr_f_l", j)]]  <- c(rep(NA, j), dr_f[1:(Tn - j)])
  df_ecm[[paste0("dr_10_l", j)]] <- c(rep(NA, j), dr_10[1:(Tn - j)])
}

# Drop rows with NAs from μ̂, Δμ̂, and lags
df_ecm <- df_ecm %>% drop_na()

##----------------------------------------------------------
## 4. Estimate asymmetric ECMs with intercept
##    Eq. 1: Δr_f,t
##    Eq. 2: Δr_10,t
##----------------------------------------------------------

rhs_lags <- c(paste0("dr_f_l", 1:p_lags),
              paste0("dr_10_l", 1:p_lags))
rhs_all <- c(rhs_lags, "mu_minus_l1", "mu_plus_l1")

# Δr_f,t equation (short rate)
fml_f <- as.formula(
  paste("dr_f ~ 1 +", paste(rhs_all, collapse = " + "))
)
mod_f <- lm(fml_f, data = df_ecm)
cat("\n=== ECM para Δr_f,t (Fed Funds) ===\n")
print(summary(mod_f))

# Δr_10,t equation (long rate)
fml_10 <- as.formula(
  paste("dr_10 ~ 1 +", paste(rhs_all, collapse = " + "))
)
mod_10 <- lm(fml_10, data = df_ecm)
cat("\n=== ECM para Δr_10,t (10-year bond) ===\n")
print(summary(mod_10))

##----------------------------------------------------------
## 5. F-tests A_ij(L) = 0 with intercept kept
##    F_11: Δr_f lags in Δr_f eq.
##    F_12: Δr_10 lags in Δr_f eq.
##    F_21: Δr_f lags in Δr_10 eq.
##    F_22: Δr_10 lags in Δr_10 eq.
##----------------------------------------------------------

# F_11: remove dr_f_lags from Δr_f eq., keep intercept & other terms
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

# F_12: remove dr_10_lags from Δr_f eq.
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

# F_21: remove dr_f_lags from Δr_10 eq.
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

# F_22: remove dr_10_lags from Δr_10 eq.
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

cat("\n=== F-tests A_ij(L) = 0 (con intercepto) ===\n")
cat("F_11 (Δr_f lags in Δr_f eq.)   =", round(F_11, 2),
    "p =", round(p_11, 3), "\n")
cat("F_12 (Δr_10 lags in Δr_f eq.)  =", round(F_12, 2),
    "p =", round(p_12, 3), "\n")
cat("F_21 (Δr_f lags in Δr_10 eq.)  =", round(F_21, 2),
    "p =", round(p_21, 3), "\n")
cat("F_22 (Δr_10 lags in Δr_10 eq.) =", round(F_22, 2),
    "p =", round(p_22, 3), "\n")

##----------------------------------------------------------
## 6. Compact summary of asymmetric EC coefficients
##    (μ_minus,t−1 and μ_plus,t−1)
##----------------------------------------------------------

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

cat("\n=== Coeficientes de corrección de error asimétrica ===\n")
print(ecm_results, digits = 4)
