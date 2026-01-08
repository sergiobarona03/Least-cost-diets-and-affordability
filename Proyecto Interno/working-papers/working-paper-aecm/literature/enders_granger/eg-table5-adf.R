############################################################
## Enders & Granger (1998)
## Dickey–Fuller models (Φμ-style regression sin umbral)
## Quarterly interest-rate differential 1958Q1–1994Q1
## Output: rho1 (φ), t-stat, AIC, BIC, Q(4)
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
##    (igual que en los modelos TAR / Momentum)
##----------------------------------------------------------

reg_det <- lm(spread ~ 1, data = df)
y <- resid(reg_det)      # serie "y_t" para el DF
Tn <- length(y)

##----------------------------------------------------------
## 2. AIC/BIC como en Enders–Granger
##    AIC = T log(SSR) + 2n
##    BIC = T log(SSR) + n log(T)
##----------------------------------------------------------

ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}

##----------------------------------------------------------
## 3. Construir datos para un modelo Dickey–Fuller con p lags
##
##    Δy_t    = dy_t
##    y_{t-1} = y_l1
##    regresión: dy_t ~ φ y_{t-1} + Σ γ_j Δy_{t-j}
##               sin intercepto
##----------------------------------------------------------

build_df_data <- function(y, p = 0) {
  y  <- as.numeric(y)
  Tn <- length(y)
  
  dy   <- c(NA_real_, diff(y))
  y_l1 <- c(NA_real_, y[-Tn])
  
  dfm <- data.frame(dy = dy, y_l1 = y_l1)
  
  if (p > 0) {
    for (j in 1:p) {
      dfm[[paste0("dy_l", j)]] <- c(rep(NA_real_, j), dy[1:(Tn - j)])
    }
  }
  
  dfm <- dfm[complete.cases(dfm), , drop = FALSE]
  if (nrow(dfm) < 20) stop("Insufficient observations after lagging.")
  
  dfm
}

##----------------------------------------------------------
## 4. Estimar modelo Dickey–Fuller:
##    Δy_t = φ y_{t-1} + Σ γ_j Δy_{t-j} + ε_t
##----------------------------------------------------------

estimate_df_model <- function(y, p = 0) {
  
  dfm <- build_df_data(y, p = p)
  
  rhs <- c("y_l1")
  if (p > 0) rhs <- c(rhs, paste0("dy_l", 1:p))
  fml <- as.formula(paste("dy ~ 0 +", paste(rhs, collapse = " + ")))
  
  mod <- lm(fml, data = dfm)
  s   <- summary(mod)
  
  # SSR, AIC, BIC
  SSR   <- sum(resid(mod)^2)
  Tn_m  <- nrow(dfm)
  kU    <- length(coef(mod))
  ic    <- ic_enders(SSR, Tn_m, kU)
  
  coefs <- coef(s)
  phi   <- coefs["y_l1", "Estimate"]
  t_phi <- coefs["y_l1", "t value"]
  
  # Ljung–Box Q(4) para residuales
  Q4  <- Box.test(resid(mod), lag = 4, type = "Ljung-Box", fitdf = kU)
  
  list(
    model = mod,
    p     = p,
    phi   = phi,
    t_phi = t_phi,
    AIC   = ic$AIC,
    BIC   = ic$BIC,
    Q4    = as.numeric(Q4$statistic),
    p_Q4  = Q4$p.value
  )
}

##----------------------------------------------------------
## 5. Correr DF para p = 0 y p = 1
##----------------------------------------------------------

df_p0 <- estimate_df_model(y, p = 0)
df_p1 <- estimate_df_model(y, p = 1)

##----------------------------------------------------------
## 6. Resumen tipo tabla (para comparar con Enders & Granger)
##----------------------------------------------------------

df_results <- tibble(
  model  = c("DF (p=0)", "DF (p=1)"),
  phi    = c(df_p0$phi,    df_p1$phi),
  t_phi  = c(df_p0$t_phi,  df_p1$t_phi),
  AIC    = c(df_p0$AIC,    df_p1$AIC),
  BIC    = c(df_p0$BIC,    df_p1$BIC),
  Q4     = c(df_p0$Q4,     df_p1$Q4),
  p_Q4   = c(df_p0$p_Q4,   df_p1$p_Q4)
)

print(df_results, digits = 4)

# Si quieres ver los coeficientes completos:
# summary(df_p0$model)
# summary(df_p1$model)
