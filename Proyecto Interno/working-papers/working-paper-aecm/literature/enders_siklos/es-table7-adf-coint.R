
############################################################
## R REPLICATION OF THE RATS SCRIPT (Enders–Siklos rates)
## - Graph Fed Funds & 10-year bond
## - ADF unit root tests (from 1980:1)
## - VAR lag selection (AIC/BIC)
## - Johansen cointegration tests (lags 2 and 3)
## - Engle–Granger regression + residual ADF / auxiliary regression
############################################################

library(tidyverse)
library(readxl)
library(ggplot2)
library(urca)   # ADF, Johansen
library(vars)   # VAR + lag selection
library(zoo)    # for time indexing if needed

##----------------------------------------------------------
## 0. Load data and build monthly ts objects
##   (RATS: open data rates.xls; calendar(m) 1964:1)
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

# Monthly ts objects
ff_ts  <- ts(df$fed_funds, start = c(1964, 1), frequency = 12)
g10_ts <- ts(df$g_10,      start = c(1964, 1), frequency = 12)

ys_ts <- cbind(fed_funds = ff_ts,
               g_10      = g10_ts)

##----------------------------------------------------------
## 1. Graph: Fed Funds & 10-Year bond
##   (RATS: graph ... fed_funds g_10)
##----------------------------------------------------------

df_long <- df %>%
  dplyr::select(date, fed_funds, g_10) %>%
  pivot_longer(-date, names_to = "series", values_to = "value")

ggplot(df_long, aes(x = date, y = value, colour = series)) +
  geom_line() +
  labs(
    title  = "Figure 4. Interest Rate Data",
    x      = "",
    y      = "Percent",
    colour = ""
  ) +
  scale_colour_manual(
    values = c("fed_funds" = "black", "g_10" = "grey40"),
    labels = c("fed_funds" = "Fed Funds", "g_10" = "10-Year Bond")
  ) +
  theme_minimal()

##----------------------------------------------------------
## 2. ADF unit root tests from 1980:1 with 4 lags
##   (RATS: @dfunit(lags=4) fed_funds 1980:1 *
##          @dfunit(lags=4) g_10 1980:1 *)
##----------------------------------------------------------

ff_1980  <- window(ff_ts,  start = c(1980, 1))
g10_1980 <- window(g10_ts, start = c(1980, 1))

adf_ff <- ur.df(ff_1980, type = "drift", lags = 4)   # constant, no trend
adf_g10 <- ur.df(g10_1980, type = "drift", lags = 4)

cat("\n=== ADF test for Fed Funds (1980:1, 4 lags) ===\n")
print(summary(adf_ff))

cat("\n=== ADF test for 10-Year Bond (1980:1, 4 lags) ===\n")
print(summary(adf_g10))

##----------------------------------------------------------
## 3. Logs: logff, log10
##   (RATS: set logff = log(fed_funds); set log10 = log(g_10))
##----------------------------------------------------------

df <- df %>%
  mutate(
    logff = log(fed_funds),
    log10 = log(g_10)
  )

logff_ts <- ts(df$logff, start = c(1964, 1), frequency = 12)
log10_ts <- ts(df$log10, start = c(1964, 1), frequency = 12)

log_ys_ts <- cbind(logff = logff_ts,
                   log10 = log10_ts)

log_ys_1980 <- window(log_ys_ts, start = c(1980, 1))

##----------------------------------------------------------
## 4. VAR lag selection up to 10 lags (AIC/BIC)
##   (RATS: @varlagselect(crit=bic,lags=10) 1980:1 logff log10
##          @varlagselect(crit=aic,lags=10) 1980:1 logff log10)
##----------------------------------------------------------

lag_sel <- VARselect(log_ys_1980, lag.max = 10, type = "const")

cat("\n=== VAR Lag Selection (1980:1, up to 10 lags) ===\n")
print(lag_sel$selection)   # AIC, HQ, SC(BIC), FPE
cat("\nCriteria table:\n")
print(lag_sel$criteria)

##----------------------------------------------------------
## 5. Johansen cointegration (Engle–Johansen MLE)
##   (RATS: @johmle(det=rc,lags=2) 1980:1 logff log10
##          @johmle(det=rc,lags=3) 1980:1 logff log10)
##
## det=rc: restricted constant → ecdet="const" in ca.jo
## RATS lags = p ~ number of Δ-lags; ca.jo uses K = p+1
##----------------------------------------------------------

# Approx "lags=2" => K=3
joh_2 <- ca.jo(
  log_ys_1980,
  type  = "trace",
  ecdet = "const",
  K     = 3
)

cat("\n=== Johansen (lags≈2, K=3) ===\n")
print(summary(joh_2))

# Approx "lags=3" => K=4
joh_3 <- ca.jo(
  log_ys_1980,
  type  = "trace",
  ecdet = "const",
  K     = 4
)

cat("\n=== Johansen (lags≈3, K=4) ===\n")
print(summary(joh_3))

##----------------------------------------------------------
## 6. Engle–Granger step 1: logff on constant + log10
##   (RATS: linreg logff 1979:10 * u; # constant log10)
##----------------------------------------------------------

# Match RATS sample start 1979:10
start_date_eg <- as.Date("1979-10-01")

eg_df <- df %>%
  filter(date >= start_date_eg)

eg_reg <- lm(logff ~ log10, data = eg_df)
cat("\n=== Engle-Granger Cointegrating Regression ===\n")
print(summary(eg_reg))

u <- resid(eg_reg)
u_ts <- ts(u,
           start = c(1979, 10),
           frequency = 12)

##----------------------------------------------------------
## 7. Engle–Granger residual test (@egtestresids(lags=2))
##   Use ADF on u_t with 2 lags, no constant/trend
##----------------------------------------------------------

adf_u <- ur.df(u_ts, type = "none", lags = 2)
cat("\n=== ADF on EG residuals u_t (lags=2, no const/trend) ===\n")
print(summary(adf_u))

##----------------------------------------------------------
## 8. Auxiliary regression:
##   set du = u-u{1}
##   linreg du on u{1} du{1 2}
##   (RATS: 'Engle-Granger Auxiliary Regression')
##----------------------------------------------------------

du    <- diff(u_ts)               # Δu_t
du_l1 <- stats::lag(du, -1)       # Δu_{t-1}
du_l2 <- stats::lag(du, -2)       # Δu_{t-2}
u_l1  <- stats::lag(u_ts, -1)     # u_{t-1}

aux_df <- cbind(du = du,
                u_l1 = u_l1,
                du_l1 = du_l1,
                du_l2 = du_l2) %>%
  ts.union() %>%
  as.data.frame() %>%
  drop_na()

aux_reg <- lm(du ~ u_l1 + du_l1 + du_l2, data = aux_df)

cat("\n=== Engle-Granger Auxiliary Regression (du on u_{t-1}, du_{t-1}, du_{t-2}) ===\n")
print(summary(aux_reg))

############################################################
## END OF SCRIPT
############################################################
