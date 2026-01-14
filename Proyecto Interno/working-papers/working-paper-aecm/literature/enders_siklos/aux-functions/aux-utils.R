############################################################
## aux-utils.R
## Shared utilities: load data, cointegration residuals,
## Enders–Granger IC (AIC/BIC)
############################################################

library(tidyverse)
library(lubridate)
library(readxl)

#-----------------------------------------------------------
# 1. Load Enders–Siklos rates data and build monthly dates
#    Optionally filter from a start date
#-----------------------------------------------------------

load_rates_data <- function(path_rates,
                            start_date = as.Date("1979-10-01")) {
  dataset <- read_excel(path_rates)
  
  colnames(dataset) <- c("date_raw", "fed_funds", "t_bill",
                         "prime", "g_10", "g3")
  
  n_obs <- nrow(dataset)
  
  dates_m <- seq.Date(
    from = as.Date("1964-01-01"),
    by   = "month",
    length.out = n_obs
  )
  
  df <- dataset %>%
    mutate(date = dates_m) %>%
    filter(date >= start_date)
  
  df
}

#-----------------------------------------------------------
# 2. Cointegrating regression log(dep_var) on const + log(reg_var)
#    Returns list: y (residuals), Tn, reg (lm object)
#-----------------------------------------------------------

get_coint_residual <- function(df,
                               dep_var = "fed_funds",
                               reg_var = "g_10") {
  fml <- as.formula(
    paste0("log(", dep_var, ") ~ 1 + log(", reg_var, ")")
  )
  reg_det <- lm(fml, data = df)
  y <- resid(reg_det)
  Tn <- length(y)
  
  list(
    y   = y,
    Tn  = Tn,
    reg = reg_det
  )
}

#-----------------------------------------------------------
# 3. Enders–Granger information criteria:
#    AIC = T log(SSR) + 2n
#    BIC = T log(SSR) + n log(T)
#-----------------------------------------------------------

ic_enders <- function(ssr, Tn, n_par) {
  aic <- Tn * log(ssr) + 2 * n_par
  bic <- Tn * log(ssr) + n_par * log(Tn)
  list(AIC = aic, BIC = bic)
}
