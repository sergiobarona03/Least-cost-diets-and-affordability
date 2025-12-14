############################################################
## Metodología II (SARIMAX) - Estimación + Predicción      ##
## Output: predictions by food (test sample)               ##
############################################################

# -----------------------
# Libraries
# -----------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(forecast)

# -----------------------
# Paths / settings
# -----------------------
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")
date_tag <- "121225"

infile <- paste0(
  "working-papers\\working-paper-1225\\mapeo ipc-sipsa\\output\\",
  date_tag, "_dataset_ipc_sipsa.xlsx"
)

out_dir <- "working-papers\\working-paper-1225\\m2\\output\\"

# -----------------------
# Load data
# -----------------------
data_merged <- readxl::read_excel(infile)

# -----------------------
# Prepare monthly series
# Endog: ln(retail)
# Exog : ln(wholesale)
# -----------------------
data_ts <- data_merged %>%
  filter(precio_ipc > 0, precio_sipsa > 0) %>%
  mutate(
    date = as.Date(paste(Year, Month, "01", sep = "-")),
    ln_retail = log(precio_ipc),
    ln_wholesale = log(precio_sipsa)
  ) %>%
  arrange(alimento_sipsa, date)

foods <- sort(unique(data_ts$alimento_sipsa))

# Containers
pred_list <- list()
model_info <- list()

# -----------------------
# Fit + forecast per food
# -----------------------
for (f in foods) {
  
  print(f)
  
  df_f <- data_ts %>%
    filter(alimento_sipsa == f) %>%
    arrange(date) %>%
    select(alimento_sipsa, date, precio_ipc, precio_sipsa, ln_retail, ln_wholesale) %>%
    drop_na(ln_retail, ln_wholesale)
  
  n <- nrow(df_f)
  
  # Minimum sample (≥ 36 months)
  if (n < 36) next
  
  cutoff <- floor(0.7 * n)
  
  train <- df_f[1:cutoff, ]
  test  <- df_f[(cutoff + 1):n, ]
  
  # Build ts objects (monthly frequency)
  start_year  <- year(train$date[1])
  start_month <- month(train$date[1])
  
  y_train <- ts(train$ln_retail, frequency = 12, start = c(start_year, start_month))
  x_train <- as.matrix(train$ln_wholesale)
  
  # Forecast horizon
  h <- nrow(test)
  
  # Safety
  if (h < 1) next
  
  x_test <- as.matrix(test$ln_wholesale)
  
  fit <- tryCatch(
    auto.arima(
      y_train,
      xreg = x_train,
      seasonal = TRUE,
      stepwise = FALSE,
      approximation = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) next
  
  fc <- tryCatch(
    forecast(fit, xreg = x_test, h = h, level = c(80, 95)),
    error = function(e) NULL
  )
  
  if (is.null(fc)) next
  
  # Convert forecasts back to levels
  pred_mean <- exp(as.numeric(fc$mean))
  
  # Intervals (also back to levels)
  lo80 <- exp(as.numeric(fc$lower[, "80%"]))
  hi80 <- exp(as.numeric(fc$upper[, "80%"]))
  lo95 <- exp(as.numeric(fc$lower[, "95%"]))
  hi95 <- exp(as.numeric(fc$upper[, "95%"]))
  
  pred_list[[f]] <- tibble(
    alimento_sipsa = f,
    date = test$date,
    observed = test$precio_ipc,
    pred_mean = pred_mean,
    pred_lo80 = lo80,
    pred_hi80 = hi80,
    pred_lo95 = lo95,
    pred_hi95 = hi95,
    n_train = nrow(train),
    n_test  = nrow(test)
  )
  
  model_info[[f]] <- tibble(
    alimento_sipsa = f,
    n_total = n,
    n_train = nrow(train),
    n_test  = nrow(test),
    arima = as.character(fit$arma[1]),
    i    = as.character(fit$arma[6]),
    ma   = as.character(fit$arma[2]),
    sar  = as.character(fit$arma[3]),
    si   = as.character(fit$arma[7]),
    sma  = as.character(fit$arma[4])
  )
}

predictions_by_food <- bind_rows(pred_list)
models_summary <- bind_rows(model_info)

# -----------------------
# Save outputs
# -----------------------
write_csv(
  predictions_by_food,
  paste0(out_dir, date_tag, "_m2_predictions_by_food.csv")
)

write_csv(
  models_summary,
  paste0(out_dir, date_tag, "_m2_models_summary.csv")
)

