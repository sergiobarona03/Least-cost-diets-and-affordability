# ===============================
# Part 0: Load packages
# ===============================
library(tidyverse)
library(apt)
library(Metrics)
library(janitor)

set.seed(123)

# ===============================
# Part 1: Simulate cointegrated data
# ===============================

n <- 50  # 10 years of monthly data
time <- seq(as.Date("2010-01-01"), by = "month", length.out = n)

# Simulate exogenous variable (log_sipsa)
dx <- rnorm(n, 0.01, 0.02)
log_sipsa <- cumsum(dx)

# Simulate long-run relationship with asymmetric adjustment
error <- numeric(n)
log_ipc <- numeric(n)

log_ipc[1] <- log_sipsa[1] + rnorm(1, 0, 0.05)
for (t in 2:n) {
  ect <- log_ipc[t-1] - log_sipsa[t-1]
  adj <- ifelse(ect >= 0, -0.2 * ect, -0.4 * ect)
  log_ipc[t] <- log_ipc[t-1] + adj + rnorm(1, 0.01, 0.02)
}

# Build data frame
data <- tibble(
  date = time,
  log_sipsa = log_sipsa,
  log_ipc = log_ipc
)

# ===============================
# Part 2: Estimate with apt::ecmAsyFit
# ===============================

ts_y <- ts(data$log_ipc, start = c(2010, 1), frequency = 12)
ts_x <- ts(data$log_sipsa, start = c(2010, 1), frequency = 12)
detach("package:dplyr", unload = TRUE)
fit <- ecmAsyFit(
  y = ts_y,
  x = ts_x,
  lag = 1,
  model = "mtar",
  split = TRUE,
  thresh = 0
)

summary(fit)

# ===============================
# Part 3: Manual estimation
# ===============================
library(dplyr)
# Prepare lagged and differenced data manually
df <- data %>%
  mutate(
    dy = c(NA, diff(log_ipc)),
    dx = c(NA, diff(log_sipsa)),
    dx_pos = pmax(dx, 0),
    dx_neg = pmin(dx, 0),
    dy_lag = lag(dy),
    ect = lag(log_ipc - log_sipsa),
    ect_pos = pmax(ect, 0),
    ect_neg = pmin(ect, 0)
  ) %>%
  drop_na()

# Manual ECM estimation via OLS
fit_manual <- lm(dy ~ dx_pos + dx_neg + dy_lag + ect_pos + ect_neg, data = df)
summary(fit_manual)

# Compare coefficients
detach("package:dplyr", unload = TRUE)
coef(apt::ecmAsyFit(
  y = ts_y,
  x = ts_x,
  lag = 1,
  model = "mtar",
  split = TRUE,
  thresh = 0
))
coef(fit_manual)
library(dplyr)
# ===============================
# Part 4: Forecast comparison
# ===============================

# Use manual coefficients to compute one-step-ahead predictions
df <- df %>%
  mutate(
    pred_dy = predict(fit_manual),
    pred_log_ipc = lag(log_ipc) + pred_dy
  )

# Compare predictions
rmse_val <- rmse(exp(df$log_ipc), exp(df$pred_log_ipc))
mape_val <- mape(exp(df$log_ipc), exp(df$pred_log_ipc))

cat("\nRMSE (manual forecast):", round(rmse_val, 4))
cat("\nMAPE (manual forecast):", round(100 * mape_val, 2), "%")

# ===============================
# Plot actual vs predicted
# ===============================
plot_df <- df %>%
  mutate(date = date[(n - nrow(df) + 1):n]) %>%
  select(date, obs = log_ipc, pred = pred_log_ipc) %>%
  pivot_longer(-date, names_to = "type", values_to = "value")

ggplot(plot_df, aes(x = date, y = exp(value), color = type)) +
  geom_line(size = 1) +
  labs(title = "Manual ECM Prediction vs Actual", y = "Price level", color = "") +
  scale_color_manual(values = c("obs" = "black", "pred" = "blue"), labels = c("Actual", "Predicted")) +
  theme_minimal()
