##################################################################
## Asymmetric ECM with log(), prediction, and CI (APT package) ##
##################################################################

library(tidyverse)
library(readxl)
library(apt)
library(Metrics)
library(ggplot2)
library(janitor)
library(seasonal)
library(dplyr)

setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Load data
infile <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
dataset <- read_excel(infile)

data.food <- dataset %>%
  dplyr::filter(alimento_sipsa == "Aguacate papelillo") %>%
  drop_na(precio_ipc, precio_sipsa) %>%
  dplyr::mutate(
    log_ipc   = log(precio_ipc),
    log_sipsa = log(precio_sipsa),
    fecha     = as.Date(paste(Year, Month, 1, sep = "-"))
  )

# Ajuste estacional
data.food.ipc = ts(data.food$log_ipc,
                     start = c(data.food$Year[1], data.food$Month[1]), frequency = 12)
data.food.sipsa = ts(data.food$log_sipsa,
                     start = c(data.food$Year[1], data.food$Month[1]), frequency = 12)

data.food <- data.food %>%
  mutate(log_ipc = predict(seas(data.food.ipc)) %>% as.vector(),
         log_sipsa = predict(seas(data.food.sipsa)) %>% as.vector())

# Partition 70/30
n <- nrow(data.food)
cut <- floor(0.7 * n)

train <- data.food[1:cut, ]
test  <- data.food[(cut + 1):n, ]

# Create ts objects
ts_y <- ts(train$log_ipc, start = c(train$Year[1], train$Month[1]), frequency = 12)
ts_x <- ts(train$log_sipsa, start = c(train$Year[1], train$Month[1]), frequency = 12)

# Fit asymmetric ECM (MTAR, lag=1)
detach("package:dplyr", unload = TRUE)
fit_asy <- apt::ecmAsyFit(
  y      = ts_y,
  x      = ts_x,
  lag    = 1,
  model  = "mtar",
  thresh = 0,
  split  = TRUE
)
library(dplyr)

# Limpieza y estructuración de coef_table
coef_table <- summary(fit_asy) %>%
  janitor::clean_names() %>%
  mutate(
    dep_var = str_trim(dep_var),
    dep_var = str_replace_all(dep_var, "[\\|\\-]", ""),
    dep_var = str_trim(dep_var),
    dep_var = na_if(dep_var, "")  # convertir cadenas vacías en NA reales
  ) %>%
  fill(dep_var)

# Helper function
get_coef <- function(dep, name) {
  coef_table %>%
    filter(dep_var == dep, ind_var == name) %>%
    pull(estimate)
}

# Extract coefficients for diff.ts_y.t_0
b0      <- get_coef("diff.ts_y.t_0", "(Intercept)")
bDypos  <- get_coef("diff.ts_y.t_0", "X.diff.ts_y.t_1.pos")
bDyneg  <- get_coef("diff.ts_y.t_0", "X.diff.ts_y.t_1.neg")
bDpos   <- get_coef("diff.ts_y.t_0", "X.diff.ts_x.t_1.pos")
bDneg   <- get_coef("diff.ts_y.t_0", "X.diff.ts_x.t_1.neg")
bECTpos <- get_coef("diff.ts_y.t_0", "X.ECT.t_1.pos")
bECTneg <- get_coef("diff.ts_y.t_0", "X.ECT.t_1.neg")


# Create lags and diffs
full_data <- data.food %>%
  mutate(
    dy        = c(NA, diff(log_ipc)),
    dx        = c(NA, diff(log_sipsa)),
    dx_pos    = pmax(dx, 0),
    dx_neg    = pmin(dx, 0),
    log_ratio = log_ipc - log_sipsa,
    lz        = lag(log_ratio)
  )

# Predict
test_rows <- (cut + 1):n
y_hat <- numeric(length(test_rows))
y_lo  <- numeric(length(test_rows))
y_hi  <- numeric(length(test_rows))

for (i in seq_along(test_rows)) {

  idx <- test_rows[i]
  
  dx_p <- full_data$dx_pos[idx]
  dx_n <- full_data$dx_neg[idx]
  dy_lag <- full_data$dy[idx - 1]
  lz_val <- full_data$lz[idx]
  
  adj_term <- ifelse(lz_val < 0,
                     bECTneg * lz_val, bECTpos * lz_val)
  
  delta_y <- b0 + bDypos * dy_lag + bDyneg * dy_lag + bDpos * dx_p + bDneg * dx_n + adj_term
  
  if (i == 1) {
    y_hat[i] <- full_data$log_ipc[idx - 1] + delta_y
  } else {
    y_hat[i] <- y_hat[i - 1] + delta_y
  }
  
  y_lo[i] <- exp(y_hat[i] - 1.96 * 0.06119 )
  y_hi[i] <- exp(y_hat[i] + 1.96 * 0.06119 )
}

# Metrics
actual <- exp(test$log_ipc)
pred   <- exp(y_hat)

rmse_val <- rmse(actual, pred)
mape_val <- mape(actual, pred)

cat("\nRMSE:", round(rmse_val, 2))
cat("\nMAPE:", round(100 * mape_val, 2), "%\n")

# Plot
train_df <- tibble(
  fecha = train$fecha,
  obs   = exp(train$log_ipc),
  pred  = NA,
  lo    = NA,
  hi    = NA
)

plot_df <- tibble(
  fecha = test$fecha,
  obs   = actual,
  pred  = pred,
  lo    = y_lo,
  hi    = y_hi
)

plot_df <- bind_rows(train_df, plot_df)

ggplot(plot_df, aes(x = fecha)) +
  geom_line(aes(y = obs, colour = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = pred, colour = "Predicho"), linetype = "dashed", linewidth = 0.7) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "red", color = "black", alpha = 0.3, linetype = 2) +
  scale_color_manual(values = c("Observado" = "black", "Predicho" = "red")) +
  labs(
    title = "Asymmetric ECM - Predicción de precios minoristas (IPC)",
    subtitle = paste0("Aguacate papelillo - Cali | MAPE: ", round(100 * mape_val, 2), "%"),
    x = "Fecha",
    y = "Precio (nivel)",
    colour = ""
  ) +
  theme_minimal()
