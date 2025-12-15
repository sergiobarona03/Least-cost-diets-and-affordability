##################################################################
## Modelo ECM con log() + validación + IC + forecast estable ##
##################################################################

library(tidyverse)
library(readxl)
library(ecm)
library(Metrics)
library(ggplot2)

setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Cargar datos
infile <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
dataset <- read_excel(infile)

data.food <- dataset %>% 
  filter(alimento_sipsa == "Aguacate papelillo") %>%
  drop_na(precio_ipc, precio_sipsa)

# Usar log precios y crear variables
data.food <- data.food %>%
  mutate(
    log_ipc    = log(precio_ipc),
    log_sipsa  = log(precio_sipsa),
    mes        = factor(Month),
    fecha      = as.Date(paste(Year, Month, 1, sep = "-"))
  ) %>% filter(fecha <= "2017-12-01")

# Partición 70/30
n <- nrow(data.food)
cut <- floor(0.7 * n)

train <- data.food[1:cut, ]
test  <- data.food[(cut + 1):n, ]

# Ajustar ECM con log precios e incluir dummies mensuales en xtr
xtr_train <- model.matrix(~ log_sipsa + mes, data = train)[, -1]  # sin intercepto duplicado

ecm_fit <- ecm(
  y    = data.frame(y = train$log_ipc),
  xeq  = data.frame(x = train$log_sipsa),
  xtr  = as.data.frame(xtr_train)
)
summary(ecm_fit)

# Predicción one-step-ahead usando valores reales de y_{t-1} (evitar acumulación de error)
test <- test %>%
  mutate(
    log_ipc_lag    = lag(log_ipc, 1),
    log_sipsa_lag  = lag(log_sipsa, 1),
    delta_sipsa    = log_sipsa - log_sipsa_lag
  )

# Crear variables para predicción con dummies
xtr_test <- model.matrix(~ delta_sipsa + mes, data = test)[, -1]  # mismo diseño que train

# Construir predicción: delta log_ipc = b0 + b1 * delta_sipsa + ... + b_eq * lag_eq + b_y * y_{t-1}
coefs <- coef(ecm_fit)

y_hat_log <- numeric(nrow(test))
y_lo      <- numeric(nrow(test))
y_hi      <- numeric(nrow(test))
se        <- summary(ecm_fit)$sigma

for (i in 1:nrow(test)) {
  delta_x <- as.numeric(xtr_test[i, ])
  xLag1   <- test$log_sipsa_lag[i]
  yLag1   <- test$log_ipc_lag[i]
  
  if (any(is.na(c(delta_x, xLag1, yLag1)))) {
    y_hat_log[i] <- NA
    y_lo[i]      <- NA
    y_hi[i]      <- NA
  } else {
    delta_y <- sum(coefs[-c(1, length(coefs)-1, length(coefs))] * delta_x) +
      coefs["xLag1"] * xLag1 +
      coefs["yLag1"] * yLag1 +
      coefs["(Intercept)"]
    
    y_hat_log[i] <- yLag1 + delta_y
    y_lo[i]      <- exp(y_hat_log[i] - 1.96 * se)
    y_hi[i]      <- exp(y_hat_log[i] + 1.96 * se)
  }
}

# Resultados
pred   <- exp(y_hat_log)
actual <- exp(test$log_ipc)

rmse_val <- rmse(actual, pred)
mape_val <- mape(actual, pred)

cat("\nRMSE:", round(rmse_val, 2))
cat("\nMAPE:", round(100 * mape_val, 2), "%\n")

# Graficar resultado
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

# Plot final

ggplot(plot_df, aes(x = fecha)) +
  geom_line(aes(y = obs, colour = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = pred, colour = "Predicho"), linetype = "dashed", linewidth = 0.7) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "red", col = NA, alpha = 0.3) +
  scale_color_manual(values = c("Observado" = "black", "Predicho" = "red")) +
  labs(
    title = "ECM - Predicción de precios minoristas (IPC)",
    subtitle = paste0("Aguacate papelillo - Cali | MAPE: ", round(100 * mape_val, 2), "%"),
    x = "Fecha",
    y = "Precio (nivel)",
    colour = ""
  ) +
  theme_minimal()
