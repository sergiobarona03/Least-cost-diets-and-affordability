##################################################################
## Modelo ECM con log() + validación + IC                      ##
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

data.food <- dataset %>% filter(alimento_sipsa == "Aguacate papelillo") %>%
  drop_na(precio_ipc, precio_sipsa)

# Usar log precios
data.food <- data.food %>%
  mutate(
    log_ipc    = log(precio_ipc),
    log_sipsa  = log(precio_sipsa),
    fecha      = as.Date(paste(Year, Month, 1, sep = "-"))
  )

# Partición 70/30
n <- nrow(data.food)
cut <- floor(0.7 * n)

train <- data.food[1:cut, ]
test  <- data.food[(cut + 1):n, ]

# Ajustar ECM con log precios
ecm_fit <- ecm(
  y    = data.frame(y = train$log_ipc),
  xeq  = data.frame(x = train$log_sipsa),
  xtr  = data.frame(x = train$log_sipsa)
)
summary(ecm_fit)

# Predecir en test set de forma recursiva
b0 <- coef(ecm_fit)["(Intercept)"]
b1 <- coef(ecm_fit)["deltax"]
b2 <- coef(ecm_fit)["xLag1"]
b3 <- coef(ecm_fit)["yLag1"]
se   <- summary(ecm_fit)$sigma

# Datos para predicción
test_x     <- test$log_sipsa
prev_x     <- lag(data.food$log_sipsa, 1)[(cut + 1):n]
prev_y     <- lag(data.food$log_ipc, 1)[(cut + 1):n]

y_hat <- numeric(length(test_x))
y_lo  <- numeric(length(test_x))
y_hi  <- numeric(length(test_x))

# Predicción recursiva con IC (±1.96 * sigma)
y_hat[1] <- prev_y[1] + (b0 + b1 * (test_x[1] - prev_x[1]) + b2 * prev_x[1] + b3 * prev_y[1])
y_lo[1]  <- exp(y_hat[1] - 1.96 * se)
y_hi[1]  <- exp(y_hat[1] + 1.96 * se)

for (i in 2:length(test_x)) {
  prev_y_i <- y_hat[i - 1]
  delta_y  <- b0 + b1 * (test_x[i] - test_x[i - 1]) + b2 * test_x[i - 1] + b3 * prev_y_i
  y_hat[i] <- prev_y_i + delta_y
  y_lo[i]  <- exp(y_hat[i] - 1.96 * se)
  y_hi[i]  <- exp(y_hat[i] + 1.96 * se)
}

# Calcular error (volver a nivel original)
actual <- exp(test$log_ipc)
pred   <- exp(y_hat)

rmse_val <- rmse(actual, pred)
mape_val <- mape(actual, pred)

cat("\nRMSE:", round(rmse_val, 2))
cat("\nMAPE:", round(100 * mape_val, 2), "%\n")

# Graficar resultado
train_df <- tibble(
  fecha = train$fecha,
  obs = exp(train$log_ipc),
  pred = NA,
  lo = NA,
  hi = NA
)

plot_df <- tibble(
  fecha   = test$fecha,
  obs     = actual,
  pred    = pred,
  lo      = y_lo,
  hi      = y_hi
)

plot_df = rbind(train_df, plot_df)

# Graficar con IC

ggplot(plot_df, aes(x = fecha)) +
  geom_line(aes(y = obs, colour = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = pred, colour = "Predicho"), linetype = "dashed", linewidth = 0.7) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "red", col= "black", alpha = 0.3,
              linetype = 2) +
  scale_color_manual(values = c("Observado" = "black", "Predicho" = "red")) +
  labs(
    title = "ECM - Predicción de precios minoristas (IPC)",
    subtitle = paste0("Aguacate papelillo - Cali | MAPE: ", round(100 * mape_val, 2), "%"),
    x = "Fecha",
    y = "Precio (nivel)",
    colour = ""
  ) +
  theme_minimal()
