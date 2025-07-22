library(tidyverse)
library(dynlm)

## -----------------------##
## 2. Datos sintéticos    ##
## -----------------------##
set.seed(2107)
n <- 200
fecha <- seq(as.Date("2020-01-01"), by = "day", length.out = n)
P_may <- cumsum(rnorm(n, mean = 0.1, sd = 0.5))
error <- rnorm(n, mean = 0, sd = 0.3)
P_min <- 2 + 1.2 * P_may + error

datos <- data.frame(fecha, P_may, P_min)

## ------------------------------------------##
## 2. Datos de entrenamiento y validación    ##
## ------------------------------------------##
train <- datos[1:160, ]
test <- datos[161:200, ]

## ------------------------------------------##
## 3. Modelos de largo plazo (cointegración) ##
## ------------------------------------------##
modelo_LP <- lm(P_min ~ P_may, data = train)
train$ecmt <- resid(modelo_LP)

## -------------------------------##
## 4. Estimación ECM              ##
## -------------------------------##
train <- train %>%
  mutate(
    d_Pmin = c(NA, diff(P_min)),
    d_Pmay = c(NA, diff(P_may)),
    lag_Pmin = lag(P_min),
    lag_Pmay = lag(P_may),
    lag_ecmt = lag(ecmt),
    lag2_ecmt = lag(ecmt, n = 2)
  )

modelo_CP <- lm(d_Pmin ~ lag2_ecmt + lag_Pmin + lag_Pmay + d_Pmay, 
                data = train, na.action = na.exclude)

summary(modelo_CP)

## -------------------------------##
## 5. Predicción recursiva        ##
## -------------------------------##
pred <- test
pred$P_min_hat <- NA

# Usamos últimos valores del entrenamiento como punto de partida
prev <- tail(train, 1)

for (i in 1:nrow(pred)) {
  # Variables de entrada para delta_hat
  if (i == 1) {
    d_Pmay <- pred$P_may[i] - prev$P_may
    lag_Pmin <- prev$P_min
    lag_Pmay <- prev$P_may
    lag_ecmt <- prev$ecmt
    last_Pmin_hat <- prev$P_min
  } else {
    d_Pmay <- pred$P_may[i] - pred$P_may[i - 1]
    lag_Pmin <- pred$P_min_hat[i - 1]
    lag_Pmay <- pred$P_may[i - 1]
    lag_ecmt <- lag_Pmin - (coef(modelo_LP)[1] + coef(modelo_LP)[2] * lag_Pmay)
    last_Pmin_hat <- pred$P_min_hat[i - 1]
  }
  
  # Calcular delta_hat
  x <- c(1, lag_ecmt, lag_Pmin, lag_Pmay, d_Pmay)
  if (any(is.na(x))) {
    delta_hat <- 0  # alternativa: NA, pero eso rompería la predicción recursiva
  } else {
    delta_hat <- sum(coef(modelo_CP) * x)
  }
  
  # Predicción recursiva
  pred$P_min_hat[i] <- last_Pmin_hat + delta_hat
}

## ----------------------------------##
## 6. Visualización de la predicción ##
## ----------------------------------##
ggplot() +
  geom_line(data = datos, aes(x = fecha, y = P_min, color = "Real")) +
  geom_line(data = pred, aes(x = fecha, y = P_min_hat, color = "Predicción")) +
  labs(title = "Predicción dentro de muestra (ECM)",
       y = "Precio minorista",
       color = "") +
  theme_bw()

# -----------------------------
# 7. Error de predicción
# -----------------------------
rmse <- sqrt(mean((pred$P_min - pred$P_min_hat)^2, na.rm = TRUE))
cat("RMSE en validación:", round(rmse, 3), "\n")
