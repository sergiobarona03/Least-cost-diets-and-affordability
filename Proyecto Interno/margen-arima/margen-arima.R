
##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# Cargar datos
source("margen-arima/margen-producto.R")

# Vector de alimentos
food_k = levels(as.factor(margen_articulo$sipsa))

# Serie k
k = 2

# Predicción de margen de comercialización
sk <- margen_articulo %>% filter(sipsa == food_k[k])

plot(x = sk$fecha, y = sk$margen, type = "l")

# Extraer año y mes inicial y final
start <- c(year(min(sk$fecha)), month(min(sk$fecha)))
end   <- c(year(max(sk$fecha)), month(max(sk$fecha)))

# Crear la serie temporal con frecuencia mensual (12)
sk_ts <- ts(sk$margen, start = start, end = end, frequency = 12)

# Validar modelo arima:
n90 <- floor(length(sk_ts) * 0.6)

# Train conserva el índice temporal
train <- window(sk_ts, end = time(sk_ts)[n90])

# Test conserva el índice temporal
test <- window(sk_ts, start = time(sk_ts)[n90])

library(forecast)

fit <- auto.arima(train)

h <- length(test)
fc <- forecast(fit, h = 100)


autoplot(fc) + autolayer(test, series = "Test")






