

##-----------------------------------##
## Prueba: predicción usando modelos ##
##         de corto y largo plazo    ##
##-----------------------------------##

# Cargar librerías
library(tidyverse)
library(dynlm)
library(ggplot2)
library(tibble)
library(lubridate)
library(forecast)
library(seastests)
library(aTSA)
library(zoo)
library(patchwork)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

## -----------------------##
## 1. Datos               ##
## -----------------------##
# Cargar datos
all_ts = readxl::read_excel("Time-series\\ipc_sipsa_ts.xlsx")

# Cambiar formato
all_ts <- all_ts %>%
  mutate(
    fecha = as.Date(paste(ano, mes_num, "01", sep = "-")),
    precio_ipc = as.numeric(precio_500g),
    precio_sipsa = as.numeric(precio_medio)
  ) %>%
  select(fecha, nombre_ciudad, articulo, sipsa, precio_ipc, precio_sipsa)

# Seleccionar aguacate en Cali (Aguacate y aguacate papelillo)
df.city.food = all_ts %>% filter(nombre_ciudad == "CALI" & 
                                  articulo == "LIMONES" &
                                   sipsa == "Limón Tahití")
  
# Crear la serie de tiempo (precio mayorista)
serie_sipsa <- ts(df.city.food$precio_sipsa,
                  start = c(year(min(df.city.food$fecha))
                            , month(min(df.city.food$fecha))), frequency = 12)

# Crear la serie de tiempo (precio minorista)
serie_ipc <- ts(df.city.food$precio_ipc,
                  start = c(year(min(df.city.food$fecha))
                            , month(min(df.city.food$fecha))), frequency = 12)

## ---------------------------------------------------##
## 2.1 Pruebas de raíz unitaria: precios minoristas   ##
## ---------------------------------------------------##

# Serie en niveles
df_ipc <- tibble(
  date = as.Date(time(serie_ipc)), 
  ipc = as.numeric(serie_ipc)
)
ggplot(df_ipc, aes(x = date, y = ipc)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Serie Precio minorista",
    x = "Fecha", y = "IPC"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12)
  )


# Seasonal plot (facets by year)
df_ipc <- tibble(
  date = as.Date(time(serie_ipc)),
  ipc = as.numeric(serie_ipc),
  year = year(date),
  month = month(date, label = TRUE)
)

ggplot(df_ipc, aes(x = month, y = ipc, group = year, color = factor(year))) +
  geom_line() +
  labs(
    title = "Evolución mensual del precio minorista por año",
    x = "Mes", y = "Precio minorista", color = "Año"
  ) +
  theme_bw()

# Descomposición de la serie de tiempo
ipc_comp <- decompose(serie_ipc)
plot(ipc_comp)

# Desestacionalizar la serie
library(seasonal)
serie_ipc_sa = seas(serie_ipc)

serie_ipc_sa %>% 
  final() %>% 
  autoplot() +
  autolayer(serie_ipc, series = 'Serie original') +
  labs(
    title = 'Precio minorista del aguacate',
    subtitle = 'Ajuste estacional',
    x = '',
    y = 'Precio minorista'
  ) +
  theme(legend.position = "bottom")


# Prueba de raíz unitaria
serie_ipc_sa = predict(serie_ipc_sa)
adf_ipc = summary(urca::ur.df(serie_ipc_sa, 
                                selectlags =  "AIC", 
                                type = "drift"))
adf_ipc

## ---------------------------------------------------##
## 2.1 Pruebas de raíz unitaria: precios mayoristas   ##
## ---------------------------------------------------##

# Prueba sobre los precios mayoristas
# Serie en niveles
df_sipsa <- tibble(
  date = as.Date(time(serie_sipsa)), 
  sipsa = as.numeric(serie_sipsa)
)

ggplot(df_sipsa, aes(x = date, y = sipsa)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Serie Precio Mayorista",
    x = "Fecha", y = "Precio mayorista"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12)
  )

# Seasonal plot (facets by year)
df_sipsa <- tibble(
  date = as.Date(time(serie_sipsa)),
  sipsa = as.numeric(serie_sipsa),
  year = year(date),
  month = month(date, label = TRUE)
)

ggplot(df_sipsa, aes(x = month, y = sipsa, group = year, color = factor(year))) +
  geom_line() +
  labs(
    title = "Evolución mensual del precio mayorista por año",
    x = "Mes", y = "IPC", color = "Año"
  ) +
  theme_bw()

# Descomposición de la serie de tiempo
sipsa_comp <- decompose(serie_sipsa)
plot(sipsa_comp)

# Desestacionalizar la serie
library(seasonal)
serie_sipsa_sa = seas(serie_sipsa)

serie_sipsa_sa %>% 
  final() %>% 
  autoplot() +
  autolayer(serie_sipsa, series = 'Serie original') +
  labs(
    title = 'Precio mayorista',
    subtitle = 'Ajuste estacional',
    x = '',
    y = 'Precio mayorista'
  ) +
  theme(legend.position = "bottom")


# Prueba de raíz unitaria
serie_sipsa_sa = predict(serie_sipsa_sa)
adf_sipsa = summary(urca::ur.df(serie_sipsa_sa, 
                                selectlags =  "AIC", 
                                type = "drift"))
adf_sipsa

## ------------------------------------------##
## 3. Pruebas de cointegración               ##
## ------------------------------------------##

# SIPSA para que termine en marzo-2018
sipsa_coint <- window(serie_sipsa, end = c(2018, 3))
ipc_coint <- window(serie_ipc, start = start(sipsa_coint), 
                    end = end(sipsa_coint))

# Gráficamente:
df_coint <- data.frame(
  Fecha = as.yearmon(time(ipc_coint)),
  IPC = as.numeric(ipc_coint),
  SIPSA = as.numeric(sipsa_coint)
) %>%
  mutate(
    IPC_norm = scale(IPC),
    SIPSA_norm = scale(SIPSA),
    spread = IPC_norm - SIPSA_norm
  ) %>%
  pivot_longer(cols = c(IPC_norm, SIPSA_norm), names_to = "Serie", values_to = "Valor")

# Panel 1: Series normalizadas
p1 <- ggplot(df_coint, aes(x = Fecha, y = Valor, color = Serie)) +
  geom_line(size = 1) +
  theme_bw() +
  labs(title = "Series normalizadas: IPC vs SIPSA",
       y = "Valor normalizado", x = "") +
  scale_color_manual(values = c("IPC_norm" = "steelblue", "SIPSA_norm" = "firebrick")) +
  theme(legend.title = element_blank())

# Panel 2: Spread (diferencia entre series normalizadas)
p2 <- ggplot(df_coint %>% filter(Serie == "IPC_norm"), 
             aes(x = Fecha, y = spread)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(color = "darkgreen", size = 1) +
  theme_bw() +
  labs(title = "Spread: IPC_norm - SIPSA_norm",
       y = "Diferencia", x = "")

p1 / p2


# Cointegración:
coint.output = coint.test(ipc_coint, sipsa_coint)
coint.output

## ------------------------------------------##
## 4. Datos de entrenamiento y validación    ##
## ------------------------------------------##

df.city.food = df.city.food %>% mutate(
  P_min = c(serie_ipc_sa, ts(NA,start = c(2018, 4),
             end = c(2018, 12), frequency = 12)),
  P_may = serie_sipsa_sa
)


df.city.food$mes <- factor(format(df.city.food$fecha,
                                  "%m"))  

train <- df.city.food %>% filter(fecha < "2017-01-01")
test <- df.city.food %>% filter(fecha >= "2017-01-01")

## ------------------------------------------##
## 5. Modelos de largo plazo (cointegración) ##
## ------------------------------------------##
modelo_LP <- lm(P_min ~ P_may, data = train)
train$ecmt <- resid(modelo_LP)

plot.ts(train$ecmt)

## -------------------------------##
## 6. Estimación ECM              ##
## -------------------------------##
train <- train %>%
  mutate(
    d_Pmin = c(NA, diff(P_min)),
    d_Pmay = c(NA, diff(P_may)),
    lag_Pmin = lag(P_min),
    lag_Pmay = lag(P_may),
    lag_ecmt = lag(ecmt),
    d_lag_Pmin = lag(d_Pmin, n = 1),
    d_lag_Pmay = lag(d_Pmay, n = 1),
    mes_n = as.numeric(as.character(mes))
  )

modelo_CP <- lm(d_Pmin ~ d_lag_Pmin + d_lag_Pmay + lag_ecmt, 
                data = train, na.action = na.exclude)

summary(modelo_CP)

## -------------------------------##
## 7. Predicción recursiva        ##
## -------------------------------##

pred <- test
pred$P_min_hat <- NA
prev <- tail(train, 1)

# Asegurar que también el mes sea numérico en test
pred$mes_n <- as.numeric(as.character(pred$mes))

for (i in 1:nrow(pred)) {
  if (i == 1) {
    d_Pmay <- pred$P_may[i] - prev$P_may
    d_lag_Pmin  <- prev$d_lag_Pmin
    d_lag_Pmay <- prev$d_lag_Pmay
    lag_ecmt <- prev$ecmt
    last_Pmin_hat <- prev$P_min
  } else {
    d_Pmay <- pred$P_may[i] - pred$P_may[i - 1]
    lag_Pmin <- pred$P_min_hat[i - 1]
    lag_Pmay <- pred$P_may[i - 1]
    lag_ecmt <- lag_Pmin - (coef(modelo_LP)[1] + coef(modelo_LP)[2] * lag_Pmay)
    last_Pmin_hat <- pred$P_min_hat[i - 1]
    
    d_lag_Pmin <- pred$P_min_hat[i - 1] - pred$P_min_hat[i - 2]
    d_lag_Pmay <- pred$P_may[i - 1] - pred$P_may[i - 2]
  }
  
  mes_n <- pred$mes_n[i]
  x <- c(1, d_lag_Pmin, d_lag_Pmay, lag_ecmt)
  
  if (any(is.na(x))) {
    delta_hat <- 0
  } else {
    delta_hat <- sum(coef(modelo_CP) * x)
  }
  
  pred$P_min_hat[i] <- last_Pmin_hat + delta_hat
}


## ----------------------------------##
## 8. Visualización de la predicción ##
## ----------------------------------##
ggplot() +
  geom_line(data = df.city.food, aes(x = fecha, y = P_min, color = "Real")) +
  geom_line(data = pred, aes(x = fecha, y = P_min_hat, color = "Predicción")) +
  labs(title = "Predicción ECM",
       y = "Precio minorista",
       color = "") +
  theme_bw() + theme(legend.position = "bottom")

# -----------------------------
# 9. Error de predicción
# -----------------------------
rmse <- sqrt(mean((pred$P_min - pred$P_min_hat)^2, na.rm = TRUE))
cat("RMSE en validación:", round(rmse, 3), "\n")
