########################################################
## Metodología 2: Estimación del margen de comercialización
## mediano (Q1 y Q3), por ciudad (CALI, BOGOTÁ D.C., MEDELLÍN)
##
## Sin split train/test:
## - Se estima margen Q1–Q2–Q3 usando solo filas con
##   precio_ipc y precio_sipsa > 0
## - Se aplica el margen a TODA la base con precio_sipsa
##   para obtener precios IPC estimados
##
## Fuente de datos: DANE - IPC y DANE-SIPSA
## Objetivo: a partir de la unión IPC–SIPSA, calcular un
## margen constante por producto *y ciudad* y usarlo para
## proyectar precios minoristas hacia adelante.
########################################################

# -----------------------
# 0. Directorio base y fecha de estimación
# -----------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

date_tag <- "121225"  # etiqueta para identificar los outputs

# -----------------------
# 1. Cargar librerías y definir directorios
# -----------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(ggplot2)
library(writexl)
library(ggforce)

# Conjunto de datos input (IPC-SIPSA unido, multi-ciudad)
in_merged <- file.path(
  base_dir, "working-papers", "working-paper-q2", 
  "mapeo ipc-sipsa", "output",
  paste0(date_tag, "_dataset_ipc_sipsa.xlsx")
)

data_merged <- read_excel(in_merged)

data_merged <- data_merged %>%
  mutate(ciudad = as.character(ciudad))

# Ruta de output
out_dir <- file.path(
  base_dir, "working-papers", "working-paper-q2", "output",
  "q2_fullsample"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# 2. Cálculo de márgenes por producto Y CIUDAD (frecuencia mensual)
# -----------------------

# 2.1. Cálculo de márgenes usando solo filas donde hay IPC y SIPSA
data_margenes <- data_merged %>%
  filter(
    !is.na(precio_sipsa),
    !is.na(precio_ipc),
    precio_sipsa > 0
  ) %>%
  mutate(
    margen = (precio_ipc - precio_sipsa) / precio_sipsa * 100
  )

# 2.2. Margen Q1–Q2–Q3 por producto y ciudad
#     clave: ciudad + alimento_sipsa + articulo_ipc + codigo_articulo
margenes_q <- data_margenes %>%
  group_by(ciudad, alimento_sipsa) %>%
  summarise(
    n_obs     = n(),
    margen_q1 = quantile(margen, 0.25, na.rm = TRUE),
    margen_q2 = quantile(margen, 0.50, na.rm = TRUE),
    margen_q3 = quantile(margen, 0.75, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  # exigir un mínimo de observaciones para que el margen sea “estable”
  filter(n_obs >= 12)

# 2.3. Guardar márgenes Q1–Q3
write_csv(
  margenes_q,
  file.path(out_dir, paste0(date_tag, "_margenes_q1_q3_sipsa_by_city.csv"))
)

write_xlsx(
  list(margenes_q = margenes_q),
  file.path(out_dir, paste0(date_tag, "_margenes_q1_q3_sipsa_by_city.xlsx"))
)

# -----------------------
# 3. Construcción del conjunto de datos para predicción
#    (sin train/test, se usa toda la muestra con SIPSA)
# -----------------------

# Nota clave:
# - Para ESTIMAR márgenes, usamos solo filas con IPC y SIPSA.
# - Para APLICAR márgenes, usamos TODAS las filas donde haya SIPSA,
#   aunque no haya IPC (es justo donde queremos predecir).

data_pred <- data_merged %>%
  # solo necesitamos precio_sipsa > 0 para poder predecir
  filter(
    !is.na(precio_sipsa),
    precio_sipsa > 0) %>%
  # unimos los márgenes por producto Y ciudad
  left_join(
    margenes_q %>%
      select(ciudad, alimento_sipsa, 
             margen_q1, margen_q2, margen_q3),
    by = c("ciudad", "alimento_sipsa")
  ) %>%
  # construimos fecha mensual si no la tienes ya
  mutate(
    date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(ciudad, alimento_sipsa, date)


# -----------------------
# 4. Estimación de precios IPC a partir de SIPSA + márgenes
# -----------------------

data_pred <- data_pred %>%
  mutate(
    precio_ipc_pred_q1 = if_else(
      !is.na(margen_q1),
      precio_sipsa * (1 + margen_q1 / 100),
      NA_real_
    ),
    precio_ipc_pred_q2 = if_else(
      !is.na(margen_q2),
      precio_sipsa * (1 + margen_q2 / 100),
      NA_real_
    ),
    precio_ipc_pred_q3 = if_else(
      !is.na(margen_q3),
      precio_sipsa * (1 + margen_q3 / 100),
      NA_real_
    )
  )

# -----------------------
# 5. Métricas de ajuste (opcional) donde sí hay IPC observado
#    (sirve para ver qué tan bien funciona el margen mediano)
# -----------------------

data_eval <- data_pred %>%
  filter(!is.na(precio_ipc)) %>%
  mutate(
    error_q2 = precio_ipc_pred_q2 - precio_ipc,
    ape_q2   = abs(error_q2) / precio_ipc * 100,
    error_q1 = precio_ipc_pred_q1 - precio_ipc,
    ape_q1   = abs(error_q1) / precio_ipc * 100,
    error_q3 = precio_ipc_pred_q3 - precio_ipc,
    ape_q3   = abs(error_q3) / precio_ipc * 100
  )

# Resumen global de los APE (todas las ciudades)
resumen_ape_global <- data_eval %>%
  summarise(
    n_obs      = n(),
    mape_q1    = mean(ape_q1, na.rm = TRUE),
    mape_q2    = mean(ape_q2, na.rm = TRUE),
    mape_q3    = mean(ape_q3, na.rm = TRUE),
    med_ape_q2 = median(ape_q2, na.rm = TRUE)
  )

# Resumen por ciudad (útil para ver si el margen funciona mejor/peor según ciudad)
resumen_ape_city <- data_eval %>%
  group_by(ciudad) %>%
  summarise(
    n_obs      = n(),
    mape_q1    = mean(ape_q1, na.rm = TRUE),
    mape_q2    = mean(ape_q2, na.rm = TRUE),
    mape_q3    = mean(ape_q3, na.rm = TRUE),
    med_ape_q2 = median(ape_q2, na.rm = TRUE),
    .groups    = "drop"
  )

print(resumen_ape_global)
print(resumen_ape_city)

# -----------------------
# 6. Guardar outputs finales
# -----------------------

# 6.1. Base de predicciones completa (con y sin IPC observado)
write_csv(
  data_pred,
  file.path(out_dir, paste0(date_tag, "_ipc_pred_from_sipsa_full_by_city.csv"))
)

saveRDS(
  data_pred,
  file.path(out_dir, paste0(date_tag, "_ipc_pred_from_sipsa_full_by_city.rds"))
)

message("DONE. Outputs en: ", out_dir)

message("Ciudades incluidas en data_pred: ",
        paste(unique(data_pred$ciudad), collapse = ", "))
