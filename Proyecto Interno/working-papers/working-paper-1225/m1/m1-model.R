###################################################
## Metodología 1: Estimación a partir del margen ##
###################################################

# Cargar librerías necesarias
library(lubridate)
library(tidyverse)
library(readxl)
library(readr)

# Definir directorio de trabajo
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Definir la etiqueta de la fecha (usar la fecha específica de tu archivo)
date_tag = "121225"

# Leer el archivo de datos
data_merged <- readxl::read_excel(paste0("working-papers\\working-paper-1225\\mapeo ipc-sipsa\\output\\", 
                                         date_tag, "_dataset_ipc_sipsa.xlsx"))

# Inspeccionar las primeras filas del conjunto de datos
head(data_merged)

# Paso 1: Calcular el margen de comercialización para cada combinación de producto y mes
data_margenes <- data_merged %>%
  filter(
    !is.na(precio_sipsa),
    !is.na(precio_ipc),
    precio_sipsa > 0  # Asegurarse de que no hay valores de precio <= 0
  ) %>%
  mutate(
    margen = (precio_ipc - precio_sipsa) / precio_sipsa * 100  # Calcular margen en porcentaje
  )

# Paso 2: Calcular el margen en el percentil 25 (Q1), mediana (Q2), y percentil 75 (Q3)
margenes_q <- data_margenes %>%
  group_by(alimento_sipsa, articulo_ipc, codigo_articulo) %>%
  summarise(
    n_obs = n(),  # Número de observaciones por alimento
    margen_q1 = quantile(margen, 0.25, na.rm = TRUE),  # Percentil 25 (Q1)
    margen_q2 = quantile(margen, 0.50, na.rm = TRUE),  # Mediana (Q2)
    margen_q3 = quantile(margen, 0.75, na.rm = TRUE),  # Percentil 75 (Q3)
    .groups = "drop"
  )

# Paso 3: Filtro para asegurar que cada alimento tenga un mínimo de observaciones
# Puedes ajustar el filtro según tus necesidades (por ejemplo, al menos 12 observaciones)
margenes_q <- margenes_q %>%
  filter(n_obs >= 12)  # Filtro mínimo de observaciones

# Paso 4: Exportar los resultados a un archivo CSV
readr::write_csv(
  margenes_q,
  paste0("working-papers\\working-paper-1225\\m1\\output\\", date_tag, "_margenes_q1_q3_sipsa.csv")
)

# Ver los primeros resultados para comprobar
head(margenes_q)
