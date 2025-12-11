
##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd( "C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Cargar datos
source("margen-dist/v1-join-ipc-sipsa.R")

# -------------------------------------------------
# 1. Datos de precios minoristas (IPC)
# -------------------------------------------------
retail <- retail_99_18 %>%
  rename(
    precio_ipc   = precio_500g,
    articulo_ipc = articulo
  ) %>%
  filter(ciudad == "76") %>%   # Cali
  select(articulo_ipc, codigo_articulo, ano, mes_num, precio_ipc)

# -------------------------------------------------
# 2. Datos de precios mayoristas (SIPSA)
# -------------------------------------------------
wholesale <- whole_18_mean %>%
  rename(
    precio_sipsa   = precio_medio,
    alimento_sipsa = Alimento
  ) %>%
  filter(cod_mun == "76001") %>%
  select(alimento_sipsa, Year, Month, precio_sipsa)

# ------------------------------
# 3. Mapping table
# ------------------------------
mapa <- ipc_sipsa %>%
  select(alimento_sipsa = sipsa, articulo_ipc = retail)

# ------------------------------
# 4. Merge the three sources
# ------------------------------
data_merged <- wholesale %>%
  left_join(mapa, by = "alimento_sipsa") %>%
  left_join(retail, by = c("articulo_ipc", "Year" = "ano", "Month" = "mes_num"))

# ------------------------------
# 5. Compute margins
# ------------------------------
margenes <- data_merged %>%
  mutate(
    factor = precio_ipc / precio_sipsa,
    margen = (factor - 1) * 100
  ) %>%
  filter(!is.na(margen), margen > 0, margen < 300)  # optional trimming

# ------------------------------
# 6. Compute quartiles per product
# ------------------------------
cuartiles <- margenes %>%
  group_by(alimento_sipsa) %>%
  summarize(
    q1 = quantile(margen, 0.25, na.rm = TRUE),
    q2 = quantile(margen, 0.50, na.rm = TRUE),
    q3 = quantile(margen, 0.75, na.rm = TRUE),
    n  = n()
  )


# Guardar el margen por artículo
readr::write_csv(cuartiles,
                 "margen-dist/output-ciudades/CALI/111225_q1_q3_margen_producto.csv")
