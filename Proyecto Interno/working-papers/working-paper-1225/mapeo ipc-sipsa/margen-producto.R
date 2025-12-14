##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Cargar datos (debe crear retail_99_18, whole_18_mean, ipc_sipsa, etc.)
source("working-papers\\working-paper-1225\\mapeo ipc-sipsa\\mapeo-ipc-sipsa.R")

date_tag = "121225"

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

# -------------------------------------------------
# 3. Tabla de mapeo IPC–SIPSA
# -------------------------------------------------
mapa <- ipc_sipsa %>%
  select(
    alimento_sipsa = sipsa,
    articulo_ipc   = retail
  )

# -------------------------------------------------
# 4. Unir las tres fuentes
# -------------------------------------------------
data_merged <- wholesale %>%
  left_join(mapa, by = "alimento_sipsa") %>%
  left_join(
    retail,
    by = c("articulo_ipc", "Year" = "ano", "Month" = "mes_num")
  ) %>% na.omit()


path = paste0("working-papers\\working-paper-1225\\mapeo ipc-sipsa\\output\\",
              date_tag, "_dataset_ipc_sipsa.xlsx")

writexl::write_xlsx(data_merged, path)
