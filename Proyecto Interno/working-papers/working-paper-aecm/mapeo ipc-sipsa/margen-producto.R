##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

# Cargar librerías
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\sergio.barona\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# Cargar datos (debe crear retail_99_18, whole_18_mean, ipc_sipsa, etc.)
source("working-papers\\working-paper-aecm\\mapeo ipc-sipsa\\mapeo-ipc-sipsa.R")

date_tag = "261225"

# Selección de alimentos
# Lista: papa, plátano, arroz, tomate, zanahoria, cebolla, yuca
food_vec = c("PAPA", "PLÁTANO", "ARROZ PARA SECO",
             "TOMATE", "ZANAHORIA", "YUCA", "CEBOLLA CABEZONA")


# -------------------------------------------------
# 1. Datos de precios minoristas (IPC)
# -------------------------------------------------
retail <- retail_99_18 %>%
  rename(
    precio_ipc   = precio_500g,
    articulo_ipc = articulo
  ) %>%
  filter(ciudad %in% c("5","05", "11", "76")) %>% 
  mutate(cod_mun = NA)

retail$cod_mun[retail$ciudad == "05"] = "05001"
retail$cod_mun[retail$ciudad == "5"] = "05001"
retail$cod_mun[retail$ciudad == "11"] = "11001"
retail$cod_mun[retail$ciudad == "76"] = "76001"

retail = retail %>% select(cod_mun, articulo_ipc, ano, mes_num, precio_ipc)

# -------------------------------------------------
# 2. Datos de precios mayoristas (SIPSA)
# -------------------------------------------------
wholesale <- whole_18_mean %>%
  rename(
    precio_sipsa   = precio_medio,
    alimento_sipsa = Alimento
  ) %>%
  filter(cod_mun %in% c("05001", "11001", "76001")) %>%
  select(cod_mun, alimento_sipsa, Year, Month, precio_sipsa)

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
    by = c("cod_mun", "Year" = "ano", "Month" = "mes_num", "articulo_ipc")
  ) 

# Seleccionar los alimentos de interés
data_merged = data_merged %>% filter(articulo_ipc %in% food_vec)

# -------------------------------------------------
# 5. Guardar en ruta
# -------------------------------------------------
path = paste0("working-papers\\working-paper-aecm\\input\\",
              date_tag, "_dataset_ipc_sipsa.xlsx")

writexl::write_xlsx(data_merged, path)
