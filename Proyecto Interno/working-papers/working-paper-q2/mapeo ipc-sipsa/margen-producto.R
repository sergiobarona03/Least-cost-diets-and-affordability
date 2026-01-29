##################################################################
## Prueba: análisis sobre margen de comercialización (subclase) ##
## Multi-ciudad                                                 ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

library(lubridate)
library(tidyverse)
library(writexl)

#------------------------------------------------------
# 0. Configuración
#------------------------------------------------------
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Cargar datos previos (debe generar retail_99_18, whole_18_mean, ipc_sipsa)
source("working-papers\\working-paper-q2\\mapeo ipc-sipsa\\mapeo-ipc-sipsa.R")

date_tag = "121225"

#------------------------------------------------------
# 1. Definir ciudades a procesar
#------------------------------------------------------
ciudades <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

# Mapa retail ↔ SIPSA (si no lo tienes, lo asumimos aquí)
city_map <- tibble(
  ciudad_retail = c("76", "11", "05"),   # codigos usados en retail_99_18 (ejemplo)
  ciudad_nombre = c("CALI", "BOGOTÁ D.C.", "MEDELLÍN"),
  cod_mun_sipsa = c("76001", "11001", "05001")  # cod_mun de SIPSA
)

#------------------------------------------------------
# 2. Normalizar bases retail y sipsa
#------------------------------------------------------

# --- Retail (IPC)
retail <- retail_99_18 %>%
  rename(
    precio_ipc   = precio_500g,
    articulo_ipc = articulo
  ) %>%
  mutate(ciudad_retail = ciudad) %>%
  select(articulo_ipc, codigo_articulo, ciudad_retail, ano, mes_num, precio_ipc)

retail$ciudad_retail[retail$ciudad_retail == "5"] = "05"

# --- Wholesale (SIPSA)
wholesale <- whole_18_mean %>%
  rename(
    precio_sipsa   = precio_medio,
    alimento_sipsa = Alimento
  ) %>%
  mutate(cod_mun_sipsa = cod_mun) %>%
  select(alimento_sipsa, cod_mun_sipsa, Year, Month, precio_sipsa)

# --- Mapeo IPC ↔ SIPSA
mapa <- ipc_sipsa %>%
  select(
    alimento_sipsa = sipsa,
    articulo_ipc   = retail
  )

#------------------------------------------------------
# 3. Generar merge por ciudad y unir resultados
#------------------------------------------------------

lista_ciudades <- list()

for (ci in ciudades) {
  
  # obtener codigos retail & sipsa
  c_retail <- city_map %>% filter(ciudad_nombre == ci) %>% pull(ciudad_retail)
  c_sipsa  <- city_map %>% filter(ciudad_nombre == ci) %>% pull(cod_mun_sipsa)
  
  message("Procesando ciudad: ", ci)
  
  # --- Subconjunto retail
  retail_ci <- retail %>%
    filter(ciudad_retail == c_retail)
  
  # --- Subconjunto SIPSA
  wholesale_ci <- wholesale %>%
    filter(cod_mun_sipsa == c_sipsa)
  
  if (nrow(retail_ci) == 0 | nrow(wholesale_ci) == 0) {
    warning("Ciudad sin datos suficientes: ", ci)
    next
  }
  
  # --- Merge
  data_merged_ci <- wholesale_ci %>%
    left_join(mapa, by = "alimento_sipsa") %>%
    left_join(
      retail_ci,
      by = c("articulo_ipc", "Year" = "ano", "Month" = "mes_num")
    ) %>%
    mutate(ciudad = ci) %>%
    drop_na(precio_sipsa) %>%
    arrange(ciudad, alimento_sipsa, Year, Month)
  
  lista_ciudades[[ci]] <- data_merged_ci
}

# Unir todas las ciudades
data_merged <- bind_rows(lista_ciudades)

#------------------------------------------------------
# 4. Guardar salida final
#------------------------------------------------------

out_path  <- file.path(
  getwd(), "working-papers", "working-paper-q2", 
  "mapeo ipc-sipsa", "output",
  paste0(date_tag, "_dataset_ipc_sipsa.xlsx")
)

write_xlsx(data_merged, out_path)

message("OK. Archivo generado: ", out_path)
message("Filas generadas: ", nrow(data_merged))
message("Ciudades incluidas: ", paste(unique(data_merged$ciudad), collapse = ", "))
