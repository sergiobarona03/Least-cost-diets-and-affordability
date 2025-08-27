
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
source("margen-arima/join-ipc-sipsa.R")

######################################
###--------------------------------###
### Datos sobre precios minoristas ###
###--------------------------------###
######################################

# Filtro para Cali, Medellín y Bogotá
whole_tres = retail_whole_18 %>% filter(nombre_ciudad %in% c("MEDELLÍN",
                                                             "CALI",
                                                             "BOGOTÁ D.C.")) 

# Organizar la base de datos
data_min_may = whole_tres %>% select(cod_mun ,ciudad, nombre_ciudad,
                                     Year,
                                     mes, Month, Alimento, retail,
                                     codigo_articulo, precio_500g_sipsa,
                                     precio_500g_ipc) %>%
  rename(mes_num = Month,
         ano = Year,
         sipsa = Alimento,
         articulo = retail)

# Recodificar fecha
data_min_may$fecha = as.Date(paste(data_min_may$ano, 
                                   data_min_may$mes_num, 
                                "01", sep = "-"))

# Filtrar para la ciudad principal
ciudad.input = c("CALI")
data_min_may = data_min_may %>% filter(nombre_ciudad %in% ciudad.input)

# Condición de exclusión:
food.exclude = vector(mode = "list")
foods_no_exclude = levels(as.factor(as.character(data_min_may$sipsa)))

# Función para definir la exclusión
tiene_48 <- function(serie_mat) {
  
  serie_vec <- as.vector(t(serie_mat))
  no_na <- !is.na(serie_vec)
  max_consecutivos <- rle(no_na)$lengths[which(rle(no_na)$values)]
  any(max_consecutivos >= 36)
  
}

for (k in 1:length(foods_no_exclude)) {
  data_k = data_min_may %>% filter(sipsa == foods_no_exclude[[k]])
  
  start1 = min(data_k$fecha)
  end1 = max(data_k$fecha)
  
  series_sipsa = ts(data_k$precio_500g_sipsa, 
                    start = c(substr(start1, 1, 4), substr(start1, 6, 7)), 
                    end = c(substr(end1, 1, 4), substr(end1, 6, 7))
                    , frequency = 12)
  
  series_ipc = ts(data_k$precio_500g_ipc, 
                  start = c(substr(start1, 1, 4), substr(start1, 6, 7)), 
                  end = c(substr(end1, 1, 4), substr(end1, 6, 7))
                  , frequency = 12)
  
  if(!(tiene_48(series_sipsa) & tiene_48(series_ipc))){
    food.exclude = append(food.exclude, unique(data_k$sipsa))
  }
  
}

# Excluimos los alimentos que no cumplen la condición
data_min_may = data_min_may %>% filter(!sipsa %in% unlist(food.exclude))

# Crear la variable "código subclase"
data_min_may$cod_subclase = paste0("0",substr(data_min_may$codigo_articulo, 1, 6), "0")

# Calcular los cuartiles por subclase
margen_articulo <- data_min_may %>% 
  group_by(articulo) %>%
  mutate(
    factor = precio_500g_ipc / precio_500g_sipsa,
    margen = (factor - 1) * 100
  ) %>% filter(margen > 0)

# Guardar el margen por artículo
readr::write_csv(margen_articulo,
                 "margen-arima/output/CALI/260825_margen_productos.csv")






