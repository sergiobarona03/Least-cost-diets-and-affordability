#############
# Librerías #
#############
library(tidyverse)
library(dplyr)
library(readr)

################
# Cargar Base #
###############
base_final <- readRDS("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/ENPH/Base ENPH/Base de datos - ENPH/Base_ENPH_Urbano_Final.rds")

##############################
# DICCIONARIO DE VARIABLES POR RUBRO
##############################

rubros <- list(
  gasto_01_alimentos = c("NC2_CC_P3_S1", "NC2_CC_P4S1", "NH_CGDUCFH_P5", "NH_CGPUCFH_P5"),
  gasto_04_vivienda_servicios = c("P10272S1A1", "P10272S2A1", "P10272S3A1", "P10272S4A1", "P10272S5A1", "P10272S6A1", "P10272S7A1"),
  gasto_06_salud = c("P3F11S1"),
  gasto_08_comunicaciones = c("P10272S8A1", "P10272S9A1"),
  gasto_09_recreacion = c("P7510S1A1", "P7510S2A1"),
  gasto_10_educacion = c("P7500S2A1", "P7513S9A1"),
  gasto_11_restaurantes = c("NH_CGDUCFH_P5", "NH_CGPUCFH_P5")
)

########################################
# SUMAR GASTOS POR RUBRO POR HOGAR
########################################

# Convertimos a numérico las variables
base_final <- base_final %>% 
  mutate(across(where(is.character), ~as.numeric(.), .names = "num_{.col}"))

# Revisar las variables y unir los rubros
gastos_rubro <- map_dfc(names(rubros), function(rubro) {
  vars <- paste0("num_", rubros[[rubro]])
  vars_existentes <- vars[vars %in% names(base_final)]
  if (length(vars_existentes) == 0) return(tibble(!!rubro := 0))
  base_final %>%
    transmute(!!rubro := rowSums(across(all_of(vars_existentes)), na.rm = TRUE))
})

# Agregar id_hogar y GTUG original
gastos_rubro <- bind_cols(
  id_hogar = base_final$id_hogar,
  GTUG = as.numeric(base_final$GTUG),
  IT = as.numeric(base_final$IT),	
  ICGU = as.numeric(base_final$ICGU),		
  ICMUG	= as.numeric(base_final$ICMUG),	
  ICMDUG = as.numeric(base_final$ICMDUG),	
  GCUG = as.numeric(base_final$GCUG),		
  GCMUG = as.numeric(base_final$GCMUG),
  gastos_rubro
)

# Calcular proporción de gasto en alimentos
gastos_rubro <- gastos_rubro %>%
  mutate(prop_gasto_alimentos = if_else(GTUG > 0, (gasto_01_alimentos / GTUG) * 100, NA_real_))


###################
# Exportar tabla #
##################
saveRDS(gastos_rubro, "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/ENPH/Base ENPH/Rubros/Gastos_por_Rubro_ENPH.rds")
