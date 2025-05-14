#############
# Librerías #
#############
library(tidyverse)
library(haven)
library(readr)

#########################################
# Directorio de trabajo
#########################################
setwd("C:\\Users\\danie\\OneDrive\\Documentos\\Datos ENPH\\")

#########################################
# Función para cargar bases             #
#########################################

cargar_base <- function(path, variables) {
  archivos <- list.files(path = path, pattern = "\\.(csv|dta|sav)$", full.names = TRUE)
  
  if (length(archivos) == 0) {
    warning(paste0("No se encontró ningún archivo en: ", path))
    return(NULL)
  }
  
  cat("Archivos encontrados en", path, ":\n")
  print(archivos)
  
  lista_bases <- lapply(archivos, function(ruta) {
    df <- if (grepl("\\.csv$", ruta)) {
      tryCatch({
        read_delim(ruta, delim = ";", show_col_types = FALSE)
      }, error = function(e) {
        read_delim(ruta, delim = ",", show_col_types = FALSE)
      })
    } else if (grepl("\\.dta$", ruta)) {
      read_dta(ruta)
    } else if (grepl("\\.sav$", ruta)) {
      read_sav(ruta)
    }
    
    df <- df %>% mutate(across(everything(), as.character))
    
    if (all(c("DIRECTORIO", "SECUENCIA_P") %in% colnames(df))) {
      df <- df %>% mutate(id_hogar = paste0(DIRECTORIO, "-", SECUENCIA_P))
    }
    
    return(df)
  })
  
  base <- bind_rows(lista_bases)
  
  if (!"id_hogar" %in% colnames(base) && all(c("DIRECTORIO", "SECUENCIA_P") %in% colnames(base))) {
    base <- base %>% mutate(id_hogar = paste0(DIRECTORIO, "-", SECUENCIA_P))
  }
  
  vars_finales <- c("id_hogar", variables)
  base <- base[, intersect(vars_finales, colnames(base)), drop = FALSE]
  
  base <- base %>% distinct(id_hogar, .keep_all = TRUE)
  
  return(base)
}

#########################################
# Cargar todas las bases                #
#########################################

caracteristicas <- cargar_base(
  "Caracteristicas generales personas",
  c("P6500", "P7500S2A1", "P7510S1A1", "P7510S2A1", "P7513S9A1")
)

hogares <- cargar_base(
  "Vivienda y hogares",
  c("IT", "ICGU", "ICMUG", "ICMDUG", "GTUG", "GCUG", "GCMUG")
)

gastos_comidas_hogar <- cargar_base(
  "Gastos diarios del hogar Urbano - Comidas preparadas fuera del hogar",
  c("NH_CGDUCFH_P1", "NH_CGDUCFH_P1_1", "NH_CGDUCFH_P1_2", "NH_CGDUCFH_P2", "NH_CGDUCFH_P3",
    "NH_CGDUCFH_P4", "NH_CGDUCFH_P5", "NH_CGDUCFH_P6", "NH_CGDUCFH_P7", "NH_CGDUCFH_P8")
)

gastos_personales_comidas <- cargar_base(
  "Gastos personales Urbano - Comidas preparadas fuera del hogar",
  c("NH_CGPUCFH_P1", "NH_CGPUCFH_P1_S1", "NH_CGPUCFH_P1_S2", "NH_CGPUCFH_P2", "NH_CGPUCFH_P3",
    "NH_CGPUCFH_P4", "NH_CGPUCFH_P5", "NH_CGPUCFH_P6")
)

gastos_articulos <- cargar_base(
  "Gastos menos frecuentes - Articulos",
  c("P10270", "FORMA", "VALOR", "P10270S2", "P10270S3")
)

gastos_diarios_c <- cargar_base(
  "Gastos diarios Urbano - Capitulo C",
  c("NC2_CC_P1", "NC2_CC_P2", "NC2_CC_P3_S1", "NC2_CC_P3_S2")
)

gastos_mercados <- cargar_base(
  "Gastos diarios Urbanos - Mercados",
  c("NC2_CC_P4", "NC2_CC_P4S1")
)

gastos_medios_pagos <- cargar_base(
  "Gastos menos frecuentes - Medio de pago",
  c("P10272S1A1", "P10272S2A1", "P10272S3A1","P10272S4A1"," P10272S5A1" ,"P10272S6A1", " P10272S7A1",
    "P10272S8A1", "P10272S9A1", "P3F11S1")
)

#############################
# Unión final por id_hogar  #
#############################

# Lista de bases
lista_bases <- list(
  caracteristicas,
  hogares,
  gastos_comidas_hogar,
  gastos_personales_comidas,
  gastos_articulos,
  gastos_diarios_c,
  gastos_mercados,
  gastos_medios_pagos
)

# Función para unir sin duplicados
base_final <- reduce(lista_bases, function(x, y) {
  merge(x, y, by = "id_hogar", all = TRUE)
})

###############
# Base final #
##############
saveRDS(base_final, "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/ENPH/Base ENPH/Base de datos - ENPH/Base_ENPH_Urbano_Final.rds")



