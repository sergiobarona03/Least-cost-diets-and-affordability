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
    } 
    if (is.null(df) & (grepl("\\.dta$", ruta))) {
      df = read_dta(ruta)
    } 
    if (is.null(df) & grepl("\\.sav$", ruta)) {
      df = read_sav(ruta)
    }
    
    df <- df %>% mutate(across(everything(), as.character))
    
    if (all(c("DIRECTORIO", "SECUENCIA_P") %in% colnames(df))) {
      df <- df %>% mutate(id_hogar = paste0(DIRECTORIO, "-", SECUENCIA_P))
    }
    
    return(df)
  })
  
  base <- as.data.frame(lista_bases)
  
  if (!"id_hogar" %in% colnames(base) && all(c("DIRECTORIO", "SECUENCIA_P") %in% colnames(base))) {
    base <- base %>% mutate(id_hogar = paste0(DIRECTORIO, "-", SECUENCIA_P))
  }
  
  base <- base %>% select(c("id_hogar", variables))
  
  return(base)
}

#########################################
# Cargar todas las bases                #
#########################################

# Tabla de mensualización
factor_mensualizacion <- function(codigo) {
  case_when(
    codigo == "1" ~ 2.14,
    codigo == "2" ~ 2.14,
    codigo == "3" ~ 2.14,
    codigo == "4" ~ 2.00,
    codigo == "5" ~ 1.00,
    codigo == "6" ~ 0.50,
    codigo == "7" ~ 0.33,
    codigo == "9" ~ 1.00,
    TRUE ~ NA_real_
  )
}

# Cargar bases
hogares <- cargar_base(
  "Vivienda y hogares",
  c("IT", "ICGU", "ICMUG", "ICMDUG", "GTUG", "GCUG", "GCMUG")
)

capitulo_c <- cargar_base(
  "Gastos diarios Urbano - Capitulo C",
  c("NC2_CC_P1", "NC2_CC_P2", "NC2_CC_P3_S1")
)

mercados <- cargar_base(
  "Gastos diarios Urbanos - Mercados",
  c("NC2_CC_P4", "NC2_CC_P4S1")
)

# Procesamiento capítulo C
capitulo_c_proc <- capitulo_c %>%
  mutate(
    valor = as.numeric(NC2_CC_P3_S1),
    frec = factor_mensualizacion(NC2_CC_P2),
    gasto_mensual = valor * frec
  ) %>%
  filter(!is.na(gasto_mensual)) %>%
  group_by(id_hogar) %>%
  summarise(gasto_alimentos_capituloC = sum(gasto_mensual, na.rm = TRUE), .groups = "drop")

# Procesamiento mercados
mercados_proc <- mercados %>%
  mutate(valor = as.numeric(NC2_CC_P4S1)) %>%
  filter(!is.na(valor)) %>%
  group_by(id_hogar) %>%
  summarise(gasto_alimentos_mercados = sum(valor, na.rm = TRUE), .groups = "drop")

# Unir gastos
gasto_alimentos_total <- full_join(capitulo_c_proc, mercados_proc, by = "id_hogar") %>%
  mutate(
    gasto_alimentos_mensual = rowSums(across(c(gasto_alimentos_capituloC, gasto_alimentos_mercados)), na.rm = TRUE)
  ) %>%
  select(id_hogar, gasto_alimentos_mensual)

# Unir con base de hogares
base_final <- left_join(hogares, gasto_alimentos_total, by = "id_hogar") %>%
  mutate(
    gasto_alimentos_mensual = replace_na(gasto_alimentos_mensual, 0),
    proporcion_alimentos = ifelse(
      !is.na(as.numeric(GTUG)) & as.numeric(GTUG) > 0,
      round((gasto_alimentos_mensual / as.numeric(GTUG)) * 100, 2), 
    )
  ) %>%
  select(
    id_hogar,
    gasto_alimentos_mensual,
    proporcion_alimentos,
    IT, ICGU, ICMUG, ICMDUG, GTUG, GCUG, GCMUG
  )

# Guardar base
saveRDS(base_final, "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/ENPH/Base ENPH/Base_ENPH_Alimentos.rds")



