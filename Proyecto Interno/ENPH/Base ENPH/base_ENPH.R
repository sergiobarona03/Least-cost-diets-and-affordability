###############################################
# ENPH 2016-2017 (Zona Urbana)
# Cálculo de ponderaciones del rubro Alimentos
# y ponderaciones por artículo de alimentos
# Dani / actualizado: 2025-07-22
###############################################

#############
# Librerías #
#############
library(tidyverse)   
library(haven)       
library(readr)       
library(labelled)    

#########################################
# Parámetros de sesión                  #
#########################################

base_path <- "C:/Users/danie/OneDrive/Documentos/Datos ENPH/" 
setwd(base_path)

#########################################
# Utilidades generales                  #
#########################################

# Conversión a numeric
as_numeric_safe <- function(x){
  if (is.numeric(x)) return(x)
  x <- gsub(",", ".", x, fixed = FALSE) 
  x <- gsub("[[:space:]]", "", x)
  out <- suppressWarnings(as.numeric(x))
  out
}

# Conversión segura a character
as_character_safe <- function(x){
  if (is.character(x)) return(x)
  as.character(x)
}

# Tabla de mensualización (frecuencia -> factor mensual)
# NOTA: Tu versión original usaba strings; aquí admitimos num o char.
# Códigos (según doc ENPH):
# 1 Diario, 2 Varias veces por semana, 3 Semanal, 4 Quincenal,
# 5 Mensual, 6 Bimestral, 7 Trimestral, 9 Esporádica.
# Conversión económica: estimaciones equivalentes al gasto mensual promedio.
# - Diario / varias veces / semanal: multiplicador ~2.14 (≈ 30/14 ventana recordatoria)
# - Quincenal: 2.00 (dos quincenas/mes)
# - Mensual: 1.00
# - Bimestral: 0.50 (1 cada 2 meses)
# - Trimestral: 0.33 (1 cada 3 meses)
# - Esporádica: 1.00 (se imputa 1 ocurrencia/mes como aproximación conservadora)

factor_mensualizacion <- function(codigo){
  codigo_chr <- as.character(codigo)
  dplyr::case_when(
    codigo_chr %in% c("1","2","3") ~ 2.14,
    codigo_chr == "4" ~ 2.00,
    codigo_chr == "5" ~ 1.00,
    codigo_chr == "6" ~ 0.50,
    codigo_chr == "7" ~ 0.33,
    codigo_chr == "9" ~ 1.00,
    TRUE ~ NA_real_
  )
}

#########################################
# Función robusta para cargar bases     #
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
# Cargar bases principales              #
#########################################

hogares <- cargar_base(
  path = "Vivienda y hogares",
  variables = c("IT","ICGU","ICMUG","ICMDUG","GTUG","GCUG","GCMUG","FEX_C") 
)

capitulo_c <- cargar_base(
  path = "Gastos diarios Urbano - Capitulo C",
  variables = c("NC2_CC_P1","NC2_CC_P2","NC2_CC_P3_S1","FEX_C")
)

mercados <- cargar_base(
  path = "Gastos diarios Urbanos - Mercados",
  variables = c("NC2_CC_P4","NC2_CC_P4S1","FEX_C")
)

#########################################
# Procesamiento: Gasto mensual por hogar (rubro alimentos)
#########################################
# Igual a tu lógica original, con correcciones menores:
# - conversión segura numeric
# - NA control

capitulo_c_proc <- capitulo_c %>%
  mutate(
    valor = as_numeric_safe(NC2_CC_P3_S1),
    frec = factor_mensualizacion(NC2_CC_P2),
    gasto_mensual = valor * frec
  ) %>%
  filter(!is.na(gasto_mensual)) %>%
  group_by(id_hogar) %>%
  summarise(gasto_alimentos_capituloC = sum(gasto_mensual, na.rm = TRUE), .groups = "drop")

mercados_proc <- mercados %>%
  mutate(
    valor = as_numeric_safe(NC2_CC_P4S1)
  ) %>%
  filter(!is.na(valor)) %>%
  group_by(id_hogar) %>%
  summarise(gasto_alimentos_mercados = sum(valor, na.rm = TRUE), .groups = "drop")

# Unir ambos componentes de gasto en alimentos
gasto_alimentos_total <- full_join(capitulo_c_proc, mercados_proc, by = "id_hogar") %>%
  mutate(
    gasto_alimentos_mensual = rowSums(across(c(gasto_alimentos_capituloC, gasto_alimentos_mercados)), na.rm = TRUE)
  ) %>%
  select(id_hogar, gasto_alimentos_mensual)

#########################################
# Base hogares + proporción alimentos en gasto total hogar
#########################################

base_final_hogar <- hogares %>%
  left_join(gasto_alimentos_total, by = "id_hogar") %>%
  mutate(
    gasto_alimentos_mensual = replace_na(gasto_alimentos_mensual, 0),
    GTUG_num = as_numeric_safe(GTUG),
    proporcion_alimentos = ifelse(!is.na(GTUG_num) & GTUG_num > 0,
                                  round((gasto_alimentos_mensual / GTUG_num) * 100, 2),
                                  NA_real_)
  ) %>%
  select(id_hogar, gasto_alimentos_mensual, proporcion_alimentos, IT, ICGU, ICMUG, ICMDUG, GTUG, GCUG, GCMUG)

#########################################
# *** NUEVO *** Ponderaciones por artículo de alimentos
#########################################

if (!"FEX_C" %in% names(capitulo_c)){
  warning("capitulo_c no trae FEX_C; las ponderaciones serán sin expansión.")
}

ponderaciones_articulos <- capitulo_c %>%
  mutate(
    articulo = NC2_CC_P1,
    valor = as_numeric_safe(NC2_CC_P3_S1),
    frec = factor_mensualizacion(NC2_CC_P2),
    fex = as_numeric_safe(FEX_C),
    gasto_mensual = valor * frec,
    gasto_expandido = ifelse(is.na(fex), gasto_mensual, gasto_mensual * fex)
  ) %>%
  filter(!is.na(articulo), !is.na(gasto_mensual)) %>%
  group_by(articulo) %>%
  summarise(
    gasto_total_articulo = sum(gasto_expandido, na.rm = TRUE),
    gasto_total_articulo_sin_exp = sum(gasto_mensual, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  mutate(
    gasto_total_general = sum(gasto_total_articulo, na.rm = TRUE),
    ponderacion_pct = round((gasto_total_articulo / gasto_total_general) * 100, 4),
    gasto_total_general_sin_exp = sum(gasto_total_articulo_sin_exp, na.rm = TRUE),
    ponderacion_pct_sin_exp = round((gasto_total_articulo_sin_exp / gasto_total_general_sin_exp) * 100, 4)
  ) %>%
  arrange(desc(ponderacion_pct))

#########################################
# Resumen rápido en consola             #
#########################################

message("\n>>> TOP 20 artículos por participación expandida (%):")
print(ponderaciones_articulos %>% slice_max(order_by = ponderacion_pct, n = 20))

#########################################
# Guardar salidas en formato .RDS       #
#########################################

# 1. Resultados por hogar
saveRDS(base_final_hogar, file = "gasto_por_hogar.rds")

# 2. Ponderaciones por artículo
saveRDS(ponderaciones_articulos, file = "ponderaciones_articulos_alimentos.rds")


#########################################
# Quick check: porcentaje suma 100       #
#########################################

pct_suma <- sum(ponderaciones_articulos$ponderacion_pct, na.rm = TRUE)
message("Suma ponderaciones expandido (%): ", pct_suma)

pct_suma_sin <- sum(ponderaciones_articulos$ponderacion_pct_sin_exp, na.rm = TRUE)
message("Suma ponderaciones SIN expansión (%): ", pct_suma_sin)

#########################################
# Cruce con quintiles de ingreso (ejemplo opcional) #
#########################################
# Si quieres ver cómo cambia la canasta por nivel de ingreso:
#   1) Clasifica hogares en quintiles usando IT o ICGU.
#   2) Une gasto por artículo a nivel hogar y agrega ponderaciones dentro de cada quintil.
# Aquí dejo plantilla (comentada):

# if (all(c("IT","id_hogar") %in% names(hogares))){
#   hogares_quint <- hogares %>%
#     mutate(IT_num = as_numeric_safe(IT)) %>%
#     filter(!is.na(IT_num)) %>%
#     mutate(quintil_IT = ntile(IT_num, 5)) %>%
#     select(id_hogar, quintil_IT)
# 
#   capitulo_c_hogar <- capitulo_c %>%
#     mutate(valor = as_numeric_safe(NC2_CC_P3_S1),
#            frec = factor_mensualizacion(NC2_CC_P2),
#            gasto_mensual = valor * frec) %>%
#     filter(!is.na(gasto_mensual)) %>%
#     group_by(id_hogar, articulo = NC2_CC_P1) %>%
#     summarise(gasto_mensual_art_hogar = sum(gasto_mensual, na.rm = TRUE), .groups = "drop")
# 
#   capitulo_c_hogar <- capitulo_c_hogar %>% left_join(hogares_quint, by = "id_hogar")
# 
#   ponderaciones_quint <- capitulo_c_hogar %>%
#     group_by(quintil_IT, articulo) %>%
#     summarise(gasto = sum(gasto_mensual_art_hogar, na.rm = TRUE), .groups = "drop") %>%
#     group_by(quintil_IT) %>%
#     mutate(total_quint = sum(gasto, na.rm = TRUE),
#            pct_quint = round((gasto / total_quint) * 100, 4)) %>%
#     ungroup()
# }