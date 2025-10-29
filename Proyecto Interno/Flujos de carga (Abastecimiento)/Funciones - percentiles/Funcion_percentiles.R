##########################
# Función Abastecimiento #
##########################

# --------- #
# Librerías #
# --------- #
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# ------ #
# Rutas  #
# ------ #
ruta   <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\"
output <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Lista de alimentos\\"

# --------------------------- #
# Cargar bases 2018 a 2024    #
# --------------------------- #
bases <- list()
for (año in 2018:2024) {
  archivo <- paste0(ruta, año, ".rds")
  if (file.exists(archivo)) {
    bases[[as.character(año)]] <- tryCatch(
      readRDS(archivo),
      error = function(e) {
        message(paste("Error al cargar", archivo, ":", e$message))
        return(NULL)
      }
    )
  } else {
    message(paste("Archivo no encontrado:", archivo))
    bases[[as.character(año)]] <- NULL
  }
}

# ------------------------------------------------------- #
# Función: procesa un (año, mes) y percentiles 0:99       #
# - Filtra por año/mes                                    #
# - Agrega por Ciudad/Grupo/(Cod_CPC opcional)/Alimento   #
# - Calcula umbrales por Grupo para todos los percentiles #
# - Devuelve filas con Cantidad_KG >= umbral              #
# ------------------------------------------------------- #
percentiles_range <- function(df, año, mes, percentiles = 0:99) {
  if (is.null(df) || nrow(df) == 0) {
    message(sprintf("No hay datos para %s-%02d", año, mes))
    return(dplyr::tibble())
  }
  
  # Asegurar columna Fecha como Date
  df <- df %>%
    mutate(Fecha = as.Date(Fecha))
  
  # Filtrar por año y mes
  df <- df %>%
    filter(format(Fecha, "%Y") == as.character(año),
           format(Fecha, "%m") == sprintf("%02d", mes))
  
  if (nrow(df) == 0) {
    message(sprintf("Sin registros en %s-%02d", año, mes))
    return(dplyr::tibble())
  }
  
  # Columnas dinámicas (Cod_CPC)
  columnas_seleccion <- if ("Cod_CPC" %in% colnames(df)) {
    c("Ciudad", "Grupo", "Cod_CPC", "Alimento", "Cantidad_KG")
  } else {
    c("Ciudad", "Grupo", "Alimento", "Cantidad_KG")
  }
  
  # Agregar por claves (Cantidad_KG)
  df_agregado <- df %>%
    select(any_of(columnas_seleccion)) %>%
    group_by(across(-Cantidad_KG)) %>%
    summarise(Cantidad_KG = sum(Cantidad_KG, na.rm = TRUE), .groups = "drop")
  
  if (nrow(df_agregado) == 0) {
    message(sprintf("Sin agregados en %s-%02d", año, mes))
    return(dplyr::tibble())
  }
  
  # Calcular umbrales para TODOS los percentiles por Grupo
  percentiles <- sort(unique(pmax(0, pmin(99, as.integer(percentiles))))) 
  umbrales_por_grupo <- df_agregado %>%
    group_by(Grupo) %>%
    reframe(
      percentil = percentiles,
      umbral    = quantile(Cantidad_KG, probs = percentiles/100, na.rm = TRUE, names = FALSE)
    )
  
  # Expandir por percentil y filtrar por umbral
  df_filtrado <- df_agregado %>%
    inner_join(umbrales_por_grupo, by = "Grupo") %>%
    filter(Cantidad_KG >= umbral) %>%
    mutate(Año = año, Mes = mes, .after = Cantidad_KG)
  
  message(sprintf("Listo: %s-%02d", año, mes))
  return(df_filtrado)
}

# ------------------------------------------------ #
# Bucle principal: años, meses y percentiles 0:99  #
# - Guarda por año y por percentil                 #
# - Consolida todo                                 #
# ------------------------------------------------ #
resultados_total <- dplyr::tibble()

for (año in 2018:2024) {
  for (mes in 1:12) {
    if (año == 2024 && mes > 8) break
    
    res_mes <- percentiles_range(
      df          = bases[[as.character(año)]],
      año         = año,
      mes         = mes,
      percentiles = 0:99
    )
    
    if (nrow(res_mes) > 0) {
      resultados_total <- bind_rows(resultados_total, res_mes)
    }
  }
  
  # Guardado por año 
  if (nrow(resultados_total) > 0) {
    res_anio <- resultados_total %>% filter(Año == año)
    if (nrow(res_anio) > 0) {
      saveRDS(res_anio, file = paste0(output, "lista_alimentos_p0_99_", año, ".rds"))
    }
  }
}

# Guardar consolidado total (todos los años/meses/percentiles)
if (nrow(resultados_total) > 0) {
  saveRDS(resultados_total, file = paste0(output, "lista_alimentos_p0_99_total.rds"))
}

# ---------------------------------------------------- #
# Guardar un archivo por percentil a nivel total       #
# Descomenta si quieres un archivo separado por cada P #
# ---------------------------------------------------- #
# if (nrow(resultados_total) > 0) {
#   split(resultados_total, resultados_total$percentil) %>%
#     imap(function(df_p, p) {
#       saveRDS(df_p, file = paste0(output, sprintf("lista_alimentos_p%02d_total.rds", as.integer(p))))
#     })
# }
