# --------- #
# Librerías #
# --------- #
library(dplyr)
library(tidyverse)
library(readr)

# -------------------- #
# Subir bases de datos #
# -------------------- #

# Definir la ruta de los archivos 
ruta <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\"
output <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Lista de alimentos\\"

# Inicializar lista de bases de datos
bases <- list()

# Cargar bases de datos de 2018 a 2024
for (año in 2018:2024) {
  archivo <- paste0(ruta, año, ".rds")
  if (file.exists(archivo)) {
    bases[[as.character(año)]] <- tryCatch(
      readRDS(archivo),
      error = function(e) {
        print(paste("Error al cargar", archivo, ":", e$message))
        return(NULL)
      }
    )
  } else {
    print(paste("Archivo no encontrado:", archivo))
    bases[[as.character(año)]] <- NULL
  }
}

# Crear una lista vacía para almacenar resultados del percentil 5
resultados_p5 <- data.frame()

# ----------------------------------------------- #
# Definir función para procesar un mes específico #
# ----------------------------------------------- #
percentile5 <- function(df, año, mes) {
  if (is.null(df)) {
    print(paste("No hay datos para", año, mes))
    return()
  }
  
  # Convertir a fecha
  df <- df %>%
    mutate(Fecha = as.Date(Fecha))
  
  # Filtrar por año y mes
  df <- df %>%
    filter(format(Fecha, "%Y") == as.character(año) & format(Fecha, "%m") == sprintf("%02d", mes))
  
  # Seleccionar columnas relevantes
  columnas_seleccion <- if ("Cod_CPC" %in% colnames(df)) {
    c("Ciudad", "Grupo", "Cod_CPC", "Alimento", "Cantidad_KG")
  } else {
    c("Ciudad", "Grupo", "Alimento", "Cantidad_KG")
  }
  
  # Agregar por Ciudad, Grupo y Alimento
  df_agregado <- df %>%
    select(all_of(columnas_seleccion)) %>%
    group_by(across(-Cantidad_KG)) %>%
    summarise(Cantidad_KG = sum(Cantidad_KG, na.rm = TRUE), .groups = "drop")
  
  # Calcular percentil 5 por grupo
  df_agregado <- df_agregado %>%
    group_by(Grupo) %>%
    mutate(p5 = quantile(Cantidad_KG, 0.05, na.rm = TRUE)) %>%
    ungroup()
  
  # Filtrar por percentil 5
  df_filtrado <- df_agregado %>%
    filter(Cantidad_KG >= p5) %>%
    mutate(Año = año, Mes = mes)
  
  # Guardar los resultados en la lista global
  resultados_p5 <<- bind_rows(resultados_p5, df_filtrado)
  
  # Mensaje de confirmación
  print(paste("Done:", año, "_", mes))
}

# ---------------------------------------- #
# Bucle para iterar sobre los años y meses #
# ---------------------------------------- #
for (año in 2018:2024) {
  resultados_año <- data.frame()  
  
  for (mes in 1:12) {
    if (año == 2024 && mes > 8) break
    percentile5(bases[[as.character(año)]], año, mes)
  }
  
  # Guardar resultados del año si hay datos
  if (nrow(resultados_p5) > 0) {
    resultados_año <- resultados_p5 %>% filter(Año == año)  
    saveRDS(resultados_año, file = paste0(output, "lista_alimentos_p5_", año, ".rds"))
  }
}

# Guardar el archivo consolidado de todos los años
saveRDS(resultados_p5, file = paste0(output, "lista_alimentos_p5_total.rds"))

