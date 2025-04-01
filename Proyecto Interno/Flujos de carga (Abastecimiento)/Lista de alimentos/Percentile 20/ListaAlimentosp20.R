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

# Cargar bases de datos de 2018 a 2024 con verificación de existencia
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

# Crear una lista vacía para almacenar resultados del percentil 20
resultados_p20 <- data.frame()

# ----------------------------------------------- #
# Definir función para procesar un mes específico #
# ----------------------------------------------- #
percentile20 <- function(df, año, mes) {
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
  
  # Calcular percentil 15 por grupo
  df_agregado <- df_agregado %>%
    group_by(Grupo) %>%
    mutate(p20 = quantile(Cantidad_KG, 0.20, na.rm = TRUE)) %>%
    ungroup()
  
  # Filtrar por percentil 20
  df_filtrado <- df_agregado %>%
    filter(Cantidad_KG >= p20) %>%
    mutate(Año = año, Mes = mes)
  
  # Guardar los resultados en la lista global
  resultados_p20 <<- bind_rows(resultados_p20, df_filtrado)
  
  # Mensaje de confirmación
  print(paste("Done:", año, "_", mes))
}

# ---------------------------------------- #
# Bucle para iterar sobre los años y meses #
# ---------------------------------------- #
for (año in 2018:2024) {
  for (mes in 1:12) {
    # Parar en agosto de 2024
    if (año == 2024 && mes > 8) break
    
    # Llamar la función 
    percentile20(bases[[as.character(año)]], año, mes)
  }
  
  # Guardar resultados del año si hay datos
  if (nrow(resultados_p20) > 0) {
    resultados_año <- resultados_p20 %>% filter(Año == año)  
    saveRDS(resultados_año, file = paste0(output, "lista_alimentos_p20_", año, ".rds"))
  }
}

# Guardar el archivo consolidado de todos los años
saveRDS(resultados_p20, file = paste0(output, "lista_alimentos_p20_total.rds"))
install.packages("writexl")
library(writexl)
write_xlsx(resultados_p20, "C:\\Users\\danie\\OneDrive\\Escritorio\\lista_alimentos_p20.xlsx")


