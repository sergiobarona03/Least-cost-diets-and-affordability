# Librerías
library(dplyr)
library(tidyverse)
library(readr)

# Definir la ruta de los archivos 
ruta <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Bases historicas\\"

# Cargar las bases de datos de 2018 a 2024
bases <- list(
  "2018" = readRDS(paste0(ruta, "2018.rds")),
  "2019" = readRDS(paste0(ruta, "2019.rds")),
  "2020" = readRDS(paste0(ruta, "2020.rds")),
  "2021" = readRDS(paste0(ruta, "2021.rds")),
  "2022" = readRDS(paste0(ruta, "2022.rds")),
  "2023" = readRDS(paste0(ruta, "2023.rds")),
  "2024" = readRDS(paste0(ruta, "2024.rds"))
)

# Crear listas vacías para almacenar los resultados por percentil
resultados_percentil <- list(
  p5  = data.frame(),
  p10 = data.frame(),
  p15 = data.frame(),
  p20 = data.frame(),
  p25 = data.frame()
)

# Bucle para procesar cada año y cada mes
for (año in 2018:2024) {
  for (mes in 1:12) {
    
    # Parar en agosto de 2024
    if (año == 2024 && mes > 8) break 
    
    # Filtrar la base de datos por año y mes
    df_mes <- bases[[as.character(año)]] %>%
      filter(Month == mes & Year == año)
    
    # Verificar si hay datos en ese mes
    if (nrow(df_mes) == 0) {
      message(paste("⚠️ No hay datos para", mes, "de", año, "- Saltando..."))
      next
    }
    
    # Seleccionar las columnas correctas (2024 tiene 'Cod_CPC')
    if ("Cod_CPC" %in% colnames(df_mes)) {
      columnas_seleccion <- c("Grupo", "Cod_CPC", "Alimento", "Cantidad_KG")
    } else {
      columnas_seleccion <- c("Grupo", "Alimento", "Cantidad_KG")
    }
    
    # Agrupar por grupo de alimentos y calcular percentiles
    df_mes <- df_mes %>%
      select(all_of(columnas_seleccion)) %>%
      group_by(Grupo) %>%
      mutate(
        p5  = quantile(Cantidad_KG, 0.05, na.rm = TRUE),
        p10 = quantile(Cantidad_KG, 0.10, na.rm = TRUE),
        p15 = quantile(Cantidad_KG, 0.15, na.rm = TRUE),
        p20 = quantile(Cantidad_KG, 0.20, na.rm = TRUE),
        p25 = quantile(Cantidad_KG, 0.25, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Filtrar alimentos por encima de cada percentil y agregarlos a la lista de alimentos
    for (p in c(5, 10, 15, 20, 25)) {
      df_filtrado <- df_mes %>%
        filter(Cantidad_KG >= get(paste0("p", p))) %>%
        mutate(Año = año, Mes = mes)
      
      resultados_percentil[[paste0("p", p)]] <- bind_rows(resultados_percentil[[paste0("p", p)]], df_filtrado)
    }
  }
}

# Guardar los archivos finales por percentil
output <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\Flujos de carga (Abastecimiento)\\Lista de alimentos\\"

for (p in c(5, 10, 15, 20, 25)) {
  write_csv(resultados_percentil[[paste0("p", p)]], paste0(output, "lista_alimentos_p", p, ".csv"))
}


