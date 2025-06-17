##----------------------------------------------##
## Definir función para limpieza de datos  (f1) ##
## Rango: 1999:2010 y 2011:2018                 ##
##----------------------------------------------##

# Cargar librerías
library(tidyverse)
library(janitor)

# Función para archivos de 2016 a 2018
f1 <- function(df, year) {
  
  # Limpiar nombres de columnas
  df <- df %>% clean_names()
  
  # Nombres de columnas de los meses
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
             "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  
  if (as.numeric(year) %in% c(1999:2010)) {
    df = df
  } else {
    # Corregir encabezados: usar la primera fila como nombres de columnas
    df[1, meses] <- as.list(meses)
    colnames(df) <- as.character(df[1, ])
    df <- df[-1, ] %>% clean_names()
  }

  # Seleccionar columnas necesarias
  colnames(df)[which(colnames(df) == "nombre_articulo")] = "articulo"
  colnames(df)[which(colnames(df) == "nombre_ciu")] = "nombre_ciudad"
  colnames(df)[which(colnames(df) == "codigo_art")] = "codigo_articulo"
  colnames(df)[which(colnames(df) == "nombre_art")] = "articulo"
  
  df <- df %>% select(ano, ciudad, nombre_ciudad, codigo_articulo, articulo, unidad, 
                      meses)
  
  # Mostrar filas con NA que serán eliminadas
  cat("Filas con NA eliminadas:\n")
  print(df %>% filter(if_all(meses, is.na)))
  
  # Eliminar NAs
  df <- df %>% filter(if_any(meses, ~ !is.na(.)))
  
  # Transformar a formato largo
  df_melt <- df %>%
    pivot_longer(
      cols = enero:diciembre,
      names_to = "mes",
      values_to = "precio"
    )
  
  return(df_melt)
}
