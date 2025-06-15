
##----------------------------------------------##
## Definir función para limpieza de datos       ##
##----------------------------------------------##

# Cargar librerías
library(tidyverse)
library(janitor)

# Definir directorio de trabajo
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE")

# Rango de la función 1990:1995
year <- 1995

archivos <- list.files(as.character(year))
  
##------------------------------------------------##
## Bucle para limpiar los archivos individuales   ##
##------------------------------------------------##
  
# Crear lista vacía
output_list = vector(mode = "list", length = length(archivos))
  
# Simular bucle
k = 1
cat(strrep("-", 50), "\n")
cat("DONE: k =", k, "en", archivos[k], "\n")
cat(strrep("-", 50), "\n\n")
    
df <- suppressMessages(
      suppressWarnings(
        readxl::read_excel(paste0(year, "/", archivos[k]))
      )
    )
    
df <- df[,1:(ncol(df) - 1)]

# Definir meses
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
           "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# Buscar el nombre del artículo
producto_match <- which(
  apply(df, c(1, 2), function(x) grepl(paste0("\\b", year, "\\b"), x)),
  arr.ind = TRUE
) 

if (is.na(producto_match[1])) {
  producto_match <- which(grepl(paste0("s*", year),
                                colnames(df),
                                ignore.case = TRUE))
  articulo = colnames(df)[producto_match]
  articulo <- sub("\\s*(AÑO\\s*)?\\d{4}$", "", articulo, ignore.case = TRUE)
} else {
  articulo = df[producto_match[1,1], producto_match[1,2]]
  articulo <- sub("\\s*(AÑO\\s*)?\\d{4}$", "", articulo, ignore.case = TRUE)
}

# Buscar la unidad
unidad_match <- which(
  apply(df, c(1,2), function(x) grepl("^\\$", x)),
  arr.ind = TRUE
)

if (is.na(unidad_match[1])) {
  unidad_match <- which(grepl("^\\$", colnames(df)))
  unidad = colnames(df)[unidad_match]
  unidad <- str_extract(unidad, "\\d+\\s*[^\\d]+")
} else {
  # Asignar a dos objetos: nombre del producto y unidad
  unidad = df[unidad_match[1], unidad_match[2]]
  unidad <- str_extract(unidad, "\\d+\\s*[^\\d]+")
}


# Buscar la fila que contiene "Enero"
fila_header <- which(apply(df, 1, function(x) any((x == "Enero") | 
                                                    (x == "enero") |
                                                    (x == "ENERO"))))[1]

# Asignar esa fila como nombres de columna (si existe)
if (!is.na(fila_header)) {
  colnames(df) <- df[fila_header, ]
  df <- df[-(1:fila_header), ]  # Eliminar filas anteriores (incluyendo la de encabezados)
}

# Elimina filas completamente vacías (todas sus columnas son NA)
df <- df[!apply(is.na(df), 1, all), ]

# Eliminar columnas completamente vacias
df <- df[, colMeans(is.na(df)) < 1]

# Eliminar las filas y columnas con más de 60% de NAs
df <- df[rowMeans(is.na(df)) <= 0.6,  colMeans(is.na(df)) <= 0.6]

# Identificar filas inútiles
df_char <- df %>% mutate(across(everything(), as.character))
filas_inutiles <- apply(df_char, 1, function(fila) {
  all(is.na(fila) | fila == "=" | fila == "|" | str_trim(fila) == "")
})

# Eliminar filas inútiles
df <- df[!filas_inutiles, ]

# Convertir el nombre de las filas
df <- df %>% clean_names()

# Seleccionar las columnas
colnames(df) <- substr(gsub("\\s+", "", gsub("_", "", colnames(df))), 1, 3)
colnames(df)[which(colnames(df) == "aep")] = "sep"

df <- df %>% select(c("ciu", substr(meses, 1, 3)))
colnames(df) <- c("ciudades", meses)

# Omitir NAs
df <- df %>% filter(if_any(meses, ~ !is.na(.)))
