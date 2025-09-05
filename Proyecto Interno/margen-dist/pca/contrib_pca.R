########################################################
## Prueba: Contribuciones del PCA                     ##
## Fecha: 22 de junio de 2025                         ##
########################################################

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# Cargar librerías
library(tidyverse)
library(seasonal)
library(zoo)
library(factoextra)
library(writexl)
library(forecast)  # Para métodos alternativos de desestacionalización

# Cargar códigos
source("margen-dist\\pca\\input_pca.R")

# Subclases
subvector <- output_final$cod_subclase %>% as.factor() %>% levels()

# Listas para almacenar resultados
resumen_data <- list()
detalle_data <- list()

# Función mejorada para desestacionalización con múltiples métodos
desestacionalizar_serie <- function(serie_ts) {
  # Intentar con X-13 (método por defecto)
  tryCatch({
    ajuste_seas <- seas(serie_ts)
    return(list(serie = final(ajuste_seas), metodo = "X-13"))
  }, error = function(e) {
    message("X-13 falló. Intentando con STL...")
  })
  
  # Intentar con STL (método robusto)
  tryCatch({
    # Asegurar que hay suficientes datos para STL
    if (length(serie_ts) >= 24) {  # Mínimo 2 años de datos
      ajuste_stl <- stl(serie_ts, s.window = "periodic", robust = TRUE)
      return(list(serie = seasadj(ajuste_stl), metodo = "STL"))
    } else {
      message("Serie demasiado corta para STL. Intentando con diferencias estacionales...")
    }
  }, error = function(e) {
    message("STL falló. Intentando con diferencias estacionales...")
  })
  
  # Intentar con diferencias estacionales
  tryCatch({
    serie_diff <- diff(serie_ts, lag = frequency(serie_ts))
    # Reconstruir la serie desestacionalizada
    serie_desest <- serie_ts
    serie_desest[(frequency(serie_ts)+1):length(serie_ts)] <- serie_ts[(frequency(serie_ts)+1):length(serie_ts)] - serie_diff
    return(list(serie = serie_desest, metodo = "Diferencias estacionales"))
  }, error = function(e) {
    message("Diferencias estacionales fallaron. Intentando con suavizado estacional...")
  })
  
  # Intentar con suavizado estacional (media móvil)
  tryCatch({
    # Calcular media móvil centrada
    ma <- stats::filter(serie_ts, filter = rep(1/frequency(serie_ts), frequency(serie_ts)), sides = 2)
    # Ajustar por valores NA en los extremos
    ma[1:(frequency(serie_ts)%/%2)] <- mean(serie_ts[1:frequency(serie_ts)], na.rm = TRUE)
    ma[(length(ma)-(frequency(serie_ts)%/%2)+1):length(ma)] <- mean(serie_ts[(length(serie_ts)-frequency(serie_ts)+1):length(serie_ts)], na.rm = TRUE)
    # Desestacionalizar
    serie_desest <- serie_ts - ma + mean(serie_ts, na.rm = TRUE)
    return(list(serie = serie_desest, metodo = "Media móvil"))
  }, error = function(e) {
    message("Todos los métodos de desestacionalización fallaron. Usando serie original.")
    return(list(serie = serie_ts, metodo = "Original (fallo todos los métodos)"))
  })
}

# Iterar sobre todas las subclases
for (k in 1:length(subvector)) {
  print(paste0("Procesando ", subvector[k]))
  
  # Df con subclase k
  df.aux <- output_final %>% filter(cod_subclase == subvector[k])
  
  # Verificar si tiene más de dos alimentos
  if (length(unique(df.aux$articulo)) >= 2) {
    
    seriesk <- list()
    metodos_utilizados <- list()
    
    for (j in unique(df.aux$articulo)) {
      print(paste0("Procesando ", j, " en ", subvector[k]))
      
      # Desestacionalizar cada serie
      serie_j <- df.aux %>% filter(articulo == j)
      
      # Calcular año y mes de inicio y fin
      start_j <- c(year(min(serie_j$fecha)), month(min(serie_j$fecha)))
      end_j <- c(year(max(serie_j$fecha)), month(max(serie_j$fecha)))
      
      # Crear serie de tiempo
      serie_j_ts <- ts(serie_j$precio_500g, start = start_j,
                       end = end_j, frequency = 12)
      
      # Desestacionalizar la serie con múltiples métodos
      resultado_desest <- desestacionalizar_serie(serie_j_ts)
      seriesk[[j]] <- resultado_desest$serie
      metodos_utilizados[[j]] <- resultado_desest$metodo
    }
    
    # Registrar métodos utilizados
    print(paste("Métodos de desestacionalización utilizados para", subvector[k], ":"))
    print(metodos_utilizados)
    
    # Dataframe con las series
    series_zoo <- lapply(seriesk, function(s) as.zoo(s))
    z_all <- do.call(merge.zoo, series_zoo)
    df_wide <- data.frame(
      fecha = as.Date(as.yearmon(index(z_all))),
      coredata(z_all)
    )
    
    colnames(df_wide) <- c("fecha", names(seriesk))
    
    # Verificar calidad de datos
    na_ratio <- mean(is.na(df_wide[,-1])) # Excluir columna de fecha
    
    if(na_ratio < 0.01 & nrow(df_wide) > 1){
      
      # Aplicar componentes principales
      input_pca <- df_wide[, -1]
      pca.aux <- prcomp(input_pca, scale = TRUE)
      pca.summary <- summary(pca.aux)
      
      # Importancia de los componentes principales
      importancia <- data.frame(
        Subclase = subvector[k],
        Componente = c("PC1", "PC2"),
        Varianza.Explicada = c(pca.summary$importance[2, 1] * 100, 
                               pca.summary$importance[2, 2] * 100),
        Varianza.Acumulada = c(pca.summary$importance[3, 1] * 100, 
                               pca.summary$importance[3, 2] * 100)
      )
      
      # Contribución de cada variable al PC1 y PC2
      var.aux <- get_pca_var(pca.aux)
      contribuciones <- data.frame(
        Subclase = subvector[k],
        Producto = rownames(var.aux$contrib),
        Contribucion.PC1 = var.aux$contrib[, 1],
        Contribucion.PC2 = var.aux$contrib[, 2]
      )
      
    } else {
      # Si hay muchos NA o pocos datos, llenar con NA
      message(paste("Subclase", subvector[k], "tiene muchos NA o pocos datos. Omitiendo PCA."))
      
      importancia <- data.frame(
        Subclase = subvector[k],
        Componente = c("PC1", "PC2"),
        Varianza.Explicada = NA,
        Varianza.Acumulada = NA
      )
      
      # Para las contribuciones, usar los nombres de los productos
      contribuciones <- data.frame(
        Subclase = subvector[k],
        Producto = names(seriesk),
        Contribucion.PC1 = NA,
        Contribucion.PC2 = NA
      )
    }
    
    # Almacenar resultados
    resumen_data[[k]] <- importancia
    detalle_data[[k]] <- contribuciones
    
  } else {
    # Si hay menos de 2 alimentos, también llenamos con NA
    print(paste0("Subclase ", subvector[k], " tiene menos de 2 alimentos. Omitiendo PCA."))
    
    importancia <- data.frame(
      Subclase = subvector[k],
      Componente = c("PC1", "PC2"),
      Varianza.Explicada = NA,
      Varianza.Acumulada = NA
    )
    
    # Para las contribuciones
    productos <- if (nrow(df.aux) > 0) unique(df.aux$articulo) else "Ninguno"
    contribuciones <- data.frame(
      Subclase = subvector[k],
      Producto = productos,
      Contribucion.PC1 = NA,
      Contribucion.PC2 = NA
    )
    
    # Almacenar resultados
    resumen_data[[k]] <- importancia
    detalle_data[[k]] <- contribuciones
  }
}

# Combinar todos los resultados
resumen_final <- bind_rows(resumen_data)
detalle_final <- bind_rows(detalle_data)

# Crear lista de hojas para exportar
hojas_excel <- list(
  Resumen = resumen_final,
  Detalle_Contribuciones = detalle_final
)

# Guardar en Excel
write_xlsx(hojas_excel, "margen-dist/pca/resultados_pca.xlsx")

