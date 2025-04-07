
#------------------------------------------------------------------------------------------#
#         PRIMERA FUNCIÓN: CARGA Y DEPURACIÓN DATOS DE COLOMBIA, DANE                      #
#-----------------------------------------------------------------------------------------#

DataCol3 <- function(Month, Year) {
  Month <- 3
  Year <- 2024
  #------------------------------------------------------------------------------------------#
  #         PRIMERA ETAPA: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                # ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#
  
  #Función para validar parámetros
  validar_parametros <- function(parametro, tipo, rango = NULL) {
    if (missing(parametro)) {
      stop("Parameter is missing", deparse(substitute(parametro)))
    }
    
    if (!is.null(tipo)) {
      tipo_funcion <- switch(tipo,
                             "numeric" = is.numeric,
                             "character" = is.character,
                             "list" = is.list,
                             "vector" = function(x) is.vector(x) || is.data.frame(x),
                             "default" = function(x) FALSE)
      
      if (!tipo_funcion(parametro)) {
        stop(paste(deparse(substitute(parametro)), " It must be of type ", tipo))
      }
    }
    
    if (!is.null(rango) && !is.infinite(rango[1]) && !is.infinite(rango[2])) {
      if (parametro < rango[1] || parametro > rango[2]) {
        stop(paste(deparse(substitute(parametro)), " It must be within the range ", rango[1], " - ", rango[2]))
      }
    }
  }
  
  #-----------------------Verificaciones de paraḿetros obligatorios
  
  # Verificación de Month
  validar_parametros(Month, "numeric", c(1, 12))
  # Verificación de Year
  validar_parametros(Year, "numeric", c(2018, 2025))
  
  #------------------------------------------------------------------------------------------#
  #                       SEGUNDA ETAPA: VALIDACIÓN DE LIBRERIAS                             # ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#
  
  Librerias_base = c("tidyverse","rio","janitor","stringdist","lpSolve","knitr", "writexl")  # Nombra las librerias necesarias
  
  if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
  pacman::p_load(char = Librerias_base,character.only = TRUE);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes
  
  
  # Instala paquetes individualmente si no se han cargado correctamente
  paquetes_faltantes <- Librerias_base[!(Librerias_base %in% pacman::p_loaded())]
  for (paquete in paquetes_faltantes) {
    if (!require(paquete, character.only = TRUE)) {
      install.packages(paquete)
      library(paquete, character.only = TRUE)
    }
  }
  
  
  #------------------------------------------------------------------------------------------#
  #                   TERCERA ETAPA: CARGA DE DATOS DESDE CARPETA (DANE)                      # ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#
  
  cargar_datos_precios <- function(año, carpeta, env) {
    archivo_excel <- if (año == 2024) {
      file.path(carpeta, "anex-SIPSA-SerieHistoricaMayorista-2024.xlsx")
    } else if (año == 2023) {
      file.path(carpeta, "anex-SIPSA-SerieHistoricaMayorista-Dic2023.xlsx")
    } else {
      file.path(carpeta, paste0("series-historicas-precios-mayoristas-", año, ".xlsx"))
    }
    
    nombre_data <- paste0("data_list_precios_", año, "_ev")
    
    if (!file.exists(archivo_excel)) {
      stop(paste("El archivo", archivo_excel, "no se encuentra en la carpeta especificada."))
    }
    
    if (!exists(nombre_data, envir = env)) {
      hojas <- readxl::excel_sheets(archivo_excel)
      
      # Excluir hojas no deseadas como 'Índice' o vacías
      hojas_validas <- hojas[!tolower(hojas) %in% c("índice", "indice", "")]
      
      datos <- purrr::map(hojas_validas, function(hoja) {
        df <- tryCatch({
          readxl::read_excel(archivo_excel, sheet = hoja, skip = 6)
        }, error = function(e) {
          warning(paste("No se pudo leer la hoja:", hoja, "-", e$message))
          return(NULL)
        })
        
        if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(NULL)
        
        names(df) <- tolower(gsub("\\s+", "_", names(df)))
        df <- dplyr::rename_with(df, ~ gsub("\\*", "", .x))
        
        # Extraer nombre del mes
        mes_detectado <- NA
        if (año == 2024 && hoja == "2024") {
          mes_detectado <- "01"  # Puedes ajustar esta lógica si hay más de un mes
        } else if (año %in% 2019:2022) {
          mes_detectado <- stringr::str_pad(match(substr(hoja, 1, 3), c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")), 2, pad = "0")
        } else if (año == 2023) {
          mes_detectado <- stringr::str_pad(match(hoja, c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")), 2, pad = "0")
        } else if (año == 2018) {
          mes_detectado <- "12"  # según imagen, diciembre 2018
        }
        
        df <- df %>%
          mutate(anio = año,
                 mes = mes_detectado)
        
        # Detectar y renombrar la columna correcta de precio
        col_precio <- if (año == 2018) {
          "precio"
        } else if (año %in% c(2019, 2020)) {
          "precio_por_kilogramo"
        } else {
          "precio_promedio_por_kilogramo"
        }
        
        if (col_precio %in% names(df)) {
          df <- df %>% rename(precio = !!sym(col_precio))
        } else {
          warning(paste("Columna de precio no encontrada en hoja:", hoja))
          return(NULL)
        }
        
        return(df)
      })
      
      # Filtrar las que no sean NULL
      datos <- purrr::compact(datos)
      
      # Unir todas las hojas válidas
      data_final <- bind_rows(datos)
      
      assign(nombre_data, data_final, envir = env)
    }
    
    return(get(nombre_data, envir = env))
  }
  
  # Define carpeta
  carpeta_precios <- "C:\\Users\\danie\\OneDrive\\Documentos\\Datos precios\\"
  
  # Define entorno
  data_list_precios_ev_nuevo <- new.env()
  
  if (!exists("Price_data_list")) {
    Price_data_list <- tryCatch({
      cargar_datos_precios(Year, carpeta_precios, data_list_precios_ev_nuevo)
    }, error = function(e) {
      message("Error al cargar los datos de precios: ", e$message)
      NULL
    })
  }
  
    
  
  #------------------------------------------------------------------------------------------#
  #                       CUARTA ETAPA: DEPURACIÓN DE LOS DATOS                              #  ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#
  
  
  #------------------ IDENTIFICACIÓN DE MES, FECHAS Y SEMESTRES ------------------------------
  
  
  Nombres_Meses = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  Mes_Num=Month
  Month=Nombres_Meses[Month]
  
  Semestres=c("I_Semestre","II_Semestre")
  
  January = seq(from = as.Date(paste(Year, "1", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "1", "31", sep = "-"), format = "%Y-%m-%d"), by = 1)
  February = seq(from = as.Date(paste(Year, "2", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "2", "28", sep = "-"), format = "%Y-%m-%d"), by = 1)
  March = seq(from = as.Date(paste(Year, "3", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "3", "31", sep = "-"), format = "%Y-%m-%d"), by = 1)
  April = seq(from = as.Date(paste(Year, "4", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "4", "30", sep = "-"), format = "%Y-%m-%d"), by = 1)
  May = seq(from = as.Date(paste(Year, "5", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "5", "31", sep = "-"), format = "%Y-%m-%d"), by = 1)
  June = seq(from = as.Date(paste(Year, "6", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "6", "30", sep = "-"), format = "%Y-%m-%d"), by = 1)
  July = seq(from = as.Date(paste(Year, "7", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "7", "31", sep = "-"), format = "%Y-%m-%d"), by = 1)
  August = seq(from = as.Date(paste(Year, "8", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "8", "31", sep = "-"), format = "%Y-%m-%d"), by = 1)
  September = seq(from = as.Date(paste(Year, "9", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "9", "30", sep = "-"), format = "%Y-%m-%d"), by = 1)
  October = seq(from = as.Date(paste(Year, "10", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "10", "31", sep = "-"), format = "%Y-%m-%d"), by = 1)
  November = seq(from = as.Date(paste(Year, "11", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "11", "30", sep = "-"), format = "%Y-%m-%d"), by = 1)
  December = seq(from = as.Date(paste(Year, "12", "1", sep = "-"), format = "%Y-%m-%d"), to = as.Date(paste(Year, "12", "30", sep = "-"), format = "%Y-%m-%d"), by = 1)
  
  
  Semestre_I = c("January", "February", "March", "April", "May", "June")
  Semestre_II = c("July", "August", "September", "October", "November", "December")
  
  Lista_Mes=list(January, February, March, April, May, June, July, August, September, October, November, December);names(Lista_Mes)=Nombres_Meses
  Lista_Semestres = list(Semestre_I, Semestre_II);names(Lista_Semestres)=c("I_Semestre","II_Semestre")
  Fecha=Lista_Mes[[Month]]
  
  # -------------------------------------------------
  
  # ------------DFECHAS
  
  
  
  convertir_fechas_vector <- function(fechas) { #Función para validar el tipo de formato de entrada
    formato_fecha <- ifelse(grepl("/", fechas[1]), "%d/%m/%y", "%d") # Comprueba el formato
    
    if (formato_fecha == "%d/%m/%y") {
      fechas_convertidas <- as.Date(fechas, format = "%d/%m/%y", na.rm = TRUE)
    } else {
      fechas_numericas <- as.numeric(fechas)
      fechas_convertidas <- as.Date(fechas_numericas, origin = "1899-12-30")
    }
    
    return(fechas_convertidas)
  }
  
  
  
  #------------------ IDENTIFICACIÓN DE MES EN PRECIOS SIPSA   ------------------------------
  
  #S---elecionando el año según la estructura de datos
  
  # Función para depurar y filtrar datos
  depurar_y_filtrar <- function(data, mes_num) {
    data <- data[rowSums(is.na(data)) / ncol(data) < 0.5, colSums(is.na(data)) / nrow(data) < 0.5 ]
    data <- data[-1,]
    colnames(data) <- c("Fecha", "Grupo", "Alimento", "Mercado", "Precio_kg")
    data$Precio_kg <- as.numeric(data$Precio_kg)
    data$Fecha <- convertir_fechas_vector(data$Fecha)
    return(data[month(data$Fecha) >= mes_num,])
  }
  
  
  
  # Selección del año según la estructura de datos
  if (Year >= 2019) {
    Meses <- Nombres_Meses[1:length(Price_data_list) - 1]
    posicion_mes <- which(Meses %in% Month)
    
    if (length(posicion_mes) == 0) {
      stop("The requested month is not yet present in the open SIPSA price data.")
    }
    
    Data_Sipsa_Precios <- depurar_y_filtrar(Price_data_list[[posicion_mes + 1]], Mes_Num)
  }
  
  if (Year == 2018 || Year < 2018) {
    Año_selec <- ifelse(Year == 2018, 2, which(Year == 2013:2017) + 1)
    Data_Sipsa_Precios <- depurar_y_filtrar(Price_data_list[[Año_selec]], Mes_Num)
  }
  
  return(Price_data_list)
  
} 
