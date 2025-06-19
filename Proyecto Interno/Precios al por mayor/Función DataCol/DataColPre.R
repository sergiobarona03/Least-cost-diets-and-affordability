
#------------------------------------------------------------------------------------------#
#         PRIMERA FUNCIÓN: CARGA Y DEPURACIÓN DATOS DE COLOMBIA, DANE                      #
#-----------------------------------------------------------------------------------------#

DataCol3 <- function(Month, Year) {
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
  validar_parametros(Year, "numeric", c(2013, 2025))
  
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
 
   # Función carga de datos de precios
  cargar_datos_precios <- function(año, carpeta, env) {
    archivo_excel <- if (año %in% 2024:2025) {
      file.path(carpeta, paste0("anex-SIPSA-SerieHistoricaMayorista-", año, ".xlsx"))
    } else if (año == 2023) {
      file.path(carpeta, "anex-SIPSA-SerieHistoricaMayorista-Dic2023.xlsx")
    } else if (año %in% 2013:2017) {
      file.path(carpeta, "series-historicas-precios-mayoristas.xlsx")
    } else {
      file.path(carpeta, paste0("series-historicas-precios-mayoristas-", año, ".xlsx"))
    }
    
    nombre_data <- paste0("data_list_precios_", año, "_ev")
    
    # Verificar si el archivo existe
    if (!file.exists(archivo_excel)) {
      stop(paste("El archivo", archivo_excel, "no se encuentra en la carpeta especificada."))
    }
    
    # Verificar si los datos ya están en el entorno
    if (!exists(nombre_data, envir = env)) {
      hojas <- readxl::excel_sheets(archivo_excel)
      
      # Si el archivo corresponde a 2024 o 2025, aplicar skip = 5
      if (año %in% 2024:2025) {
        data_importada <- rio::import_list(archivo_excel, setclass = "tbl", which = hojas, skip = 5)
      } else {
        data_importada <- rio::import_list(archivo_excel, setclass = "tbl", which = hojas)
      }
      
      suppressMessages(assign(nombre_data, data_importada, envir = env))
    }
    
    return(get(nombre_data, envir = env))
  }
  
  
  # Definir la carpeta donde están los archivos
  carpeta_local <- "C:\\Users\\danie\\OneDrive\\Documentos\\Datos precios\\"
  
  # Función para crear o reutilizar un entorno
  crear_o_reusar_entorno <- function(nombre_entorno) {
    if (!exists(nombre_entorno, envir = globalenv())) {
      assign(nombre_entorno, new.env(parent = emptyenv()), envir = globalenv())
    }
    return(get(nombre_entorno, envir = globalenv()))
  }
  
  # Crear o reutilizar entorno para los precios
  data_list_precios_ev_nuevo <- crear_o_reusar_entorno("data_list_precios_ev")
  
  # Cargar datos de precios
  Price_data_list <- tryCatch({
    cargar_datos_precios(Year, carpeta_local, data_list_precios_ev_nuevo)
  }, error = function(e) {
    message("Error al cargar los datos de precios: ", e$message)
    NULL
  })
  
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
  
  
  
  # ------------------ SELECCIÓN DEL AÑO SEGÚN LA ESTRUCTURA DE DATOS ------------------
  
  if (Year %in% 2024:2025) {
    # Buscar hoja que contenga el nombre del año
    hoja_primaria <- grep(as.character(Year), names(Price_data_list), value = TRUE)
    if (length(hoja_primaria) == 0) {
      hoja_primaria <- names(Price_data_list)[1]  # usar la primera hoja como respaldo
    }
    
    if (is.null(Price_data_list[[hoja_primaria]])) {
      stop("No se encontró hoja válida en el archivo para el año ", Year)
    }
    
    Data_Sipsa_Precios <- depurar_y_filtrar(Price_data_list[[hoja_primaria]], Mes_Num)
    
  } else if (Year >= 2019) {
    Meses <- Nombres_Meses[1:(length(Price_data_list) - 1)]
    posicion_mes <- which(Meses %in% Month)
    
    if (length(posicion_mes) == 0) {
      stop("The requested month is not yet present in the open SIPSA price data.")
    }
    
    Data_Sipsa_Precios <- depurar_y_filtrar(Price_data_list[[posicion_mes + 1]], Mes_Num)
    
  } else if (Year == 2018 || Year < 2018) {
    Año_selec <- ifelse(Year == 2018, 2, which(Year == 2013:2017) + 1)
    Data_Sipsa_Precios <- depurar_y_filtrar(Price_data_list[[Año_selec]], Mes_Num)
  }
  
  
}
  
