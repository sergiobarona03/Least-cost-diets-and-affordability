

#-----------------------------------------------------------------------------------------#
#         NUEVA VERSIÓN: CARGA Y DEPURACIÓN DATOS DE COLOMBIA, DANE                       #
#-----------------------------------------------------------------------------------------#

DataCol4 <- function(Month, Year) {

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
  
  Librerias_base = c("tidyverse","rio","janitor","stringdist","lpSolve","knitr")  # Nombra las librerias necesarias
  
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
  #                   TERCERA ETAPA: CARGA DE DATOS DESDE EL DANE (COL)                      # ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#
  
  options(rio.column_names = FALSE)  # No mostrar nombres de columnas al importar como lista
  options(timeout = 1000)  # Tiempo de espera alto
  
  
  # Función para descargar y cargar datos desde el DANE
  cargar_datos_dane <- function(tipo, año, env) {
    
    temp_dir <- tempdir()
    archivo_excel <- file.path(temp_dir, paste0("archivo_", tipo, "_", año, ".xlsx"))
    nombre_data <- paste0("data_list_", tipo, "_", año, "_ev")
    
    if (!exists(nombre_data, envir = env)) {
      url_excel <- switch(
        tipo,
        "precios" = {
          if (año == 2023) {
            sprintf("https://www.dane.gov.co/files/operaciones/SIPSA/anex-SIPSA-SerieHistoricaMayorista-Dic2023.xlsx")
          } else if (año == 2024) {
            sprintf("https://www.dane.gov.co/files/operaciones/SIPSA/anex-SIPSA-SerieHistoricaMayorista-2024.xlsx")
          } else if (año > 2017) {
            sprintf("https://www.dane.gov.co/files/investigaciones/agropecuario/sipsa/series-historicas/series-historicas-precios-mayoristas-%d.xlsx", año)
          } else {
            "https://www.dane.gov.co/files/investigaciones/agropecuario/sipsa/series-historicas/series-historicas-precios-mayoristas.xlsx"
          }
        },
        stop("Tipo de datos no reconocido.")
      )
      
      if (!file.exists(archivo_excel)) {
        download.file(url_excel, archivo_excel, mode = "wb", timeout = 444)
        suppressMessages(assign(nombre_data, rio::import_list(archivo_excel, setclass = "tbl"), envir = env))
      } else {
        suppressMessages(assign(nombre_data, rio::import_list(archivo_excel, setclass = "tbl"), envir = env))
      }
    }
    
    return(get(nombre_data, envir = env))
  }
  
  
  # Función para crear o reutilizar un entorno
  crear_o_reusar_entorno <- function(nombre_entorno) {
    if (!exists(nombre_entorno, envir = globalenv())) {
      assign(nombre_entorno, new.env(parent = emptyenv()), envir = globalenv())
    }
    return(get(nombre_entorno, envir = globalenv()))
  }
  
  # Crear o reutilizar entornos para precios y abastecimiento
  data_list_precios_ev_nuevo <- crear_o_reusar_entorno("data_list_precios_ev")
  
  # Carga de precios mayoristas
  Price_data_list = cargar_datos_dane("precios", Year, data_list_precios_ev_nuevo)
  
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
  depurar_y_filtrar2 <- function(data, mes_num) {
    data <- data[rowSums(is.na(data)) / ncol(data) < 0.5, colSums(is.na(data)) / nrow(data) < 0.5 ]
    data <- data[-1,]
    colnames(data) <- c("Fecha", "Grupo", "Alimento", "Mercado", "Precio_kg")
    data$Precio_kg <- as.numeric(data$Precio_kg)
    data$Fecha <- convertir_fechas_vector(data$Fecha)
    return(data[month(data$Fecha) == mes_num,])
  }
  
  
  
  # Selección del año según la estructura de datos
  if (Year >= 2019 & Year < 2024) {
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
  
  if (Year == 2024){
    Data_Sipsa_Precios <- depurar_y_filtrar2(Price_data_list[[1]], Mes_Num)
  } 

  return(Data_Sipsa_Precios %>% mutate(Year = Year, Month = Mes_Num))
  }


# Prueba: año 2024
list_2024 <- vector(mode = "list", length = 12)

for (k in 1:12) {
  list_2024[[k]] <- DataCol4(Month = k, Year = 2024) 
}

data_2024 <- do.call(rbind, list_2024)
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\Precios al por mayor\\")
saveRDS(data_2024, "Bases historicas\\2024.rds")

