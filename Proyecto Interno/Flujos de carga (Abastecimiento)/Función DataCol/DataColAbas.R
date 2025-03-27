
#------------------------------------------------------------------------------------------#
#         PRIMERA FUNCIÓN: CARGA Y DEPURACIÓN DATOS DE COLOMBIA, DANE                      #
#-----------------------------------------------------------------------------------------#

DataCol2 <- function(Month, Year) {
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
  
  # Función carga de datos abastecimiento 
  cargar_datos_abaste <- function(año, carpeta, env) {
    # Determinar el formato del archivo según el año
    if (año >= 2024) {
      archivo_excel <- file.path(carpeta, paste0("anex-Microdato-abastecimiento-", año, ".xlsx"))
    } else {
      archivo_excel <- file.path(carpeta, paste0("microdato-abastecimiento-", año, ".xlsx"))
    }
    
    nombre_data <- paste0("data_list_abastecimiento_", año, "_ev")
    
    # Verificar si el archivo existe
    if (!file.exists(archivo_excel)) {
      stop(paste("El archivo", archivo_excel, "no se encuentra en la carpeta especificada."))
    }
    
    # Verificar si los datos ya están en el entorno
    if (!exists(nombre_data, envir = env)) {
      suppressMessages(assign(nombre_data, rio::import_list(archivo_excel, setclass = "tbl"), envir = env))
    }
    
    return(get(nombre_data, envir = env))
  }
  
  # Definir la carpeta donde están los archivos
  carpeta_local <- "C:\\Users\\danie\\OneDrive\\Documentos\\Datos abastecimiento\\"
  
  # Función para crear o reutilizar un entorno
  crear_o_reusar_entorno <- function(nombre_entorno) {
    if (!exists(nombre_entorno, envir = globalenv())) {
      assign(nombre_entorno, new.env(parent = emptyenv()), envir = globalenv())
    }
    return(get(nombre_entorno, envir = globalenv()))
  }
  
  # Crear o reutilizar entorno para abastecimiento
  data_list_abast_ev_nuevo <- crear_o_reusar_entorno("data_list_abast_ev")
  
  # Cargar datos de abastecimiento
  Supply_data_list <- tryCatch({
    cargar_datos_abaste(Year, carpeta_local, data_list_abast_ev_nuevo)
  }, error = function(e) {
    message("Error al cargar los datos de abastecimiento: ", e$message)
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
  #------------------ IDENTIFICACIÓN DE MES EN ABASTECIMIENTO    ------------------------------
  
  if (Year >= 2022) {
    Data_Sipsa_Abas <- Supply_data_list[[as.integer(which(sapply(Lista_Semestres, function(x) Month %in% x))) + 2]]
  } else {
    Data_Sipsa_Abas <- Supply_data_list[[as.integer(which(sapply(Lista_Semestres, function(x) Month %in% x))) + 1]]
  }
  
  if (ncol(Data_Sipsa_Abas) < 9) {
    stop("Error: There is no information for the specified date in the DANE data for this month; please omit the supply data.")
  }
  
  if (Year < 2024) {
    
    colnames(Data_Sipsa_Abas) = c("Ciudad_Mercado", "Fecha","Cod_Dep", "Cod_Mun", "Dep_Proc", "Mun_Proc","Grupo", "Alimento", "Cantidad_KG")
    
  } else {
    
    colnames(Data_Sipsa_Abas) = c("Ciudad_Mercado", "Fecha","Cod_Dep", "Cod_Mun", "Dep_Proc", "Mun_Proc","Grupo", "Cod_CPC", "Alimento", "Cantidad_KG")
  }
  
  Data_Sipsa_Abas <- Data_Sipsa_Abas[rowSums(is.na(Data_Sipsa_Abas)) / ncol(Data_Sipsa_Abas) < 0.5, colSums(is.na(Data_Sipsa_Abas)) / nrow(Data_Sipsa_Abas) < 0.5 ]
  Data_Sipsa_Abas <- Data_Sipsa_Abas[-1,]
  Data_Sipsa_Abas$Cantidad_KG <- as.numeric(Data_Sipsa_Abas$Cantidad_KG)
  
  Data_Sipsa_Abas$Ciudad <- sapply(strsplit(as.character(Data_Sipsa_Abas$Ciudad_Mercado), ","), function(x) x[1])
  
  Data_Sipsa_Abas$Fecha <- convertir_fechas_vector(Data_Sipsa_Abas$Fecha)
  
} 

  
  
  
  
  
  
  