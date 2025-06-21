
##----------------------------------##
## Pruebas de raíz unitaria         ##
##----------------------------------##

# Cargar librerías
library(zoo)
library(lubridate)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

##-----------------------------##
## 1. Pruebas de Dickey-Füller ##
##-----------------------------##

# Cargar datos
all_ts = readxl::read_excel("Time-series\\ipc_sipsa_ts.xlsx")

# Cambiar formato
all_ts <- all_ts %>%
  mutate(
    fecha = as.Date(paste(ano, mes_num, "01", sep = "-")),
    precio_ipc = as.numeric(precio_500g),
    precio_sipsa = as.numeric(precio_medio)
  ) %>%
  select(fecha, nombre_ciudad, articulo, sipsa, precio_ipc, precio_sipsa)

# Bucle para la prueba
city_i = levels(as.factor(all_ts$nombre_ciudad))
food_j = levels(as.factor(all_ts$articulo))

for (i in city_i) {

  df.city = all_ts %>% filter(nombre_ciudad == i)
  food_j = levels(as.factor(df.city$articulo))
  
  for (j in food_j) {
    
    print(paste0("DONE: ", j, " en ", i))
    df.food = df.city %>% filter(articulo == j) %>%
    select(fecha, articulo, precio_ipc) %>% distinct()
  
  # Crear la serie de tiempo
  serie_ipc <- ts(df.food$precio_ipc,
              start = c(year(min(df.food$fecha))
                        , month(min(df.food$fecha))), frequency = 12)
  
  # Raíz unitaria para precio sipsa
  # Ese na.omit() está mal, porque la serie debe ser continua
  library(urca)
  
  if (all(is.na(serie_ipc))){
    serie_ipc = NA
  } else {
    serie_ipc <- na.trim(serie_ipc)}
  
  if (all(is.na(serie_ipc))) {
    output.ipc = data.frame(articulo = unique(df.food$articulo),
                              start_date =  NA,
                              end_date = NA,
                              tau = NA,
                              phi = NA, 
                              # 1%
                              tau_1 = NA,
                              phi_1 = NA,
                              # 5%
                              tau_5 = NA,
                              phi_5 = NA,
                              h0_tau_1 = NA, h0_tau_5 =NA,
                              h0_phi_1 = NA, h0_phi_5 = NA)
  } else if ((anyNA(serie_ipc) == TRUE) | length(serie_ipc) < 12) {
    
    output.ipc = data.frame(articulo = unique(df.food$articulo),
                            start_date =  as.Date(as.yearmon(time(serie_ipc)[1])),
                            end_date = as.Date(as.yearmon(tail(time(serie_ipc)[!is.na(serie_ipc)], 1))),
                              tau = NA,
                              phi = NA, 
                              # 1%
                              tau_1 = NA,
                              phi_1 = NA,
                              # 5%
                              tau_5 = NA,
                              phi_5 = NA,
                              h0_tau_1 = NA, h0_tau_5 =NA,
                              h0_phi_1 = NA, h0_phi_5 = NA)
    
  } else {
  
  adf_ipc = summary(urca::ur.df(serie_ipc, 
          selectlags =  "BIC", type = "drift"))
  
  # Definir salida
  output.ipc = data.frame(articulo = unique(df.food$articulo),
                          start_date =  as.Date(as.yearmon(time(serie_ipc)[1])),
                          end_date = as.Date(as.yearmon(tail(time(serie_ipc)[!is.na(serie_ipc)], 1))),
                          
                          tau = adf_ipc@teststat[1],
                          phi = adf_ipc@teststat[2], 
                          # 1%
                          tau_1 = adf_ipc@cval[1,1],
                          phi_1 = adf_ipc@cval[2,1],
                          # 5%
                          tau_5 = adf_ipc@cval[1,2],
                          phi_5 = adf_ipc@cval[2,2])
  
  # Rechazo o no de H0
  output.ipc$h0_tau_1 = output.ipc$tau < output.ipc$tau_1
  output.ipc$h0_tau_5 = output.ipc$tau < output.ipc$tau_5
  
  output.ipc$h0_phi_1 = output.ipc$phi > output.ipc$phi_1
  output.ipc$h0_phi_5 = output.ipc$phi > output.ipc$phi_5
  }
  
  # Pregunta: ¿hay alimentos sipsa?
  lista_sipsa <- df.city %>%
    filter(articulo == j) %>%
    distinct(sipsa) %>%
    pull(sipsa)
  
  if(!anyNA(lista_sipsa)) {
    
    lista_df_sipsa = vector(mode = "list", length = length(lista_sipsa))
    
    for (k in 1:length(lista_sipsa)) {
      k = 1
      df_sipsa = df.city %>%
        filter(articulo == j & sipsa == lista_sipsa[k])
      
      serie_sipsa <- ts(df_sipsa$precio_sipsa,
                      start = c(year(min(df_sipsa$fecha))
                                , month(min(df_sipsa$fecha))), frequency = 12)
      
      if (all(is.na(serie_sipsa))){
        serie_sipsa = NA
      } else {
      serie_sipsa <- na.trim(serie_sipsa)}
      
      
      if (all(is.na(serie_sipsa))) {
        output.sipsa = data.frame(articulo = unique(df_sipsa$sipsa),
                                start_date =  NA,
                                end_date = NA,
                                tau = NA,
                                phi = NA, 
                                # 1%
                                tau_1 = NA,
                                phi_1 = NA,
                                # 5%
                                tau_5 = NA,
                                phi_5 = NA,
                                h0_tau_1 = NA, h0_tau_5 =NA,
                                h0_phi_1 = NA, h0_phi_5 = NA)
      } else if ((anyNA(serie_sipsa) == TRUE) | length(serie_sipsa) < 12) {
        
        output.sipsa = data.frame(articulo = unique(df_sipsa$sipsa),
                                  start_date =  as.Date(as.yearmon(time(serie_sipsa)[1])),
                                  end_date = as.Date(as.yearmon(tail(time(serie_sipsa)[!is.na(serie_sipsa)], 1))),
                                  tau = NA,
                                  phi = NA, 
                                  # 1%
                                  tau_1 = NA,
                                  phi_1 = NA,
                                  # 5%
                                  tau_5 = NA,
                                  phi_5 = NA,
                                  h0_tau_1 = NA, h0_tau_5 =NA,
                                  h0_phi_1 = NA, h0_phi_5 = NA)
        
      } else {
        
        adf_sipsa = summary(urca::ur.df(serie_sipsa, 
                                      selectlags =  "BIC", type = "drift"))
        
        # Definir salida
        output.sipsa = data.frame(articulo = unique(df_sipsa$sipsa),
                                start_date =  as.Date(as.yearmon(time(serie_sipsa)[1])),
                                end_date = as.Date(as.yearmon(tail(time(serie_sipsa)[!is.na(serie_sipsa)], 1))),
                                
                                tau = adf_sipsa@teststat[1],
                                phi = adf_sipsa@teststat[2], 
                                # 1%
                                tau_1 = adf_sipsa@cval[1,1],
                                phi_1 = adf_sipsa@cval[2,1],
                                # 5%
                                tau_5 = adf_sipsa@cval[1,2],
                                phi_5 = adf_sipsa@cval[2,2])
        
        # Rechazo o no de H0
        output.sipsa$h0_tau_1 = output.sipsa$tau < output.ipc$tau_1
        output.sipsa$h0_tau_5 = output.sipsa$tau < output.ipc$tau_5
        
        output.sipsa$h0_phi_1 = output.sipsa$phi > output.ipc$phi_1
        output.sipsa$h0_phi_5 = output.sipsa$phi > output.ipc$phi_5
      }
      
      lista_df_sipsa[[k]] = output.sipsa
      

      if (all(is.na(serie_sipsa)) | all(is.na(serie_ipc))) {
        
        lista_df_sipsa[[k]]  = lista_df_sipsa[[k]] %>% mutate(
          coint.p.type1 = NA, # trend
          coint.p.type2 = NA, # linear trend
          coint.p.type3 = NA  # quadratic trend
          
        )
        
          
      } else if ((anyNA(serie_sipsa) == TRUE) | (length(serie_sipsa) < 12) |
                 (anyNA(serie_ipc) == TRUE) | (length(serie_ipc) < 12)) {
        
        lista_df_sipsa[[k]]  = lista_df_sipsa[[k]] %>% mutate(
          coint.p.type1 = NA, # trend
          coint.p.type2 = NA, # linear trend
          coint.p.type3 = NA  # quadratic trend
          
        )
        
      } else {
       
        # SIPSA para que termine en marzo-2018
        sipsa_coint <- window(serie_sipsa, end = c(2018, 3))
        ipc_coint <- window(serie_ipc, start = start(sipsa_coint), 
                            end = end(sipsa_coint))

        # Cointegración:
        coint.output = coint.test(ipc_coint, sipsa_coint)
    
        lista_df_sipsa[[k]]  = lista_df_sipsa[[k]] %>% mutate(
          coint.p.type1 = coint.output[1,3], # trend
          coint.p.type2 = coint.output[2,3], # linear trend
          coint.p.type3 = coint.output[3,3]  # quadratic trend
          
        )
        
      }  
      
      
    }
    
    overall_output = bind_rows(output.ipc %>% mutate(clase = "IPC"),
                           do.call(rbind, lista_df_sipsa) %>% mutate(clase = "SIPSA"))
    
  } else {
   
    overall_output = output.ipc %>% mutate(clase = "IPC")
     
  }
  
  
  # Se guarda:
  # Ruta del directorio y archivo
  dir_path <- file.path("Time-series", "ts-unit-root", i)
  file_path <- file.path(dir_path, paste0(i, "_", j, "_df.xlsx"))
  
  # Crear carpeta si no existe
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  # Guardar el gráfico
  writexl::write_xlsx(overall_output, path = file_path)
  
  }
  
}


