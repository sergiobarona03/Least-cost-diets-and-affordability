#######################################
## Prueba: primera estimación CoCA   ##
#######################################

# Cargar librerías
library(lubridate)
library(tidyverse)
library(FoodpriceR)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# Cargar base de datos
input_cali_hat <- read.csv("estimadores-banrep/CALI/input/010825_q1_q3_comp_price_data_cali.csv")

# Vector de nombres para las variables de precio en los tres escenarios
escenarios <- c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")

# Inicializar resultados
resultados <- list()

for (k in seq_along(escenarios)) {

  var_precio <- escenarios[k]
  message(paste0("Procesando escenario ", k, ": ", var_precio))
  
  data_k <- input_cali_hat %>%
    mutate(
      Food = food_sipsa,
      Price_100g = .data[[var_precio]],
      Serving = 100,
      Energy = energia_kcal,
      fecha = as.Date(paste(year, month, "01", sep = "-"))
    )
  
  fechas_k <- seq(min(data_k$fecha, na.rm = TRUE),
                  max(data_k$fecha, na.rm = TRUE),
                  by = "month")
  
  output_k <- vector("list", length(fechas_k))
  
  for (t in seq_along(fechas_k)) {

    message(paste0(" - Fecha ", t, ": ", fechas_k[t]))
    
    df.aux <- data_k %>%
      filter(fecha == fechas_k[t]) %>%
      select(Food, Price_100g, Serving, Energy) %>%
      filter(!is.na(Price_100g), !is.na(Energy)) %>%
      mutate(Price_kcal = Price_100g / Energy)
    
    message(paste0("   Filas en df.aux: ", nrow(df.aux)))
    
    if (nrow(df.aux) == 0) {
      message("   No hay datos para esta fecha, asignando NA")
      output_k[[t]] <- NA
      next
    }
    
    tryCatch({
      coca.aux <- FoodpriceR::CoCA(data = df.aux, EER = EER)
      output_k[[t]] <- coca.aux$cost
      output_k[[t]]$fecha = fechas_k[t]
      output_k[[t]]$escenario = var_precio
      message(paste0("   CoCA calculado: ", round(coca.aux$cost, 2)))
    }, error = function(e) {
      warning(paste("Error en CoCA para fecha", fechas_k[t], ":", e$message))
      output_k[[t]] <- data.frame(Food = NA,
                                  quantity = NA,
                                  Demo_Group = NA,
                                  Sex = NA,
                                  cost_day = NA,
                                  Cost_1000kcal = NA)
      output_k[[t]]$fecha = fechas_k[t]
      output_k[[t]]$escenario = var_precio
    })
  }
  
  resultados[[k]] <- output_k
  
  message(paste0("Escenario ", k, " terminado.\n"))
  
}

resultados_coca <- bind_rows(resultados)

print(head(resultados_coca, 10))
