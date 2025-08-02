#######################################
## Prueba: primera estimación CoCA   ##
#######################################

# Cargar librerías
library(lubridate)
library(tidyverse)
library(FoodpriceR)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# Cargar base de datos
input_cali_hat <- read.csv("estimadores-banrep/CALI/input/010825_q1_q3_comp_price_data_cali.csv")

# Vector de nombres para las variables de precio en los tres escenarios
escenarios <- c("precio_hat_q1", "precio_hat_q2", "precio_hat_q3")
resultados <- list()

# Bucle generalizado para cada escenario
for (k in 1:3) {
  # Nombre de la variable de precio actual
  var_precio <- escenarios[k]
  
  # Crear data frame para este caso
  data_k <- input_cali_hat %>%
    mutate(
      Food = food_sipsa,
      Price_100g = .data[[var_precio]],  # Referencia dinámica a la columna
      Serving = 100,
      Energy = energia_kcal,
      fecha = as.Date(paste(year, month, "01", sep = "-"))
    )
  
  # Secuencia de fechas
  fechas_k <- seq(min(data_k$fecha, na.rm = TRUE),
                  max(data_k$fecha, na.rm = TRUE),
                  by = "month")
  
  # Vector para almacenar resultados
  output_k <- vector(mode = "list", length = length(fechas_k))
  
  # Loop de estimación
  for (t in seq_along(fechas_k)) {
    df.aux <- data_k %>%
      filter(fecha == fechas_k[t]) %>%
      select(Food, Price_100g, Serving, Energy) %>%
      filter(!is.na(Price_100g), !is.na(Energy))
    
    # Calcular CoCA
    coca.aux <- FoodpriceR::CoCA(data = df.aux, EER = EER)
    output_k[[t]] <- coca.aux$cost
  }
  
  # Guardar resultados en la lista principal
  resultados[[k]] <- tibble(
    fecha = fechas_k,
    CoCA = unlist(output_k),
    escenario = var_precio
  )
}

# Combinar en un solo data frame
resultados_coca <- bind_rows(resultados)

# Vista previa
head(resultados_coca)
