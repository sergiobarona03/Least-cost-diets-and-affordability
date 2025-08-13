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

print(head(resultados_coca, 20))

# Gráfica para visualizar los resultados:
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Aseguramos que 'escenario' y 'Demo_Group' son factores
resultados_coca <- resultados_coca %>%
  mutate(
    escenario = factor(escenario, levels = c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")),
    Demo_Group = factor(Demo_Group),
    Sex = factor(Sex,
                 labels = c("Hombres", "Mujeres"))
  )

# Función para preparar datos y graficar según sexo
plot_coca_band <- function(data, sexo) {
  data_sexo <- data %>% filter(Sex == sexo)
  
  # Pivotar para columnas Q1, Q2, Q3
  data_wide <- data_sexo %>%
    select(fecha, Demo_Group, escenario, cost_day) %>%
    pivot_wider(names_from = escenario, values_from = cost_day)
  
  ggplot(data_wide, aes(x = fecha, group = Demo_Group)) +
    geom_ribbon(aes(ymin = precio_q1_100g,
                    ymax = precio_q3_100g),
                fill = "red", alpha = 0.15) +  # Banda roja transparente
    geom_line(aes(y = precio_q1_100g), 
              color = "red", linetype = 2, size = 0.6) +  # Q1 roja punteada
    geom_line(aes(y = precio_q3_100g), 
              color = "red", linetype = 2, size = 0.6) +  # Q3 roja punteada
    geom_line(aes(y = precio_q2_100g), color = "black", size = 1) +  # Q2 negra sólida
    facet_wrap(~ Demo_Group, scales = "free_y", ncol = 3) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    labs(
      title = paste("Evolución del costo diario con banda entre Q1 y Q3 (", sexo, ")", sep = ""),
      x = "Fecha",
      y = "Costo diario (COP)"
    ) +
    theme_bw(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}

# Graficar para hombres
p_hombres <- plot_coca_band(resultados_coca,
                            "Hombres")
print(p_hombres)

# Graficar para mujeres
p_mujeres <- plot_coca_band(resultados_coca,
                            "Mujeres")
print(p_mujeres)

# El costo por 1000kcal no es constante para cada grupo demográfico
coca_1000kcal = resultados_coca %>% group_by(fecha, escenario) %>%
  summarise(mean_1000kcal = mean(Cost_1000kcal))

# Pivotar para tener columnas q1, q2, q3
coca_wide <- coca_1000kcal %>%
  pivot_wider(names_from = escenario, values_from = mean_1000kcal)

ggplot(coca_wide, aes(x = fecha)) +
  geom_ribbon(aes(ymin = precio_q1_100g, ymax = precio_q3_100g),
              fill = "red", alpha = 0.15) +
  geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.7) +
  geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.7) +
  geom_line(aes(y = precio_q2_100g), color = "black", size = 1) +
  labs(
    title = "CoCA - Evolución del costo por 1000 kcal",
    x = "Fecha",
    y = "Costo promedio (COP)",
    caption = "Líneas rojas punteadas: cuartiles 1 y 3; línea negra: mediana (Q2)"
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

