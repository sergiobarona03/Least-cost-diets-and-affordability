#######################################
## Prueba: sexta estimación CoNA    ##
#######################################

# Cargar librerías
library(lubridate)
library(tidyverse)
library(FoodpriceR)
library(tidyverse)

# Definir directorio de trabajo
setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Cargar base de datos
input_cali_hat <- read.csv("estimadores-banrep/CALI/DANE/input/v7/v7_comp_price_data_cali.csv")

# Selección y recodificación de nutrientes
input_cali_hat <- input_cali_hat %>%
  rename(
    Energy = energia_kcal,
    Protein = proteina_g,
    Lipids = lipidos_g,
    Carbohydrates = carbohidratos_totales_g,
    VitaminC = vitamina_c_mg,
    Folate = folatos_mcg,
    VitaminA = vitamina_a_er,
    Thiamine = tiamina_mg,
    Riboflavin = riboflavina_mg,
    Niacin = niacina_mg,
    VitaminB12 = vitamina_b12_mcg,
    Magnesium = magnesio_mg,
    Phosphorus = fosforo_mg,
    Sodium = sodio_mg,
    Calcium = calcio_mg,
    Iron = hierro_mg,
    Zinc = zinc_mg
  ) %>%
  select(ano, mes_num, articulo , precio_100g, codigo_tcac,
         Energy, Protein, Lipids, Carbohydrates, VitaminC, Folate, VitaminA,
         Thiamine, Riboflavin, Niacin, VitaminB12, Magnesium, Phosphorus,
         Sodium, Calcium, Iron, Zinc)

# Vector de nombres para las variables de precio en los tres escenarios
escenarios <- c("precio_100g")

# Inicializar resultados
resultados <- list()
resultados_comp <- list()


for (k in seq_along(escenarios)) {
  var_precio <- escenarios[k]
  message(paste0("Procesando escenario ", k, ": ", var_precio))
  
  data_k <- input_cali_hat %>%
    mutate(
      Food = articulo,
      Price_100g = .data[[var_precio]],
      Serving = 100,
      fecha = as.Date(paste(ano, mes_num, "01", sep = "-"))
    )
  
  fechas_k <- seq(min(data_k$fecha, na.rm = TRUE),
                  max(data_k$fecha, na.rm = TRUE),
                  by = "month")
  
  output_k <- vector("list", length(fechas_k))
  output_comp_k <- vector("list", length(fechas_k))
  
  for (t in seq_along(fechas_k)) {
    
    message(paste0(" - Fecha ", t, ": ", fechas_k[t]))
    
    df.aux <- data_k %>%
      filter(fecha == fechas_k[t]) %>%
      filter(!is.na(Price_100g), !is.na(Energy)) 
    
    # Ad hoc: por simplcidad, voy a promediar para evitar duplicados
    # pero un procedimiento más riguroso precisa otro mapeo
    df.aux <- df.aux %>% group_by(Food) %>%
      summarise(
        Price_100g = mean(Price_100g, na.rm = TRUE),
        Serving = 100,
        Energy = mean(Energy, na.rm = TRUE),
        Protein = mean(Protein, na.rm = TRUE),
        Lipids = mean(Lipids, na.rm = TRUE),
        Carbohydrates = mean(Carbohydrates, na.rm = TRUE),
        VitaminC = mean(VitaminC, na.rm = TRUE),
        Folate = mean(Folate, na.rm = TRUE),
        VitaminA = mean(VitaminA, na.rm = TRUE),
        Thiamine = mean(Thiamine, na.rm = TRUE),
        Riboflavin = mean(Riboflavin, na.rm = TRUE),
        Niacin = mean(Niacin, na.rm = TRUE),
        VitaminB12 = mean(VitaminB12, na.rm = TRUE),
        Magnesium = mean(Magnesium, na.rm = TRUE),
        Phosphorus = mean(Phosphorus, na.rm = TRUE),
        Sodium = mean(Sodium, na.rm = TRUE),
        Calcium = mean(Calcium, na.rm = TRUE),
        Iron = mean(Iron, na.rm = TRUE),
        Zinc = mean(Zinc, na.rm = TRUE),
      ) 
    
    
    
    message(paste0("   Número de alimentos en la fecha: ", nrow(df.aux)))
    
    if (nrow(df.aux) == 0) {
      message("   No hay datos para esta fecha, asignando NA")
      output_k[[t]] <- NA
      next
    }
    
    tryCatch({
      cona.aux <- FoodpriceR::CoNA(data = df.aux,
                                   EER_LL = EER_LL,
                                   UL = UL)
      output_k[[t]] <- cona.aux$cost
      output_k[[t]]$fecha = fechas_k[t]
      output_k[[t]]$escenario = var_precio
      
      output_comp_k[[t]] <- cona.aux$comp
      output_comp_k[[t]]$fecha = fechas_k[t]
      output_comp_k[[t]]$escenario = var_precio
      
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
      
      output_comp_k[[t]] <- data.frame(Food = NA,
                                       quantity = NA,
                                       Demo_Group = NA,
                                       Sex = NA)
      output_comp_k[[t]]$fecha = fechas_k[t]
      output_comp_k[[t]]$escenario = var_precio
      
    })
  }
  
  resultados[[k]] <- output_k
  resultados_comp[[k]] <- output_comp_k
  
  message(paste0("Escenario ", k, " terminado.\n"))
  
}

resultados_cona <- do.call(rbind, resultados[[1]])
resultados_cona_comp <- do.call(rbind, resultados_comp[[1]])

print(head(resultados_cona, 20))

# Gráfica para visualizar los resultados:
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)


writexl::write_xlsx(resultados_cona %>% na.omit(), "estimadores-banrep/CALI/DANE/output/v7/v7_dane_cona_cali.xlsx")
writexl::write_xlsx(resultados_cona_comp %>% na.omit(), "estimadores-banrep/CALI/DANE/output/v7/v7_dane_cona_cali_comp.xlsx")


# Función para preparar datos y graficar según sexo
plot_cona_band <- function(data, sexo, age) {
  data_sexo <- data %>% filter(Sex == sexo & Demo_Group == age)
  
  # Pivotar para columnas Q1, Q2, Q3
  data_wide <- data_sexo %>%
    select(fecha, Demo_Group, escenario, cost_day) %>%
    pivot_wider(names_from = escenario, values_from = cost_day)
  
  ggplot(data_wide, aes(x = fecha, group = Demo_Group)) +
    geom_line(aes(y = precio_100g), 
              color = "red", linetype = 2, size = 0.6) +  
    facet_wrap(~ Demo_Group, scales = "free_y", ncol = 3) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    labs(
      title = paste("CoNA - Evolución del costo diario - DANE -", 
                    sexo, " - ", age ,")", sep = ""),
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

# --- Loop para imprimir gráficos por sexo y por edad ---
resultados_cona$Sex = as.factor(resultados_cona$Sex)
resultados_cona$Demo_Group = as.factor(resultados_cona$Demo_Group)

for (sx in levels(resultados_cona$Sex)) {
  for (agx in resultados_cona %>% filter(Sex == sx) %>% 
       mutate(Ages = as.factor(as.character(Demo_Group))) %>%
       dplyr::select(Ages) %>% pull() %>% levels()) {
    ggsave(plot_cona_band(resultados_cona,sx, agx),
           filename = paste0("estimadores-banrep/CALI/DANE/output/v7/cona_",
                             sx,"_",agx,".png"), dpi = 300)
  }
  
}

# El costo por 1000kcal no es constante para cada grupo demográfico
cona_1000kcal = resultados_cona %>% group_by(fecha, escenario) %>%
  summarise(mean_1000kcal = mean(Cost_1000kcal))

# Pivotar para tener columnas q1, q2, q3
cona_wide <- cona_1000kcal %>%
  pivot_wider(names_from = escenario, values_from = mean_1000kcal)

plot_cona_wide = ggplot(cona_wide, aes(x = fecha)) +
  geom_line(aes(y = precio_100g), color = "red", linetype = 2, size = 0.7) +
  labs(
    title = "CoNA - Evolución del costo por 1000 kcal",
    x = "Fecha",
    y = "Costo promedio (COP)",
    caption = "Líneas rojas punteadas: cuartiles 1 y 3; línea negra: mediana (Q2)"
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(plot_cona_wide,
       filename = "estimadores-banrep/CALI/DANE/output/v7/v7_cona_1000kcal.png", 
       dpi = 300)