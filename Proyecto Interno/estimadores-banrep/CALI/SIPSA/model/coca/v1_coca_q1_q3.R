
library(lubridate)
library(tidyverse)
library(FoodpriceR)
library(scales)
library(RColorBrewer)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# --- Cargar base de datos ---
input_cali_hat <- read.csv("estimadores-banrep/CALI/SIPSA/input/v1/v1_q1_q3_comp_price_data_cali.csv")

# Vector de nombres para las variables de precio en los tres escenarios
escenarios <- c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")

# Inicializar resultados
resultados <- list()

for (k in seq_along(escenarios)) {
  var_precio <- escenarios[k]
  
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
    df.aux <- data_k %>%
      filter(fecha == fechas_k[t]) %>%
      select(Food, Price_100g, Serving, Energy) %>%
      filter(!is.na(Price_100g), !is.na(Energy)) %>%
      mutate(Price_kcal = Price_100g / Energy)
    
    if (nrow(df.aux) == 0) {
      output_k[[t]] <- NA
      next
    }
    
    tryCatch({
      coca.aux <- FoodpriceR::CoCA(data = df.aux, EER = EER)
      output_k[[t]] <- coca.aux$cost
      output_k[[t]]$fecha = fechas_k[t]
      output_k[[t]]$escenario = var_precio
    }, error = function(e) {
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
}

# Combinar resultados
resultados_coca <- bind_rows(resultados)

# --- Preparar datos ---
resultados_coca <- resultados_coca %>%
  mutate(
    escenario = factor(escenario, levels = c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")),
    Demo_Group = factor(Demo_Group),
    Sex = factor(Sex, labels = c("Hombres", "Mujeres"))
  )

writexl::write_xlsx(resultados_coca, "estimadores-banrep/CALI/SIPSA/output/v1/v1_sipsa_coca_cali.csv")

# --- Función para graficar bandas por grupo demográfico ---
plot_coca_band <- function(data, sexo, age) {
  data_sexo <- data %>% filter(Sex == sexo & Demo_Group == age)
  
  data_wide <- data_sexo %>%
    select(fecha, Demo_Group, escenario, cost_day) %>%
    pivot_wider(names_from = escenario, values_from = cost_day)
  
  ggplot(data_wide, aes(x = fecha, group = Demo_Group)) +
    geom_ribbon(aes(ymin = precio_q1_100g,
                    ymax = precio_q3_100g),
                fill = "red", alpha = 0.15) +
    geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.6) +
    geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.6) +
    geom_line(aes(y = precio_q2_100g), color = "black", size = 1) +
    facet_wrap(~ Demo_Group, scales = "free_y", ncol = 3) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    labs(
      title = paste("CoCA - Evolución del costo diario con banda entre Q1 y Q3 (", 
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
for (sx in levels(resultados_coca$Sex)) {
  for (agx in resultados_coca %>% filter(Sex == sx) %>% 
       mutate(Ages = as.factor(as.character(Demo_Group))) %>%
       dplyr::select(Ages) %>% pull() %>% levels()) {
    ggsave(plot_coca_band(resultados_coca,sx, agx),
           filename = paste0("estimadores-banrep/CALI/SIPSA/output/v1/",
                             sx,"_",agx,".png"), dpi = 300)
  }

}

# --- Costo promedio por 1000 kcal ---
coca_1000kcal <- resultados_coca %>%
  group_by(fecha, escenario) %>%
  summarise(mean_1000kcal = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop")

coca_wide <- coca_1000kcal %>%
  pivot_wider(names_from = escenario, values_from = mean_1000kcal)

# --- Gráfico agregado ---
plot_coca_wide = ggplot(coca_wide, aes(x = fecha)) +
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

ggsave(plot_coca_wide,
       filename = "estimadores-banrep/CALI/SIPSA/output/v1/v1_coca_1000kcal.png", 
       dpi = 300)
