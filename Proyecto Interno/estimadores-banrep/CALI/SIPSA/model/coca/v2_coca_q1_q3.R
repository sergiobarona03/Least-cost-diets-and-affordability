# ============================================================
#   CoCA - Costo de una dieta suficiente en energía
#   Cali | Escenario 2 (Q1–Q3)
#   Versión con facet_grid para uso en informe
# ============================================================

# --- Librerías ---
library(lubridate)
library(tidyverse)
library(FoodpriceR)
library(scales)
library(RColorBrewer)
library(patchwork)

# --- Definir directorio de trabajo ---
setwd("C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# --- Cargar base de datos ---
input_cali_hat <- read.csv("estimadores-banrep/CALI/SIPSA/input/v2/v2_q1_q3_comp_price_data_cali.csv")

# --- Escenarios de precio ---
escenarios <- c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")

# --- Inicializar resultados ---
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
      output_k[[t]] <- data.frame(
        Food = NA,
        quantity = NA,
        Demo_Group = NA,
        Sex = NA,
        cost_day = NA,
        Cost_1000kcal = NA
      )
      output_k[[t]]$fecha = fechas_k[t]
      output_k[[t]]$escenario = var_precio
    })
  }
  resultados[[k]] <- output_k
}

# --- Combinar resultados ---
resultados_coca <- bind_rows(resultados)

# --- Preparar datos ---
resultados_coca <- resultados_coca %>%
  dplyr::mutate(
    escenario = factor(escenario, levels = c("precio_q1_100g",
                                             "precio_q2_100g",
                                             "precio_q3_100g")),
    Demo_Group = factor(Demo_Group),
    Sex = factor(Sex, labels = c("Hombres", "Mujeres"))
  )

writexl::write_xlsx(resultados_coca ,
                    "estimadores-banrep/CALI/SIPSA/output/v2/v2_sipsa_coca_cali.xlsx")

# ============================================================
#   VISUALIZACIÓN PRINCIPAL: Facet por sexo y grupo
# ============================================================

# --- Pivotear datos ---
coca_plot_data <- resultados_coca %>%
  select(fecha, Sex, Demo_Group, escenario, cost_day) %>%
  pivot_wider(names_from = escenario, values_from = cost_day) %>%
  filter(!is.na(precio_q2_100g))

# --- Gráfico facetado limpio : para hombres ########
g_coca_facet_male <- ggplot(coca_plot_data %>%
                              filter(Sex == "Hombres"), aes(x = fecha)) +
  geom_ribbon(aes(ymin = precio_q1_100g, ymax = precio_q3_100g),
              fill = "red", alpha = 0.15) +
  geom_line(aes(y = precio_q2_100g), color = "black", size = 0.8) +
  geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.5) +
  geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.5) +
  facet_wrap(. ~ Demo_Group, ncol = 3, scales = "free_y") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(
    title = "Hombres: Evolución del costo diario de una dieta suficiente en energía (CoCA)",
    subtitle = "Cali - Escenario 2 (Q1–Q3)",
    x = "Fecha",
    y = "Costo diario (COP)",
    caption = "Líneas rojas punteadas: cuartiles 1 y 3; línea negra: mediana (Q2)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15)
  )

# --- Mostrar y guardar ---
ggsave(g_coca_facet_male,
       filename = "estimadores-banrep/CALI/SIPSA/output/v2/v2_sipsa_coca_male_cali.png",
       dpi = 600, width = 15, height = 9)

# --- Gráfico facetado limpio : para mujeres ########
g_coca_facet_female <- ggplot(coca_plot_data %>%
                                filter(Sex == "Mujeres"), aes(x = fecha)) +
  geom_ribbon(aes(ymin = precio_q1_100g, ymax = precio_q3_100g),
              fill = "red", alpha = 0.15) +
  geom_line(aes(y = precio_q2_100g), color = "black", size = 0.8) +
  geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.5) +
  geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.5) +
  facet_wrap(. ~ Demo_Group, ncol = 3, scales = "free_y") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(
    title = "Mujeres: Evolución del costo diario de una dieta suficiente en energía (CoCA)",
    subtitle = "Cali - Escenario 2 (Q1–Q3)",
    x = "Fecha",
    y = "Costo diario (COP)",
    caption = "Líneas rojas punteadas: cuartiles 1 y 3; línea negra: mediana (Q2)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15)
  )

# --- Mostrar y guardar ---
ggsave(g_coca_facet_female,
       filename = "estimadores-banrep/CALI/SIPSA/output/v2/v2_sipsa_coca_female_cali.png",
       dpi = 600, width = 15, height = 12)

# ============================================================
#   GRÁFICO AGREGADO: Costo promedio por 1000 kcal
# ============================================================

coca_1000kcal <- resultados_coca %>%
  group_by(fecha, escenario) %>%
  summarise(mean_1000kcal = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = escenario, values_from = mean_1000kcal)

g_coca_agg <- ggplot(coca_1000kcal, aes(x = fecha)) +
  geom_ribbon(aes(ymin = precio_q1_100g, ymax = precio_q3_100g),
              fill = "red", alpha = 0.15) +
  geom_line(aes(y = precio_q2_100g), color = "black", size = 0.9) +
  geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.6) +
  geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.6) +
  labs(
    title = "CoCA - Evolución del costo promedio por 1000 kcal",
    subtitle = "Cali - Escenario 2 (Q1–Q3)",
    x = "Fecha",
    y = "Costo promedio (COP)",
    caption = "Líneas rojas punteadas: cuartiles 1 y 3; línea negra: mediana (Q2)"
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(g_coca_agg,
       filename = "estimadores-banrep/CALI/SIPSA/output/v2/v2_sipsa_coca_1000kcal_cali.png",
       dpi = 600, width = 15, height = 9)
