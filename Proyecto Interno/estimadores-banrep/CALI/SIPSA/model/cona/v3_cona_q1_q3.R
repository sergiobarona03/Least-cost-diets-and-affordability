# ============================================================
#   CoNA - Costo de una dieta adecuada en nutrientes
#   Cali | Escenario 2 (Q1–Q3)
#   Versión con facet_grid por sexo y grupo demográfico
# ============================================================

# --- Paquetes requeridos ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

# Definir directorio de trabajo
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\")

# --- Cargar base de datos ---
input_cali_hat <- read.csv("estimadores-banrep/CALI/SIPSA/input/v3/v3_q1_q3_comp_price_data_cali.csv")

# --- Selección y recodificación de nutrientes ---
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
  select(year, month, food_sipsa, precio_q1_100g, precio_q2_100g, precio_q3_100g,
         codigo_tcac, Energy, Protein, Lipids, Carbohydrates, VitaminC, Folate,
         VitaminA, Thiamine, Riboflavin, Niacin, VitaminB12,
         Magnesium, Phosphorus, Sodium, Calcium, Iron, Zinc)

# --- Definir escenarios de precio ---
escenarios <- c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")

# --- Inicializar lista de resultados ---
resultados <- list()
resultados_comp <- list()

# --- Bucle principal de estimación por escenario ---
for (k in seq_along(escenarios)) {
  var_precio <- escenarios[k]
  message(paste0("Procesando escenario ", k, ": ", var_precio))
  
  data_k <- input_cali_hat %>%
    mutate(
      Food = food_sipsa,
      Price_100g = .data[[var_precio]],
      Serving = 100,
      fecha = as.Date(paste(year, month, "01", sep = "-"))
    )
  
  fechas_k <- seq(min(data_k$fecha, na.rm = TRUE),
                  max(data_k$fecha, na.rm = TRUE),
                  by = "month")
  
  output_k <- vector("list", length(fechas_k))
  compose_k <-vector("list", length(fechas_k))
  
  for (t in seq_along(fechas_k)) {
    message(paste0(" - Fecha ", t, ": ", fechas_k[t]))
    
    df.aux <- data_k %>%
      filter(fecha == fechas_k[t]) %>%
      filter(!is.na(Price_100g), !is.na(Energy))
    
    message(paste0("   Número de alimentos en la fecha: ", nrow(df.aux)))
    
    if (nrow(df.aux) == 0) {
      output_k[[t]] <- NA
      next
    }
    
    tryCatch({
      cona.aux <- FoodpriceR::CoNA(
        data = df.aux,
        EER_LL = EER_LL,
        UL = UL
      )
      output_k[[t]] <- cona.aux$cost
      output_k[[t]]$fecha = fechas_k[t]
      output_k[[t]]$escenario = var_precio
      
      compose_k[[t]] <- cona.aux$comp
      compose_k[[t]]$fecha = fechas_k[t]
      compose_k[[t]]$escenario = var_precio
      
    }, error = function(e) {
      warning(paste("Error en CoNA para fecha", fechas_k[t], ":", e$message))
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
      
      compose_k[[t]] <- data.frame(Food = NA,
                                   quantity = NA,
                                   Demo_Group = NA,
                                   Sex = NA)
      compose_k[[t]]$fecha = fechas_k[t]
      compose_k[[t]]$escenario = var_precio
      
    })
  }
  
  resultados[[k]] <- output_k
  resultados_comp[[k]] <- compose_k
  
  message(paste0("Escenario ", k, " terminado.\n"))
}

# --- Consolidar resultados ---
resultados_cona <- bind_rows(resultados)
resultados_comp <- bind_rows(resultados_comp)

# --- Limpieza y factores ---
resultados_cona <- resultados_cona %>%
  mutate(
    escenario = factor(escenario, levels = c("precio_q1_100g", "precio_q2_100g", "precio_q3_100g")),
    Demo_Group = factor(Demo_Group),
    Sex = factor(Sex, labels = c("Hombres", "Mujeres"))
  )

writexl::write_xlsx(resultados_cona ,
                    "estimadores-banrep/CALI/SIPSA/output/v3/v3_cona_cali.xlsx")
writexl::write_xlsx(resultados_comp,
                    "estimadores-banrep/CALI/SIPSA/output/v3/v3_sipsa_cona_cali_comp.csv")


# ============================================================
#   VISUALIZACIÓN PRINCIPAL: Facet por sexo y grupo
# ============================================================

cona_plot_data <- resultados_cona %>%
  select(fecha, Sex, Demo_Group, escenario, cost_day) %>%
  pivot_wider(names_from = escenario, values_from = cost_day) %>%
  filter(!is.na(precio_q2_100g))

# --- Gráfico facetado principal: Hombres ---
g_cona_facet_male <- ggplot(cona_plot_data %>%
                              filter(Sex == "Hombres"), aes(x = fecha)) +
  geom_ribbon(aes(ymin = precio_q1_100g, ymax = precio_q3_100g),
              fill = "red", alpha = 0.15) +
  geom_line(aes(y = precio_q2_100g), color = "black", size = 0.8) +
  geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.5) +
  geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.5) +
  facet_wrap(. ~ Demo_Group, ncol = 3, scales = "free_y") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(
    title = "Evolución del costo diario de una dieta adecuada en nutrientes (CoNA)",
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
ggsave(g_cona_facet_male,
       filename = "estimadores-banrep/CALI/SIPSA/output/v3/v3_cona_male_cali.png",
       dpi = 600, width = 15, height = 9)

# --- Gráfico facetado principal: Mujeres ---
g_cona_facet_female <- ggplot(cona_plot_data %>%
                                filter(Sex == "Mujeres"), aes(x = fecha)) +
  geom_ribbon(aes(ymin = precio_q1_100g, ymax = precio_q3_100g),
              fill = "red", alpha = 0.15) +
  geom_line(aes(y = precio_q2_100g), color = "black", size = 0.8) +
  geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.5) +
  geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.5) +
  facet_wrap(. ~ Demo_Group, ncol = 3, scales = "free_y") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(
    title = "Mujeres: Evolución del costo diario de una dieta adecuada en nutrientes (CoNA)",
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
ggsave(g_cona_facet_female,
       filename = "estimadores-banrep/CALI/SIPSA/output/v3/v3_cona_female_cali.png",
       dpi = 600, width = 15, height = 12)

# ============================================================
#   GRÁFICO AGREGADO: Costo promedio por 1000 kcal
# ============================================================

cona_1000kcal <- resultados_cona %>%
  group_by(fecha, escenario) %>%
  summarise(mean_1000kcal = mean(Cost_1000kcal, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = escenario, values_from = mean_1000kcal)

g_cona_agg <- ggplot(cona_1000kcal, aes(x = fecha)) +
  geom_ribbon(aes(ymin = precio_q1_100g, ymax = precio_q3_100g),
              fill = "red", alpha = 0.15) +
  geom_line(aes(y = precio_q2_100g), color = "black", size = 0.9) +
  geom_line(aes(y = precio_q1_100g), color = "red", linetype = 2, size = 0.6) +
  geom_line(aes(y = precio_q3_100g), color = "red", linetype = 2, size = 0.6) +
  labs(
    title = "CoNA - Evolución del costo promedio por 1000 kcal",
    subtitle = "Cali - Escenario 2 (Q1–Q3)",
    x = "Fecha",
    y = "Costo promedio (COP)",
    caption = "Líneas rojas punteadas: cuartiles 1 y 3; línea negra: mediana (Q2)"
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Mostrar y guardar gráfico agregado ---
ggsave(g_cona_agg,
       filename = "estimadores-banrep/CALI/SIPSA/output/v3/v3_cona_1000kcal_cali.png",
       dpi = 600, width = 15, height = 9)


