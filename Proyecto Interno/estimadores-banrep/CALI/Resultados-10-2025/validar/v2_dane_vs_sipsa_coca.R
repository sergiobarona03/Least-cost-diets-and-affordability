# ============================================================
#   Comparación de resultados CoCA: SIPSA vs. DANE
#   Cali | Panel facetado por sexo y grupo demográfico
# ============================================================

# --- Librerías ---
library(lubridate)
library(tidyverse)
library(writexl)
library(scales)
library(readxl)

# --- Directorio de trabajo ---
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# ============================================================
#   Cargar y preparar los datos
# ============================================================

# --- Cargar resultados ---
sipsa_coca <- read_excel("estimadores-banrep/CALI/Resultados-10-2025/coca/010825_coca_cali.xlsx")
dane_coca  <- read_excel("estimadores-banrep/CALI/Resultados-10-2025/estimadores-DANE/coca/291025_dane_coca_cali.xlsx")

# --- Estandarizar estructuras ---
sipsa_coca <- sipsa_coca %>%
  mutate(
    Fuente = "SIPSA",
    fecha = as.Date(fecha),
    Sex = factor(Sex, labels = c("Hombres", "Mujeres")),
    Demo_Group = as.factor(Demo_Group)
  )

dane_coca <- dane_coca %>%
  mutate(
    Fuente = "DANE",
    fecha = as.Date(fecha),
    Sex = factor(Sex, labels = c("Hombres", "Mujeres")),
    Demo_Group = as.factor(Demo_Group)
  )

# ============================================================
#   Preparar datos para comparación
# ============================================================

# --- 1. SIPSA: Q1 / Q2 / Q3 (bandas y línea negra) ---
sipsa_wide <- sipsa_coca %>%
  select(fecha, Sex, Demo_Group, escenario, cost_day) %>%
  pivot_wider(
    names_from = escenario,
    values_from = cost_day
  )
# genera columnas: precio_q1_100g, precio_q2_100g, precio_q3_100g

# --- 2. DANE: precio_100g (línea verde) ---
dane_wide <- dane_coca %>%
  select(fecha, Sex, Demo_Group, escenario, cost_day) %>%
  pivot_wider(
    names_from = escenario,
    values_from = cost_day
  )
# genera columna: precio_100g

# --- 3. Unir SIPSA y DANE por fecha / sexo / grupo demográfico ---
coca_plot_data <- full_join(
  sipsa_wide,
  dane_wide,
  by = c("fecha", "Sex", "Demo_Group")
) %>%
  arrange(Sex, Demo_Group, fecha)

# ============================================================
#   Determinar período realmente comparable
#   (filas donde hay datos de ambas fuentes al mismo tiempo)
# ============================================================

coca_plot_data_common <- coca_plot_data %>%
  filter(
    !is.na(precio_q2_100g),   # SIPSA disponible (usamos Q2 como referencia central)
    !is.na(precio_100g)       # DANE disponible
  )

# Nota:
# - Esta intersección es fila a fila, por combo (Sex, Demo_Group, fecha).
# - Es la definición estricta de "tenemos SIPSA y DANE para comparar aquí".

# Si quieres ser estricto a nivel de TODA la ciudad (mismo rango temporal para TODAS las facetas),
# podrías acotar aún más a las fechas que existen para ambos en TODOS los grupos,
# pero eso ya es una decisión editorial más dura.

# ============================================================
#   Función de gráfico comparativo (reutilizable)
# ============================================================

plot_coca_comparativo <- function(data, sexo, periodo = "Completo") {
  
  data_sexo <- data %>% filter(Sex == sexo)
  
  ggplot(data_sexo, aes(x = fecha)) +
    # Banda SIPSA (Q1–Q3)
    geom_ribbon(
      aes(ymin = precio_q1_100g, ymax = precio_q3_100g),
      fill = "red", alpha = 0.15
    ) +
    # Líneas SIPSA
    geom_line(
      aes(y = precio_q1_100g),
      color = "red", linetype = 2, linewidth = 0.5
    ) +
    geom_line(
      aes(y = precio_q3_100g),
      color = "red", linetype = 2, linewidth = 0.5
    ) +
    geom_line(
      aes(y = precio_q2_100g),
      color = "black", linewidth = 0.7
    ) +
    # Línea DANE
    geom_line(
      aes(y = precio_100g),
      color = "darkgreen", linewidth = 0.8
    ) +
    facet_wrap(~ Demo_Group, nrow = 4, scales = "free_y") +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    labs(
      title = paste0(
        sexo,
        ": Comparación del costo diario de una dieta suficiente en energía (CoCA)"
      ),
      subtitle = paste0(
        "Cali - SIPSA (banda roja y línea negra) vs. DANE (línea verde)\nPeríodo: ",
        periodo
      ),
      x = "Fecha",
      y = "Costo diario (COP)",
      caption = "SIPSA: banda roja (Q1–Q3) y línea negra (Q2)\nDANE: línea verde continua (precio_100g)"
    ) +
    theme_bw(base_size = 13) +
    theme(
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 12)
    )
}

# ============================================================
#   Paneles por sexo (período completo tal cual viene cada fuente)
# ============================================================

g_coca_comparacion_male_completo <- plot_coca_comparativo(
  coca_plot_data,
  "Hombres",
  "Completo"
)

ggsave(
  filename = "estimadores-banrep/CALI/Resultados-10-2025/validar/coca/291025_panel_comparacion_coca_male_completo.png",
  plot   = g_coca_comparacion_male_completo,
  dpi    = 600,
  width  = 15,
  height = 9
)

g_coca_comparacion_female_completo <- plot_coca_comparativo(
  coca_plot_data,
  "Mujeres",
  "Completo"
)

ggsave(
  filename = "estimadores-banrep/CALI/Resultados-10-2025/validar/coca/291025_panel_comparacion_coca_female_completo.png",
  plot   = g_coca_comparacion_female_completo,
  dpi    = 600,
  width  = 15,
  height = 9
)

# ============================================================
#   Paneles por sexo (solo período estrictamente comparable)
# ============================================================

g_coca_comparacion_male_common <- plot_coca_comparativo(
  coca_plot_data_common,
  "Hombres",
  "Período común DANE–SIPSA"
)

ggsave(
  filename = "estimadores-banrep/CALI/Resultados-10-2025/validar/coca/291025_panel_comparacion_coca_male_common.png",
  plot   = g_coca_comparacion_male_common,
  dpi    = 600,
  width  = 15,
  height = 9
)

g_coca_comparacion_female_common <- plot_coca_comparativo(
  coca_plot_data_common,
  "Mujeres",
  "Período común DANE–SIPSA"
)

ggsave(
  filename = "estimadores-banrep/CALI/Resultados-10-2025/validar/coca/291025_panel_comparacion_coca_female_common.png",
  plot   = g_coca_comparacion_female_common,
  dpi    = 600,
  width  = 15,
  height = 9
)

# ============================================================
#   MÉTRICAS DE PRECISIÓN: SIPSA (predicción) vs. DANE (observado)
#   (Solo RMSE, MAE, MAPE y % dentro de banda)
# ============================================================

metricas_precision <- coca_plot_data_common %>%
  group_by(Sex, Demo_Group) %>%
  summarise(
    n_obs = n(),
    RMSE = sqrt(mean((precio_q2_100g - precio_100g)^2, na.rm = TRUE)),
    MAE  = mean(abs(precio_q2_100g - precio_100g), na.rm = TRUE),
    MAPE = mean(abs((precio_q2_100g - precio_100g) / precio_100g), na.rm = TRUE) * 100,
    Dentro_Banda = mean(
      precio_100g >= precio_q1_100g & precio_100g <= precio_q3_100g,
      na.rm = TRUE
    ) * 100,
    .groups = "drop"
  )

# ============================================================
#   Tabla formateada y guardado
# ============================================================

tabla_metricas_precision <- metricas_precision %>%
  arrange(Sex, Demo_Group) %>%
  mutate(
    RMSE = round(RMSE, 1),
    MAE = round(MAE, 1),
    MAPE = round(MAPE, 1),
    Dentro_Banda = round(Dentro_Banda, 1)
  ) %>%
  rename(
    "Sexo" = Sex,
    "Grupo demográfico" = Demo_Group,
    "N obs." = n_obs,
    "RMSE (COP)" = RMSE,
    "MAE (COP)" = MAE,
    "MAPE (%)" = MAPE,
    "% Dentro de banda" = Dentro_Banda
  )

# Guardar como objeto R y Excel
save(metricas_precision,
     file = "estimadores-banrep/CALI/Resultados-10-2025/validar/coca/291025_metricas_coca.RData")




