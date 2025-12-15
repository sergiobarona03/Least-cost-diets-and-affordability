
########################################################
## Metodología 2: Estimación del margen de comercialización
## mediano (Q1 y Q3)
## Train <= 70% de la muestra
## Fuente de datos: DANE - IPC y DANE-SIPSA
## Objetivo: a partir de la unión entre los datos del
## IPC y SIPSA, se calcula un margen Q2 [Q1, Q3] constante
## para estimar los precios hacia adelante.
########################################################

# Directorio base
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

# Fecha de estimación
date_tag <- "121225"


# -----------------------
# 1. Cargar librerías y definir directorios
# -----------------------

# Librerías:
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(ggplot2)
library(writexl)
library(ggforce)

# Conjunto de datos input
in_merged <- file.path(
  base_dir, "working-papers", "working-paper-1225",
  "mapeo ipc-sipsa", "output",paste0(date_tag, "_dataset_ipc_sipsa.xlsx")
)

data_merged <- read_excel(in_merged)

# Ruta de output
out_dir <- file.path(base_dir, "working-papers", "working-paper-1225","m1", "output")

# -----------------------
# 2. Cálculo de márgenes por producto (con frecuencia mensual)
# -----------------------

# Cálculo de márgenes
data_margenes <- data_merged %>%
  filter(
    !is.na(precio_sipsa),
    !is.na(precio_ipc),
    precio_sipsa > 0
  ) %>% mutate( 
    margen = (precio_ipc - precio_sipsa) / precio_sipsa * 100
  )

# Margen en Q2 [Q2, Q3]
margenes_q <- data_margenes %>%
  group_by(alimento_sipsa, articulo_ipc, codigo_articulo) %>%
  summarise(
    n_obs = n(),
    margen_q1 = quantile(margen, 0.25, na.rm = TRUE),
    margen_q2 = quantile(margen, 0.50, na.rm = TRUE),
    margen_q3 = quantile(margen, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(n_obs >= 12)

# Guardar los margenes Q2 [Q1, Q3] en .csv y .xlsx
write_csv(margenes_q,file.path(out_dir, paste0(date_tag, "_margenes_q1_q3_sipsa.csv")))
write_xlsx(list(margenes_q = margenes_q),file.path(out_dir, paste0(date_tag, "_margenes_q1_q3_sipsa.xlsx")))

# -----------------------
# 3. Conjunto de datos de validación
# -----------------------
# Preparación de la base de datos
data_val <- data_merged %>%
  filter(
    !is.na(precio_sipsa),
    !is.na(precio_ipc),
    precio_sipsa > 0
  ) %>%
  left_join(
    margenes_q %>% select(alimento_sipsa, margen_q1, margen_q2, margen_q3),
    by = "alimento_sipsa"
  ) %>%
  drop_na(margen_q2) %>%
  mutate(
    date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(alimento_sipsa, date)

# Dividir la muestra
data_val <- data_val %>%
  group_by(alimento_sipsa) %>%
  mutate(t_id = row_number(),
    cutoff = floor(0.7 * max(t_id)),
    sample = ifelse(t_id <= cutoff, "train", "test")) %>% ungroup()

# Datos de validación (30%)
test_data <- data_val %>% filter(sample == "test")

# -----------------------
# 4. Estimación de precios
# -----------------------
test_data <- test_data %>%
  mutate(
    pred_q1 = precio_sipsa * (1 + margen_q1 / 100),
    pred_q2 = precio_sipsa * (1 + margen_q2 / 100),
    pred_q3 = precio_sipsa * (1 + margen_q3 / 100)
  )

# -----------------------
# 5. Cálculo de las métricas de validación
# -----------------------
# Las métricas se calculan por alimento
metrics_food <- test_data %>%
  group_by(alimento_sipsa) %>%
  summarise(
    n_test = n(),
    RMSE_Q1 = Metrics::rmse(precio_ipc, pred_q1),
    RMSE_Q2 = Metrics::rmse(precio_ipc, pred_q2),
    RMSE_Q3 = Metrics::rmse(precio_ipc, pred_q3),
    MAPE_Q1 = Metrics::mape(precio_ipc, pred_q1)*100,
    MAPE_Q2 = Metrics::mape(precio_ipc, pred_q2)*100,
    MAPE_Q3 = Metrics::mape(precio_ipc, pred_q3)*100,
    .groups = "drop"
  )

# Guardar métricas de validación
write_csv(
  metrics_food,
  file.path(out_dir, paste0(date_tag, "_m1_metrics_by_food.csv"))
)

# Cálculo de métricas agregadas
metrics_agg <- tibble(
  scenario = c("Q1 (low margin)", "Q2 (median)", "Q3 (high margin)"),
  RMSE = c(
    Metrics::rmse(test_data$precio_ipc, test_data$pred_q1),
    Metrics::rmse(test_data$precio_ipc, test_data$pred_q2),
    Metrics::rmse(test_data$precio_ipc, test_data$pred_q3)
  ),
  MAPE = c(
    Metrics::mape(test_data$precio_ipc, test_data$pred_q1)*100,
    Metrics::mape(test_data$precio_ipc, test_data$pred_q2)*100,
    Metrics::mape(test_data$precio_ipc, test_data$pred_q3)*100
  )
)

write_csv(
  metrics_agg,
  file.path(out_dir, paste0(date_tag, "_m1_metrics_aggregate.csv"))
)
write_xlsx(
  list(
    metrics_food = metrics_food,
    metrics_agg  = metrics_agg
  ),
  file.path(out_dir, paste0(date_tag, "_m1_metrics.xlsx"))
)

print(metrics_agg)

# -----------------------
# 6. Guardar gráficos por páginas (9 por página)
# -----------------------
plot_food <- test_data %>%
  select(alimento_sipsa, date, precio_ipc, pred_q1, pred_q2, pred_q3)

plot_food <- bind_rows(plot_food,
                   data_val %>% filter(sample == "train") %>%
                     select(alimento_sipsa, date, precio_ipc)
                   )  %>% arrange(alimento_sipsa, date)

foods <- sort(unique(plot_food$alimento_sipsa))
n_per_page <- 9
n_pages <- ceiling(length(foods) / n_per_page)

for (p in seq_len(n_pages)) {
  
  gg_food <- ggplot(plot_food, aes(x = date)) +
    geom_ribbon(
      aes(ymin = pred_q1, ymax = pred_q3),
      alpha = 0.25, fill = "red"
    ) +
    geom_line(aes(y = precio_ipc), linewidth = 1) +
    geom_line(aes(y = pred_q2), linetype = "dashed", linewidth = 1,
              col = "black") +
    facet_wrap_paginate(
      ~ alimento_sipsa,
      scales = "free_y",
      ncol = 3,
      nrow = 3,
      page = p
    ) +
    
    labs(
      title = "Observed vs Predicted Retail Prices — Methodology 1",
      subtitle = paste("Q1–Q3 margin band | Page", p, "of", n_pages),
      y = "Price",
      x = NULL
    ) +
    
    theme_classic(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold"),
      legend.position = "none"
    )
  
  ggsave(
    filename = file.path(out_dir, paste0(date_tag, "_m1_forecast_by_food_page_", sprintf("%02d", p), ".png")),
    plot = gg_food,
    width = 13,
    height = 9,
    dpi = 300
  )
}
