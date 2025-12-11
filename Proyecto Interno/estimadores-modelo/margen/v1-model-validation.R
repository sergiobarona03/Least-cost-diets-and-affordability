##################################################################
## MODELO VALIDADO 70/30 POR PRODUCTO (articulo_ipc) – Cali     ##
## ln(precio_retail) ~ ln(precio_mayorista) + mes               ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

library(lubridate)
library(tidyverse)
library(broom)
library(Metrics)

#-------------------------------------------------
# 0. Directorio de trabajo y datos
#-------------------------------------------------
setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")
source("margen-dist/v1-join-ipc-sipsa.R")

#-------------------------------------------------
# 1. Datos Retail (IPC) – Cali
#-------------------------------------------------
retail <- retail_99_18 %>%
  rename(
    precio_ipc   = precio_500g,
    articulo_ipc = articulo
  ) %>%
  filter(ciudad == "76") %>%   
  select(articulo_ipc, codigo_articulo, ano, mes_num, precio_ipc)

#-------------------------------------------------
# 2. Datos Wholesale (SIPSA) – Cali
#-------------------------------------------------
wholesale <- whole_18_mean %>%
  rename(
    precio_sipsa   = precio_medio,
    alimento_sipsa = Alimento
  ) %>%
  filter(cod_mun == "76001") %>%
  select(alimento_sipsa, Year, Month, precio_sipsa)

#-------------------------------------------------
# 3. Mapa IPC–SIPSA
#-------------------------------------------------
mapa <- ipc_sipsa %>%
  select(
    alimento_sipsa = sipsa,
    articulo_ipc   = retail
  )

#-------------------------------------------------
# 4. Merge
#-------------------------------------------------
data_merged <- wholesale %>%
  left_join(mapa, by = "alimento_sipsa") %>%
  left_join(
    retail,
    by = c("articulo_ipc", "Year" = "ano", "Month" = "mes_num")
  )

#-------------------------------------------------
# 5. Construir logs + fecha
#-------------------------------------------------
margenes <- data_merged %>%
  mutate(
    fecha        = as.Date(paste(Year, Month, "01", sep="-")),
    ln_retail    = log(precio_ipc),
    ln_wholesale = log(precio_sipsa),
    mes_factor   = factor(Month)
  ) %>%
  filter(
    !is.na(ln_retail),
    !is.na(ln_wholesale)
  ) %>%
  arrange(articulo_ipc, fecha)

#-------------------------------------------------
# 6. Definir plot dir
#-------------------------------------------------
plot_dir <- "estimadores-modelo/output/margen/plots_validacion_CALI_70_30"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

#-------------------------------------------------
# 7. Función para ejecutar MODELO + VALIDACIÓN por producto
#-------------------------------------------------
validar_modelo <- function(df) {
  
  n <- nrow(df)
  n_train <- floor(0.7 * n)
  
  df_train <- df[1:n_train, ]
  df_test  <- df[(n_train+1):n, ]
  
  # Modelo con datos de entrenamiento únicamente
  modelo <- lm(ln_retail ~ ln_wholesale + mes_factor, data = df_train)
  
  # Predicción para toda la serie
  df$pred <- predict(modelo, newdata = df)
  
  # Métricas
  rmse_train <- rmse(df_train$ln_retail, df_train$pred)
  rmse_test  <- rmse(df_test$ln_retail,  df_test$pred)
  
  mae_train <- mae(df_train$ln_retail, df_train$pred)
  mae_test  <- mae(df_test$ln_retail,  df_test$pred)
  
  # Output
  list(
    modelo = modelo,
    df     = df,
    metrics = tibble(
      articulo_ipc = df$articulo_ipc[1],
      n_obs        = n,
      rmse_train   = rmse_train,
      rmse_test    = rmse_test,
      mae_train    = mae_train,
      mae_test     = mae_test
    )
  )
}

#-------------------------------------------------
# 8. Aplicar modelo por producto IPC
#-------------------------------------------------
modelos_lista <- margenes %>%
  group_by(articulo_ipc) %>%
  group_split() %>%
  map(validar_modelo)

# Extraer métricas
metricas_tbl <- bind_rows(map(modelos_lista, "metrics"))

#-------------------------------------------------
# 9. Guardar tabla métrica
#-------------------------------------------------
dir_out_tablas <- "estimadores-modelo/output/margen"
if (!dir.exists(dir_out_tablas)) dir.create(dir_out_tablas, recursive = TRUE)

write_csv(metricas_tbl,
          file.path(dir_out_tablas, "CALI_111225_metricas_validacion_70_30_por_articuloIPC.csv"))

#-------------------------------------------------
# 10. Función de gráfico con train/test + predicción
#-------------------------------------------------
plot_validacion <- function(df_prod, output_dir) {
  
  articulo <- unique(df_prod$articulo_ipc)
  n <- nrow(df_prod)
  n_train <- floor(0.7*n)
  split_date <- df_prod$fecha[n_train]
  
  safe_name <- gsub("[^A-Za-z0-9_]", "_", articulo)
  
  p <- ggplot(df_prod, aes(x=fecha)) +
    geom_line(aes(y = ln_retail, colour="Observado"), linewidth=0.8) +
    geom_line(aes(y = pred, colour="Predicho"), linewidth=0.8, linetype="dashed") +
    geom_vline(xintercept = split_date, linetype="dotted", colour="black") +
    annotate("text", x = split_date, y = max(df_prod$ln_retail, na.rm=TRUE),
             label="Split 70/30", vjust=-0.5, size=3) +
    labs(
      title = paste0("Cali – Validación 70/30 – ", articulo),
      subtitle = "Modelo: ln(precio IPC) ~ ln(precio SIPSA) + dummies mensuales",
      y = "Log precio retail",
      x = "Fecha",
      colour = ""
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path(output_dir, paste0("Validacion_7030_", safe_name, ".png")),
    plot = p,
    width = 9, height = 5, dpi = 300
  )
}

#-------------------------------------------------
# 11. Generar plots completos por alimento
#-------------------------------------------------
walk(modelos_lista, ~ plot_validacion(.x$df, plot_dir))

#-------------------------------------------------
# FIN
#-------------------------------------------------
