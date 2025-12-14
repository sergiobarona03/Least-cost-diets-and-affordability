##################################################################
## MODELO: ln(precio_retail) ~ ln(precio_mayorista) + mes (Cali)##
## Un modelo por alimento IPC (articulo_ipc)                    ##
## Fecha: 31 de julio de 2025                                   ##
##################################################################

library(lubridate)
library(tidyverse)
library(broom)

#-------------------------------------------------
# 0. Directorio de trabajo y datos
#-------------------------------------------------
setwd("C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Cargar datos (debe crear retail_99_18, whole_18_mean, ipc_sipsa, etc.)
source("margen-dist/v1-join-ipc-sipsa.R")

#-------------------------------------------------
# 1. Datos Retail (IPC) – Cali
#-------------------------------------------------
retail <- retail_99_18 %>%
  rename(
    precio_ipc   = precio_500g,
    articulo_ipc = articulo
  ) %>%
  filter(ciudad == "76") %>%   # Cali
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
# 4. Unir las tres fuentes
#-------------------------------------------------
data_merged <- wholesale %>%
  left_join(mapa, by = "alimento_sipsa") %>%
  left_join(
    retail,
    by = c("articulo_ipc", "Year" = "ano", "Month" = "mes_num")
  )

#-------------------------------------------------
# 5. Construir márgenes y logs
#-------------------------------------------------
margenes <- data_merged %>%
  mutate(
    factor       = precio_ipc / precio_sipsa,
    margen       = (factor - 1) * 100,
    fecha        = as.Date(paste(Year, Month, "01", sep = "-")),
    ln_retail    = log(precio_ipc),
    ln_wholesale = log(precio_sipsa),
    mes_factor   = factor(Month)  # dummies mensuales
  ) %>%
  # trimming opcional de márgenes extremos
  filter(
    !is.na(ln_retail),
    !is.na(ln_wholesale),
    margen > 0,
    margen < 300
  )

# A partir de aquí, TODO es "food by food" = por articulo_ipc (IPC)

#-------------------------------------------------
# 6. Estimar modelos por alimento IPC (articulo_ipc)
#    ln_retail ~ ln_wholesale + dummies de mes
#-------------------------------------------------
modelos_tbl <- margenes %>%
  group_by(alimento_sipsa) %>%
  nest() %>%
  mutate(
    modelo   = map(data, ~ lm(ln_retail ~ ln_wholesale + mes_factor, data = .x)),
    coefs    = map(modelo, tidy),
    resumen  = map(modelo, glance),
    ajustado = map2(modelo, data, ~ augment(.x, newdata = .y))
  )

#-------------------------------------------------
# 7. Tabla de coeficientes (incluye elasticidad beta)
#-------------------------------------------------
coefs_tbl <- modelos_tbl %>%
  select(alimento_sipsa, coefs) %>%
  unnest(coefs) %>%
  mutate(
    ci_lo = estimate - 1.96 * std.error,
    ci_hi = estimate + 1.96 * std.error
  )

elasticidades_tbl <- coefs_tbl %>%
  filter(term == "ln_wholesale") %>%
  rename(
    beta       = estimate,
    beta_se    = std.error,
    beta_ci_lo = ci_lo,
    beta_ci_hi = ci_hi
  ) %>%
  select(alimento_sipsa,
         beta, beta_se, beta_ci_lo, beta_ci_hi, statistic, p.value)

#-------------------------------------------------
# 8. Tabla con medidas de ajuste por modelo (R2, etc.)
#-------------------------------------------------
resumen_tbl <- modelos_tbl %>%
  dplyr::select(coefs, resumen) %>%
  unnest(resumen)

#-------------------------------------------------
# 9. Datos con predicciones para gráficos
#-------------------------------------------------
ajustado_tbl <- modelos_tbl %>%
  select(alimento_sipsa, ajustado) %>%
  unnest(ajustado)  # contiene ln_retail, .fitted, .resid, Month, fecha, etc.

#-------------------------------------------------
# 10. Guardar resultados numéricos
#-------------------------------------------------
dir_out_tablas <- "estimadores-modelo/output/margen"
if (!dir.exists(dir_out_tablas)) dir.create(dir_out_tablas, recursive = TRUE)

readr::write_csv(
  coefs_tbl,
  file.path(dir_out_tablas,
            "CALI_111225_coeficientes_modelos_por_articuloIPC.csv")
)

readr::write_csv(
  elasticidades_tbl,
  file.path(dir_out_tablas,
            "CALI_111225_elasticidades_lnW_lnR_por_articuloIPC.csv")
)

readr::write_csv(
  resumen_tbl,
  file.path(dir_out_tablas,
            "CALI_111225_resumen_ajuste_modelos_por_articuloIPC.csv")
)

#-------------------------------------------------
# 11. Directorio para gráficos de validación
#-------------------------------------------------
plot_dir <- "estimadores-modelo/output/margen/plots_modelos_CALI_111225"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

#-------------------------------------------------
# 12. Función de gráfico:
#     serie temporal ln_retail observado vs predicho
#     UN png por articulo_ipc
#-------------------------------------------------
plot_modelo_articulo <- function(df_art, food, output_dir) {
  
  art_name <- unique(df_art$articulo_ipc)
  # por info, qué wholesale(s) entran:
  wh_names <- unique(df_art$alimento_sipsa)
  wh_label <- paste(wh_names, collapse = ", ")
  
  titulo <- paste0("Cali - Modelo por alimento IPC\n",
                   art_name)
  subtit <- paste0("Mayoristas usados (SIPSA): ", wh_label,
                   "\nln(precio IPC) ~ ln(precio SIPSA) + dummies mensuales")
  
  p <- ggplot(df_art, aes(x = fecha)) +
    geom_line(aes(y = ln_retail, colour = "Observado"), linewidth = 0.7) +
    geom_line(aes(y = .fitted,  colour = "Predicho"),  linewidth = 0.7,
              linetype = "dashed") +
    labs(
      title   = titulo,
      subtitle = subtit,
      x = "Fecha",
      y = "Log precio retail (IPC)",
      colour = ""
    ) +
    theme_minimal()
  
  # Un nombre de archivo por articulo_ipc (food item)
  safe_art <- gsub("[^A-Za-z0-9_]+", "_", art_name)
  out_file <- file.path(
    output_dir,
    paste0("CALI_modelo_lnR_lnW_", safe_art, ".png")
  )
  
  ggsave(
    filename = out_file,
    plot     = p,
    width    = 8,
    height   = 5,
    dpi      = 300
  )
}

#-------------------------------------------------
# 13. Generar y guardar gráficos alimento por alimento (IPC)
#-------------------------------------------------
ajustado_tbl %>%
  group_by(alimento_sipsa) %>%
  group_walk(~ plot_modelo_articulo(.x, plot_dir))

#-------------------------------------------------
# FIN
#-------------------------------------------------
