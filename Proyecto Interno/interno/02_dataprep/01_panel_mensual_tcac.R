########################################################
## SCRIPT 02_dataprep/01_panel_mensual_tcac.R
## Toma el panel balanceado (panel_balanceado, diario, ya
## filtrado por cobertura de dias en 02a/02b, con precio_500g
## calculado desde 01_construccion_panel.R), lo agrega a nivel
## MENSUAL (mediana entre las fechas de scraping del mes) y lo
## cruza con la composicion nutricional TCAC (output de
## 00_tcac_composicion.R) para calcular precio por 100g de
## porcion comestible.
##
## Reads:  output_dir/paneles/panel_balanceado.rds
##         output_dir/tcac/composicion_270726.rds
## Writes: output_dir/paneles/panel_mensual_cities_tcac.rds
##         (1 fila por alimento-ciudad-mes)
########################################################

library(tidyverse)

# ============================================================
# Rutas
# ============================================================

proyecto_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
interno_dir  <- file.path(proyecto_dir, "interno")
output_dir   <- file.path(interno_dir, "output")

ruta_panel_v2 <- file.path(output_dir, "paneles/panel_balanceado.rds")
ruta_tcac     <- file.path(output_dir, "tcac/composicion_270726.rds")

# ============================================================
# Cargar insumos
# ============================================================

panel_v2 <- readRDS(ruta_panel_v2)
tcac_raw <- readRDS(ruta_tcac)

# ============================================================
# Agregar a nivel mensual: mediana de precio_500g entre todas
# las fechas de scraping del mes, por alimento-ciudad. El
# filtro de cobertura de 02a ya se aplicó a nivel diario, así
# que esto solo agrega -- no vuelve a filtrar nada.
# ============================================================

panel_mensual <- panel_v2 %>%
  mutate(mes_ym = format(fecha, "%Y-%m")) %>%
  group_by(sipsa_name, city, mes_ym) %>%
  summarise(
    precio_500g  = round(median(precio_500g, na.rm = TRUE), 0),
    n_fechas_mes = n_distinct(fecha),
    .groups = "drop"
  ) %>%
  mutate(fecha = as.Date(paste0(mes_ym, "-01"))) %>%
  select(-mes_ym)

# =========================================================================
# Limpiar TCAC
# parte_comestible_percent se divide entre 100 para que quede entre 0 y 1
# Se colapsa a una sola fila por sipsa_name: como composicion_270726.rds
# se construyo mapeando muchos sku_code al mismo nombre normalizado de
# TCAC, todas las filas de un mismo alimento ya comparten los mismos
# valores nutricionales, asi que cualquiera de ellas sirve como unica.
# =========================================================================

tcac <- tcac_raw %>%
  distinct(sipsa_name, .keep_all = TRUE) %>%
  select(
    sipsa_name, codigo_tcac, parte_analizada,
    humedad_g, energia_kcal, proteina_g, lipidos_g,
    carbohidratos_totales_g, fibra_dietaria_g, cenizas_g,
    calcio_mg, hierro_mg, sodio_mg, fosforo_mg, zinc_mg,
    magnesio_mg, potasio_mg, tiamina_mg, riboflavina_mg,
    niacina_mg, folatos_mcg, vitamina_b12_mcg, vitamina_c_mg,
    vitamina_a_er, parte_comestible_percent,
    grupos_gabas, subgrupos_gabas, grupo_tcac, factor_de_conversion,
    imputacion_complemento,
    any_of("gramos_g_1_intercambio_1_intercambio")
  ) %>%
  mutate(
    parte_comestible = parte_comestible_percent / 100
  )

# =======================================================================
# Join con TCAC (por sipsa_name) y calcular precio por 100g porcion
# comestible. Formula: P_500g / (5 * pc)
# =======================================================================

panel_con_tcac <- panel_mensual %>%
  left_join(tcac, by = "sipsa_name") %>%
  mutate(
    precio_100g = round(precio_500g / (5 * parte_comestible), 0)
  )

# ==========================
# Construcción panel final
# ==========================

panel_mensual_cities_tcac <- panel_con_tcac %>%
  rename(
    articulo = sipsa_name,
    ciudad   = city
  ) %>%
  select(
    articulo, ciudad, fecha,
    precio_500g, precio_100g,
    codigo_tcac, humedad_g, energia_kcal, proteina_g, lipidos_g,
    carbohidratos_totales_g, fibra_dietaria_g, cenizas_g,
    calcio_mg, hierro_mg, sodio_mg, fosforo_mg, zinc_mg,
    magnesio_mg, potasio_mg, tiamina_mg, riboflavina_mg,
    niacina_mg, folatos_mcg, vitamina_b12_mcg, vitamina_c_mg,
    vitamina_a_er, parte_comestible_percent, parte_comestible,
    any_of("gramos_g_1_intercambio_1_intercambio"),
    grupos_gabas, subgrupos_gabas, grupo_tcac, factor_de_conversion
  )

# ===============
# Guardar base
# ===============

saveRDS(
  panel_mensual_cities_tcac,
  file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")
)

cat("\nListo. panel_mensual_cities_tcac.rds guardado en", output_dir, "\n")
