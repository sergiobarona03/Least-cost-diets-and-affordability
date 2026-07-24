########################################################
## SCRIPT 02_dataprep/01_panel_mensual_tcac.R
## Toma el panel balanceado (panel_balanceado), estandariza el precio a
## 500g/1000ml y lo cruza con la composición nutricional TCAC
## (output de 00_tcac_composicion.R) para calcular precio por
## 100g de porción comestible.
##
## Reads:  output_dir/paneles/panel_balanceado.rds
##         output_dir/lista_alimentos/lista_alimentos.xlsx
##         output_dir/tcac/composicion_270726.rds
##         dataprep_dir/unidades/lista_unidades gramos.xlsx
## Writes: output_dir/lista_alimentos/lista_unidades.xlsx
##         output_dir/paneles/panel_mensual_cities_tcac.rds
########################################################

library(tidyverse)
library(openxlsx)

# ============================================================
# Rutas
# ============================================================

proyecto_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
interno_dir  <- file.path(proyecto_dir, "interno")
output_dir   <- file.path(interno_dir, "output")

dataprep_dir <- file.path(interno_dir, "02_dataprep")

ruta_panel_v2      <- file.path(output_dir, "paneles/panel_balanceado.rds")
ruta_lista_alim    <- file.path(output_dir, "lista_alimentos/lista_alimentos.xlsx")
ruta_tcac          <- file.path(output_dir, "tcac/composicion_270726.rds")
ruta_gramos_unidad <- file.path(dataprep_dir, "unidades/lista_unidades gramos.xlsx")

# ============================================================
# Cargar insumos
# ============================================================

panel_v2 <- readRDS(ruta_panel_v2)
lista_representativa <- read.xlsx(ruta_lista_alim)
tcac_raw <- readRDS(ruta_tcac)

# Gramos por unidad para productos vendidos como "Unidad" sin objetivo_texto
gramos_unidad <- read.xlsx(ruta_gramos_unidad) %>%
  dplyr::select(sipsa_name, city, cantidad_gramos = `cantidad.aproximada.gramos`) %>%
  distinct(sipsa_name, city, cantidad_gramos)

# ===================================================================
# Extraer referencia de cantidad desde lista representativa
# ===================================================================

referencias <- lista_representativa %>%
  dplyr::select(sipsa_name, city, sku_code, exito_name, cantidad_extraida, objetivo) %>%
  dplyr::rename(objetivo_texto = objetivo)

# ============================================================
# Precio mediana mensual por alimento-ciudad-mes
# measurement_unit y tcac_code NO van en el group_by: si vienen NA en
# algunas fechas de scraping, agrupar por ellos crea filas "fantasma"
# separadas del mismo alimento con unidad válida. Se toma el valor
# no-NA representativo dentro de cada grupo en su lugar.
# ============================================================

panel_mensual <- panel_v2 %>%
  group_by(sipsa_name, city, sku_code, mes) %>%
  dplyr::summarise(
    price            = median(price, na.rm = TRUE),
    measurement_unit = measurement_unit[!is.na(measurement_unit)][1],
    tcac_code        = tcac_code[!is.na(tcac_code)][1],
    .groups = "drop"
  ) %>%
  rename(mes_fmt = mes)

# ============================================================
# Pegar cantidad y objetivo de la presentación representativa
# ============================================================

panel_con_cantidad <- panel_mensual %>%
  left_join(referencias, by = c("sipsa_name", "city", "sku_code")) %>%
  left_join(gramos_unidad, by = c("sipsa_name", "city")) %>%
  mutate(
    unidades_paquete = coalesce(
      as.numeric(str_extract(exito_name, "(?<=\\()\\d+(?=\\s*und\\))")),
      1
    )
  )

# ============================================================
# Lista de alimentos en unidades (se mantiene igual, por si
# aparecen productos nuevos sin cubrir en gramos_unidad)
# ============================================================

lista_unidades <- panel_con_cantidad %>%
  filter(is.na(objetivo_texto) & str_to_lower(measurement_unit) == "unidad" & is.na(cantidad_gramos)) %>%
  distinct(sipsa_name, city, price, measurement_unit) %>%
  arrange(sipsa_name, city)

write.xlsx(
  lista_unidades,
  file = file.path(output_dir, "lista_alimentos/lista_unidades.xlsx"),
  overwrite = TRUE
)

# ============================================================
# Estandarizar precio a 500g o 1000ml
# Productos con objetivo_texto: fórmula (P / cantidad_extraida) * objetivo
# Productos vendidos por kg o g sin empaque: (price / 1000) * 500
# Productos vendidos por unidad sin objetivo_texto: (price / (cantidad_gramos * unidades_paquete)) * 500
# ============================================================

panel_estandar <- panel_con_cantidad %>%
  mutate(
    objetivo = case_when(
      objetivo_texto == "500 gramos"      ~ 500,
      objetivo_texto == "1000 mililitros" ~ 1000,
      TRUE ~ NA_real_
    ),
    precio_500g = case_when(
      !is.na(objetivo_texto)                                       ~ (price / cantidad_extraida) * objetivo,
      str_to_lower(measurement_unit) %in% c("kilogramo", "gramo")  ~ (price / 1000) * 500,
      is.na(objetivo_texto) & str_to_lower(measurement_unit) == "unidad" & !is.na(cantidad_gramos) &
        str_detect(str_to_lower(sipsa_name), "huevo") ~
        (price / (cantidad_gramos * unidades_paquete)) * 500,
      is.na(objetivo_texto) & str_to_lower(measurement_unit) == "unidad" & !is.na(cantidad_gramos) ~
        (price / cantidad_gramos) * 500,
      TRUE ~ NA_real_
    )
  )

# =========================================================================
# Limpiar TCAC
# parte_comestible_percent se divide entre 100 para que quede entre 0 y 1
# =========================================================================

tcac <- tcac_raw %>%
  select(
    sku_code, codigo_tcac, parte_analizada,
    humedad_g, energia_kcal, proteina_g, lipidos_g,
    carbohidratos_totales_g, fibra_dietaria_g, cenizas_g,
    calcio_mg, hierro_mg, sodio_mg, fosforo_mg, zinc_mg,
    magnesio_mg, potasio_mg, tiamina_mg, riboflavina_mg,
    niacina_mg, folatos_mcg, vitamina_b12_mcg, vitamina_c_mg,
    vitamina_a_er, parte_comestible_percent,
    codigo2, micr_sin_inf_por_alimento, grupos_gabas,
    subgrupos_gabas, grupo_tcac, factor_de_conversion,
    imputacion_complemento
  ) %>%
  mutate(
    parte_comestible = parte_comestible_percent / 100
  )

# =======================================================================
# Join con TCAC y calcular precio por 100g porción comestible
# Fórmula: P_500g / (5 * pc)
# =======================================================================

panel_con_tcac <- panel_estandar %>%
  left_join(tcac, by = "sku_code") %>%
  mutate(
    precio_100g = precio_500g / (5 * parte_comestible),
    precio_500g = round(precio_500g, 0),
    precio_100g = round(precio_100g, 0)
  )

# ==========================
# Construcción panel final
# ==========================

panel_mensual_cities_tcac <- panel_con_tcac %>%
  mutate(
    fecha = as.Date(paste0(mes_fmt, "-01"))
  ) %>%
  rename(
    articulo = sipsa_name,
    ciudad   = city
  ) %>%
  select(
    articulo, ciudad, sku_code, exito_name, fecha,
    precio_500g, precio_100g,
    codigo_tcac, humedad_g, energia_kcal, proteina_g, lipidos_g,
    carbohidratos_totales_g, fibra_dietaria_g, cenizas_g,
    calcio_mg, hierro_mg, sodio_mg, fosforo_mg, zinc_mg,
    magnesio_mg, potasio_mg, tiamina_mg, riboflavina_mg,
    niacina_mg, folatos_mcg, vitamina_b12_mcg, vitamina_c_mg,
    vitamina_a_er, parte_comestible_percent, parte_comestible,
    grupos_gabas, subgrupos_gabas, grupo_tcac, factor_de_conversion
  )

# ===============
# Guardar base
# ===============

View(panel_mensual_cities_tcac)

saveRDS(
  panel_mensual_cities_tcac,
  file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")
)
