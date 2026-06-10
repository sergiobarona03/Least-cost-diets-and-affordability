# ============================================================
# Panel mensual de precios estandarizados + TCAC
# ============================================================

# =============
# Librerías
# =============

library(tidyverse)
library(openxlsx)

# =======
# Rutas
# =======

panel_v2 <- readRDS("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/output/paneles/panel_v2.rds")
lista_representativa <- read.xlsx("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/output/lista alimentos/lista_alimentos.xlsx")
tcac_raw <- readRDS("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/output/tcac/composicion_310526.rds")

# ===================================================================
# Extraer referencia de cantidad desde lista representativa
# ===================================================================

referencias <- lista_representativa %>%
  dplyr::select(sipsa_name, city, sku_code, exito_name, cantidad_extraida, objetivo) %>%
  dplyr::rename(objetivo_texto = objetivo)

# ============================================================
# Precio mediana mensual por alimento-ciudad-mes
# ============================================================

panel_mensual <- panel_v2 %>%
  group_by(sipsa_name, city, sku_code, measurement_unit, tcac_code, mes) %>%
  dplyr::summarise(
    price = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(mes_fmt = mes)

# ============================================================
# Pegar cantidad y objetivo de la presentación representativa
# ============================================================

panel_con_cantidad <- panel_mensual %>%
  left_join(referencias, by = c("sipsa_name", "city", "sku_code"))

# ============================================================
# Lista de alimentos en unidades
# ============================================================

lista_unidades <- panel_con_cantidad %>%
  filter(is.na(objetivo_texto) & str_to_lower(measurement_unit) == "unidad") %>%
  distinct(sipsa_name, city, price, measurement_unit) %>%
  arrange(sipsa_name, city)

write.xlsx(
  lista_unidades,
  file = "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/output/lista alimentos/lista_unidades.xlsx",
  overwrite = TRUE
)

# ============================================================
# Estandarizar precio a 500g o 1000ml
# Productos con objetivo_texto: fórmula (P / cantidad_extraida) * objetivo
# Productos vendidos por kg o g sin empaque: (price / 1000) * 500
# ============================================================

panel_estandar <- panel_con_cantidad %>%
  mutate(
    objetivo = case_when(
      objetivo_texto == "500 gramos"      ~ 500,
      objetivo_texto == "1000 mililitros" ~ 1000,
      TRUE ~ NA_real_
    ),
    precio_500g = case_when(
      !is.na(objetivo_texto)                                          ~ (price / cantidad_extraida) * objetivo,
      str_to_lower(measurement_unit) %in% c("kilogramo", "gramo")    ~ (price / 1000) * 500,
      TRUE ~ NA_real_
    )
  )

# =========================================================================
# Limpiar TCAC
# parte_comestible_percent se divide entre 100 para que quede entre 0 y 1
# =========================================================================

tcac <- tcac_raw %>%
  select(
    sku_code, sipsa_name, codigo_tcac, parte_analizada,
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
    precio_100g = precio_500g / (5 * parte_comestible)
  )

# ==========================
# Construcción panel final
# ==========================

panel_mensual_cities_tcac <- panel_con_tcac %>%
  mutate(
    fecha = as.Date(paste0(mes_fmt, "-01"))
  ) %>%
  rename(
    articulo = sipsa_name.x,
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

glimpse(panel_mensual_cities_tcac)

saveRDS(
  panel_mensual_cities_tcac,
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/output/paneles/panel_mensual_cities_tcac.rds"
)

View(panel_con_cantidad %>%
       filter(is.na(objetivo_texto) | !objetivo_texto %in% c("500 gramos", "1000 mililitros")) %>%
       distinct(sipsa_name, objetivo_texto, exito_name) %>%
       arrange(sipsa_name))
