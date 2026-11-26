########################################################
## SCRIPT 02_dataprep/01_composicion_panel_mensual.R
##
## Reune lo que antes hacian 00_tcac_composicion.R,
## 01_panel_mensual_tcac.R y aux-functions/mapeo_tcac.R:
##   1. Cruza la lista de alimentos del panel con el mapeo SIPSA-TCAC
##      (composicion nutricional, parte comestible y grupo GABA).
##   2. Lleva el panel diario a mensual: mediana del precio_base entre
##      las fechas de recoleccion de cada mes, por alimento y ciudad.
##   3. Calcula el precio por 100 g de parte comestible:
##      precio_base / (objetivo_base * densidad / 100 * parte_comestible),
##      que en los solidos es P_500g / (5 * pc).
##
## Reads:  output_dir/lista_alimentos/lista_total_alimentos.xlsx
##         output_dir/paneles/panel_balanceado.rds
##         ruta_mapeo (Mapeo Sipsa TCAC _28.07.26.xlsx, hoja "Imputada")
## Writes: output_dir/tcac/composicion_270726.rds
##         output_dir/tcac/composicion_270726.xlsx
##         output_dir/paneles/panel_mensual_cities_tcac.rds
##           (una fila por alimento-ciudad-mes)
########################################################

library(tidyverse)
library(openxlsx)
library(stringi)
library(janitor)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/paper-global-food"

output_dir <- file.path(base_dir, "output")
tcac_dir   <- file.path(output_dir, "tcac")
ruta_mapeo <- file.path(base_dir, "02_dataprep/composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx")

dir.create(tcac_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Paso 1. Composicion nutricional de cada alimento
# ============================================================

normalizar_nombre <- function(x) {
  x %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_squish()
}

mapeo <- read.xlsx(ruta_mapeo, sheet = "Imputada") %>%
  rename(sipsa_name = `Alimento.(Nombre.sipsa)`) %>%
  mutate(sipsa_name = str_squish(as.character(sipsa_name)),
         sipsa_name_norm = normalizar_nombre(sipsa_name))

# Un alimento repetido en el mapeo con dos filas distintas se resolveria
# tomando la primera sin aviso: se listan para revisarlos.
repetidos <- mapeo %>%
  count(sipsa_name_norm) %>%
  filter(n > 1)

if (nrow(repetidos) > 0) {
  message("Alimentos repetidos en el mapeo (se toma la primera fila): ",
          paste(repetidos$sipsa_name_norm, collapse = ", "))
}

mapeo_unico <- mapeo %>%
  distinct(sipsa_name_norm, .keep_all = TRUE) %>%
  select(-sipsa_name)

lista_total <- read.xlsx(file.path(output_dir, "lista_alimentos/lista_total_alimentos.xlsx"))

# Alimentos que el mapeo nombra distinto
composicion <- lista_total %>%
  mutate(
    sipsa_name_join = case_when(
      sipsa_name == "Ajo importado"                    ~ "Ajo",
      sipsa_name == "Almejas con concha"               ~ "Almejas",
      sipsa_name == "Bagre rayado en postas congelado" ~ "Bagre rayado",
      sipsa_name == "Carne de cerdo, lomo sin hueso"   ~ "Carne de cerdo, lomo",
      sipsa_name == "Carne de cerdo, pernil sin hueso" ~ "Carne de cerdo, lomo",  # mapeo dudoso: el pernil toma la composicion del lomo
      sipsa_name == "Trucha en corte mariposa"         ~ "Trucha",
      sipsa_name == "Uva roja"                         ~ "Uva comun",
      sipsa_name == "Yuca ICA"                         ~ "Yuca",
      TRUE ~ sipsa_name
    ),
    sipsa_name_norm = normalizar_nombre(sipsa_name_join)
  ) %>%
  left_join(mapeo_unico, by = "sipsa_name_norm") %>%
  select(-sipsa_name_norm, -sipsa_name_join) %>%
  relocate(sipsa_name) %>%
  clean_names()

saveRDS(composicion, file.path(tcac_dir, "composicion_270726.rds"))
write.xlsx(composicion, file.path(tcac_dir, "composicion_270726.xlsx"), overwrite = TRUE)

# ============================================================
# Paso 2. Panel mensual
# ============================================================
# El filtro de cobertura de fechas ya se aplico en 01_panel_precios.R:
# aqui solo se agrega, sin volver a filtrar.

panel_mensual <- readRDS(file.path(output_dir, "paneles/panel_balanceado.rds")) %>%
  mutate(mes_ym = format(fecha, "%Y-%m")) %>%
  group_by(sipsa_name, city, mes_ym) %>%
  summarise(
    precio_base   = round(median(precio_base, na.rm = TRUE), 0),
    objetivo_base = first(objetivo_base),
    densidad      = first(densidad),
    n_fechas_mes  = n_distinct(fecha),
    .groups = "drop"
  ) %>%
  mutate(fecha = as.Date(paste0(mes_ym, "-01"))) %>%
  select(-mes_ym)

# ============================================================
# Paso 3. Composicion y precio por 100 g de parte comestible
# ============================================================

# Una fila por alimento: todas las filas de un mismo alimento comparten
# la composicion del codigo TCAC que le asigna el mapeo.
tcac <- composicion %>%
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
  mutate(parte_comestible = parte_comestible_percent / 100)

panel_con_tcac <- panel_mensual %>%
  left_join(tcac, by = "sipsa_name") %>%
  mutate(precio_100g = round(precio_base / (objetivo_base * densidad / 100 * parte_comestible), 0))

# Alimentos del panel sin composicion, sin parte comestible o sin grupo
# GABA: se quedarian sin precio por 100 g o fuera de los modelos.
sin_composicion <- panel_con_tcac %>%
  filter(is.na(energia_kcal) | is.na(parte_comestible) | parte_comestible <= 0 |
           is.na(grupos_gabas)) %>%
  distinct(sipsa_name, energia_kcal, parte_comestible, grupos_gabas)

if (nrow(sin_composicion) > 0) {
  message("Alimentos del panel con composicion incompleta:")
  print(sin_composicion, n = Inf)
}

# ============================================================
# Panel final
# ============================================================

panel_mensual_cities_tcac <- panel_con_tcac %>%
  rename(articulo = sipsa_name, ciudad = city) %>%
  select(
    articulo, ciudad, fecha, n_fechas_mes,
    precio_base, objetivo_base, densidad, precio_100g,
    codigo_tcac, humedad_g, energia_kcal, proteina_g, lipidos_g,
    carbohidratos_totales_g, fibra_dietaria_g, cenizas_g,
    calcio_mg, hierro_mg, sodio_mg, fosforo_mg, zinc_mg,
    magnesio_mg, potasio_mg, tiamina_mg, riboflavina_mg,
    niacina_mg, folatos_mcg, vitamina_b12_mcg, vitamina_c_mg,
    vitamina_a_er, parte_comestible_percent, parte_comestible,
    any_of("gramos_g_1_intercambio_1_intercambio"),
    grupos_gabas, subgrupos_gabas, grupo_tcac, factor_de_conversion
  )

saveRDS(panel_mensual_cities_tcac, file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds"))

message(sprintf("Done. %d alimentos | %d ciudades | %d filas alimento-ciudad-mes",
                n_distinct(panel_mensual_cities_tcac$articulo),
                n_distinct(panel_mensual_cities_tcac$ciudad),
                nrow(panel_mensual_cities_tcac)))
