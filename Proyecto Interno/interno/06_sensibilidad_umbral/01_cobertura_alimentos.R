########################################################
## SCRIPT 06_sensibilidad_umbral/01_cobertura_alimentos.R
## Cobertura de fechas de cada alimento en cada ciudad, antes de
## aplicar el umbral, y qué alimentos pasarían con cada regla:
##   actual     al menos dos fechas con precio en cada mes (02a)
##   grupo85    85% de los días en que el grupo del alimento tiene datos
##   base       85% de los días de la ciudad
##
## Reads:  panel_dir/panel_v2.rds (panel filtrado por 02a)
##         proyecto_dir/composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx
## Writes: sens_dir/cobertura.rds
########################################################

library(tidyverse)
library(openxlsx)
library(stringi)
library(janitor)

proyecto_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
base_dir     <- file.path(proyecto_dir, "interno/")

panel_dir <- file.path(base_dir, "output/paneles/raw_mensual")
sens_dir  <- file.path(base_dir, "output/sensibilidad_umbral")
ruta_tcac <- file.path(proyecto_dir, "composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx")

dir.create(sens_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(base_dir, "01_webscrap_prep/aux-functions/mapeo_tcac.R"), encoding = "UTF-8")

panel <- readRDS(file.path(panel_dir, "panel_v2.rds")) %>%
  mutate(fecha = as.Date(fecha), mes = format(fecha, "%Y-%m"))

grupos <- unir_mapeo_tcac(distinct(panel, sipsa_name), leer_mapeo_tcac(ruta_tcac)) %>%
  clean_names() %>%
  transmute(
    sipsa_name,
    grupo = case_when(
      subgrupos_gabas %in% c("FRUTAS", "VERDURAS") ~ subgrupos_gabas,
      grupos_gabas == "SIN CATEGORIA"              ~ NA_character_,
      TRUE                                         ~ grupos_gabas
    )
  )

ciudad <- panel %>%
  group_by(city) %>%
  summarise(dias = n_distinct(fecha), .groups = "drop")

dias_grupo <- panel %>%
  left_join(grupos, by = "sipsa_name") %>%
  filter(!is.na(grupo)) %>%
  group_by(city, grupo) %>%
  summarise(dias_grupo = n_distinct(fecha), .groups = "drop")

meses_ciudad <- panel %>% distinct(city, mes)

dias_mes_min <- panel %>%
  distinct(city, sipsa_name, fecha, mes) %>%
  count(city, sipsa_name, mes) %>%
  full_join(
    distinct(panel, city, sipsa_name) %>% inner_join(meses_ciudad, by = "city", relationship = "many-to-many"),
    by = c("city", "sipsa_name", "mes")
  ) %>%
  mutate(n = coalesce(n, 0L)) %>%
  group_by(city, sipsa_name) %>%
  summarise(dias_mes_min = min(n), .groups = "drop")

cobertura <- panel %>%
  group_by(city, sipsa_name) %>%
  summarise(n_dias = n_distinct(fecha), n_meses = n_distinct(mes), .groups = "drop") %>%
  left_join(dias_mes_min, by = c("city", "sipsa_name")) %>%
  left_join(ciudad, by = "city") %>%
  left_join(grupos, by = "sipsa_name") %>%
  left_join(dias_grupo, by = c("city", "grupo")) %>%
  mutate(
    es_fruta     = grupo %in% "FRUTAS",
    pasa_actual  = dias_mes_min >= 2,
    pasa_grupo85 = n_dias >= floor(0.85 * coalesce(dias_grupo, dias)),
    pasa_base    = n_dias >= floor(0.85 * dias)
  )

saveRDS(list(cobertura = cobertura, ciudad = ciudad), file.path(sens_dir, "cobertura.rds"))

message(sprintf("Alimentos distintos: %d | pares alimento-ciudad vistos: %d",
                n_distinct(cobertura$sipsa_name), nrow(cobertura)))
message("Pares que pasan, por regla:")
print(colSums(select(cobertura, starts_with("pasa_"))))
