########################################################
## SCRIPT 05_sensibilidad_umbral/01_cobertura_alimentos.R
## Cobertura de fechas de cada alimento en cada ciudad, antes de
## aplicar el umbral, y qué alimentos pasarían con cada regla:
##   actual     85% de los días del grupo en la ciudad (02a)
##   base       85% de los días de la ciudad (regla anterior)
##   fruta75    base, con las frutas al 75%
##   fruta80    base, con las frutas al 80%
##   semanal    al menos tantos días con dato como semanas con datos
##   quincenal  al menos la mitad de esos días
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

semana_iso <- function(f) as.integer(format(f, "%G")) * 100L + as.integer(format(f, "%V"))
panel <- mutate(panel, semana = semana_iso(fecha))

ciudad <- panel %>%
  group_by(city) %>%
  summarise(dias = n_distinct(fecha), semanas = n_distinct(semana), .groups = "drop")

dias_grupo <- panel %>%
  left_join(grupos, by = "sipsa_name") %>%
  filter(!is.na(grupo)) %>%
  group_by(city, grupo) %>%
  summarise(dias_grupo = n_distinct(fecha), .groups = "drop")

cobertura <- panel %>%
  group_by(city, sipsa_name) %>%
  summarise(n_dias = n_distinct(fecha), n_meses = n_distinct(mes), .groups = "drop") %>%
  left_join(ciudad, by = "city") %>%
  left_join(grupos, by = "sipsa_name") %>%
  left_join(dias_grupo, by = c("city", "grupo")) %>%
  mutate(
    es_fruta       = grupo %in% "FRUTAS",
    pasa_actual    = n_dias >= floor(0.85 * coalesce(dias_grupo, dias)),
    pasa_base      = n_dias >= floor(0.85 * dias),
    pasa_fruta75   = n_dias >= if_else(es_fruta, floor(0.75 * dias), floor(0.85 * dias)),
    pasa_fruta80   = n_dias >= if_else(es_fruta, floor(0.80 * dias), floor(0.85 * dias)),
    pasa_semanal   = n_dias >= semanas,
    pasa_quincenal = n_dias >= ceiling(semanas / 2)
  )

saveRDS(list(cobertura = cobertura, ciudad = ciudad), file.path(sens_dir, "cobertura.rds"))

message(sprintf("Alimentos distintos: %d | pares alimento-ciudad vistos: %d",
                n_distinct(cobertura$sipsa_name), nrow(cobertura)))
message("Pares que pasan, por regla:")
print(colSums(select(cobertura, starts_with("pasa_"))))
