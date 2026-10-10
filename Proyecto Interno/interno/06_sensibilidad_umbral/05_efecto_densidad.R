########################################################
## SCRIPT 06_sensibilidad_umbral/05_efecto_densidad.R
## Comprueba que los aceites entran bien a los modelos y mide cuánto
## habría pesado el error de unidades de la v1, donde 1 ml se trataba
## como 1 g y el precio de 1.000 ml se dividía entre 5 como si fueran
## 500 g (el precio por 100 g de los aceites quedaba 2 x 0,92 = 1,84
## veces por encima del correcto). Ahora los líquidos se estandarizan a
## 1.000 ml y se pasan a gramos con su densidad.
##
## 1. Validación: para los tres aceites de Bogotá en julio, el precio
##    por 100 g del panel contra el que sale a mano desde el precio del
##    scraper, sus gramos y su parte comestible.
## 2. Efecto: corre los cuatro modelos con el precio de los aceites
##    multiplicado por 1,84 y lo compara con los resultados actuales.
##
## Reads:  panel_dir/data/julio.rds
##         output_dir/lista_alimentos/lista_alimentos.xlsx
##         output_dir/paneles/panel_mensual_cities_tcac.rds
##         03_models/ (resultados del proyecto)
## Writes: sens_dir/error_densidad/ (resultados de cada modelo)
##         sens_dir/densidad.rds
########################################################

library(tidyverse)
library(lubridate)
library(openxlsx)

dir_proyecto <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
dir_interno  <- file.path(dir_proyecto, "interno/")

dir_salida <- file.path(dir_interno, "output/sensibilidad_umbral/error_densidad")
dir.create(dir_salida, showWarnings = FALSE)

source(file.path(dir_interno, "06_sensibilidad_umbral/aux-functions/correr_con_cambios.R"))

aceites <- c("Aceite girasol", "Aceite soya", "Aceite vegetal mezcla")
factor_error <- 2 * 0.92

panel <- readRDS(file.path(dir_interno, "output/paneles/panel_mensual_cities_tcac.rds"))

# ============================================================
# 1. Validación del precio por 100 g
# ============================================================

elegidos <- read.xlsx(file.path(dir_interno, "output/lista_alimentos/lista_alimentos.xlsx")) %>%
  filter(sipsa_name %in% aceites, city == "Bogotá") %>%
  mutate(sku_code = as.character(sku_code)) %>%
  select(sipsa_name, sku_code, exito_name, gramos_sku)

scraper <- readRDS(file.path(dir_interno, "01_webscrap_prep/data/julio.rds")) %>%
  filter(sipsa_name %in% aceites, str_detect(str_to_lower(city), "^bogot")) %>%
  transmute(
    sipsa_name = as.character(sipsa_name),
    sku_code   = as.character(sku_code),
    fecha      = dmy(str_extract(archivo_origen, "\\d{2}-\\d{2}-\\d{4}")),
    precio     = parse_number(as.character(price))
  ) %>%
  filter(precio > 0) %>%
  semi_join(elegidos, by = c("sipsa_name", "sku_code")) %>%
  group_by(sipsa_name, sku_code, fecha) %>%
  summarise(precio = median(precio), .groups = "drop") %>%
  group_by(sipsa_name, sku_code) %>%
  summarise(precio_scraper = median(precio), .groups = "drop")

validacion <- elegidos %>%
  left_join(scraper, by = c("sipsa_name", "sku_code")) %>%
  left_join(
    panel %>%
      filter(ciudad == "Bogotá", fecha == as.Date("2025-07-01"), articulo %in% aceites) %>%
      select(sipsa_name = articulo, precio_base, objetivo_base, precio_100g, parte_comestible),
    by = "sipsa_name"
  ) %>%
  mutate(
    ml                 = gramos_sku / 0.92,
    precio_100g_mano   = precio_scraper / gramos_sku * 100 / parte_comestible,
    precio_100g_error  = precio_scraper / ml * 1000 / 5 / parte_comestible
  )

# ============================================================
# 2. Modelos con el precio de los aceites de la v1
# ============================================================

ruta_panel_error <- file.path(tempdir(), "panel_mensual_error.rds")

panel %>%
  mutate(across(c(precio_base, precio_100g), ~ if_else(articulo %in% aceites, . * factor_error, .))) %>%
  saveRDS(ruta_panel_error)

en_panel <- 'file.path\\(output_dir, "paneles/panel_mensual_cities_tcac.rds"\\)'
etiqueta <- "error_densidad"

correr_con_cambios("03_models/01_coca.R", setNames(
  list("ruta_panel_error", "coca_dir <- dir_salida"), c(en_panel, "^coca_dir\\s*<-")), etiqueta)
correr_con_cambios("03_models/02_cona.R", setNames(
  list("ruta_panel_error", "cona_dir <- dir_salida"), c(en_panel, "^cona_dir\\s*<-")), etiqueta)
correr_con_cambios("03_models/03_cord.R", setNames(
  list("ruta_panel_error", "cord_dir <- dir_salida"), c(en_panel, "^cord_dir\\s*<-")), etiqueta)
correr_con_cambios("03_models/031_coahd.R", setNames(
  list("ruta_panel_error", "coahd_dir <- dir_salida"), c(en_panel, "^coahd_dir\\s*<-")), etiqueta)

# ============================================================
# Costos per capita y alimentos del CoNA con y sin el error
# ============================================================

percapita <- function(df) {
  df %>%
    group_by(ciudad, fecha) %>% summarise(hogar = sum(costo), .groups = "drop") %>%
    group_by(ciudad) %>% summarise(pc = mean(hogar) / 3, .groups = "drop")
}

leer_costos <- function(dir, modelo) {
  switch(modelo,
    CoCA  = readRDS(file.path(dir, "coca_results.rds")) %>%
      distinct(ciudad, fecha, Demo_Group, Sex, costo = cost_day),
    CoNA  = readRDS(file.path(dir, "cona_results.rds"))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, costo = cona_cost),
    CoRD  = readRDS(file.path(dir, "cord_results.rds"))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, costo = cost_day),
    CoAHD = readRDS(file.path(dir, "coahd_results.rds"))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, costo = cost_day)
  ) %>% mutate(fecha = as.Date(fecha))
}

modelos <- c("CoCA", "CoNA", "CoRD", "CoAHD")
dir_proyecto_modelos <- c(CoCA = "coca", CoNA = "cona", CoRD = "cord", CoAHD = "coahd")

costos <- map_dfr(modelos, function(m) {
  inner_join(
    percapita(leer_costos(file.path(dir_interno, "03_models", dir_proyecto_modelos[[m]]), m)),
    percapita(leer_costos(dir_salida, m)),
    by = "ciudad", suffix = c("_correcto", "_error")
  ) %>% mutate(modelo = m)
})

aceites_en_cona <- function(dir) {
  readRDS(file.path(dir, "cona_results.rds"))$comp %>%
    filter(quantity > 0, Food %in% aceites) %>%
    distinct(ciudad, Food) %>% count(Food, name = "ciudades")
}

saveRDS(list(
  validacion = validacion, costos = costos, factor_error = factor_error,
  aceites_correcto = aceites_en_cona(file.path(dir_interno, "03_models/cona")),
  aceites_error = aceites_en_cona(dir_salida)
), file.path(dir_interno, "output/sensibilidad_umbral/densidad.rds"))
