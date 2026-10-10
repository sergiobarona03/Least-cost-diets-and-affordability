########################################################
## SCRIPT 06_sensibilidad_umbral/06_aceites_sin_umbral.R
## Prueba si el precio de los aceites está inflado por el umbral de
## tamaño de 500 g: corre los cuatro modelos con el panel del
## proyecto, cambiando solo el precio de los tres aceites por el que
## sale sin umbral de tamaño (04_sin_umbral_tamano.R). Los demás
## alimentos quedan igual.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         sens_dir/sin_umbral_tamano/insumos/paneles/panel_mensual_cities_tcac.rds
##         03_models/ (resultados del proyecto)
## Writes: sens_dir/aceites_sin_umbral/ (resultados de cada modelo)
##         sens_dir/aceites.rds
########################################################

library(tidyverse)

dir_proyecto <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
dir_interno  <- file.path(dir_proyecto, "interno/")
dir_sens     <- file.path(dir_interno, "output/sensibilidad_umbral")

dir_salida <- file.path(dir_sens, "aceites_sin_umbral")
dir.create(dir_salida, showWarnings = FALSE)

source(file.path(dir_interno, "06_sensibilidad_umbral/aux-functions/correr_con_cambios.R"))

aceites <- c("Aceite girasol", "Aceite soya", "Aceite vegetal mezcla")

panel_actual <- readRDS(file.path(dir_interno, "output/paneles/panel_mensual_cities_tcac.rds"))
panel_sin    <- readRDS(file.path(dir_sens, "sin_umbral_tamano/insumos/paneles/panel_mensual_cities_tcac.rds"))

precios_sin <- panel_sin %>%
  filter(articulo %in% aceites) %>%
  select(articulo, ciudad, fecha, precio_base_sin = precio_base, precio_100g_sin = precio_100g)

ruta_panel <- file.path(tempdir(), "panel_mensual_aceites.rds")

panel_actual %>%
  left_join(precios_sin, by = c("articulo", "ciudad", "fecha")) %>%
  mutate(
    precio_base = coalesce(precio_base_sin, precio_base),
    precio_100g = coalesce(precio_100g_sin, precio_100g)
  ) %>%
  select(-precio_base_sin, -precio_100g_sin) %>%
  saveRDS(ruta_panel)

en_panel <- 'file.path\\(output_dir, "paneles/panel_mensual_cities_tcac.rds"\\)'
etiqueta <- "aceites_sin_umbral"

correr_con_cambios("03_models/01_coca.R", setNames(
  list("ruta_panel", "coca_dir <- dir_salida"), c(en_panel, "^coca_dir\\s*<-")), etiqueta)
correr_con_cambios("03_models/02_cona.R", setNames(
  list("ruta_panel", "cona_dir <- dir_salida"), c(en_panel, "^cona_dir\\s*<-")), etiqueta)
correr_con_cambios("03_models/03_cord.R", setNames(
  list("ruta_panel", "cord_dir <- dir_salida"), c(en_panel, "^cord_dir\\s*<-")), etiqueta)
correr_con_cambios("03_models/031_coahd.R", setNames(
  list("ruta_panel", "coahd_dir <- dir_salida"), c(en_panel, "^coahd_dir\\s*<-")), etiqueta)

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
dir_modelos <- c(CoCA = "coca", CoNA = "cona", CoRD = "cord", CoAHD = "coahd")

costos <- map_dfr(modelos, function(m) {
  inner_join(
    percapita(leer_costos(file.path(dir_interno, "03_models", dir_modelos[[m]]), m)),
    percapita(leer_costos(dir_salida, m)),
    by = "ciudad", suffix = c("_actual", "_aceites")
  ) %>% mutate(modelo = m)
})

aceites_en_cona <- readRDS(file.path(dir_salida, "cona_results.rds"))$comp %>%
  filter(quantity > 0, Food %in% aceites) %>%
  distinct(ciudad, Food) %>% count(Food, name = "ciudades")

saveRDS(list(costos = costos, aceites_en_cona = aceites_en_cona), file.path(dir_sens, "aceites.rds"))
