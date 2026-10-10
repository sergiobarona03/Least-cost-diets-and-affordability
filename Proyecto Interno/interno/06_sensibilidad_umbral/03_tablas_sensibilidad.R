########################################################
## SCRIPT 06_sensibilidad_umbral/03_tablas_sensibilidad.R
## Junta los resultados de 02_correr_regla.R (una carpeta por regla de
## fechas), de 04_sin_umbral_tamano.R y la cobertura de
## 01_cobertura_alimentos.R en las tablas que usa
## docs/sensibilidad_umbral_tamano.Rmd y docs/prueba_aceites.Rmd.
##
## Reads:  sens_dir/cobertura.rds
##         sens_dir/<regla>/ (resultados de cada modelo)
##         sens_dir/sin_umbral_tamano/ (resultados y paneles)
##         03_models/ y output/ (resultados del proyecto, para validar
##         "actual" y comparar el umbral de tamano)
## Writes: sens_dir/tablas.rds
########################################################

library(tidyverse)
library(openxlsx)

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
sens_dir <- file.path(base_dir, "output/sensibilidad_umbral")

reglas  <- c("actual", "grupo85", "base", "sin_umbral_tamano")
modelos <- c("CoCA", "CoNA", "CoRD", "CoAHD")

# El CC-CoNA se corre solo en la regla adoptada y sin umbral de tamano
source(file.path(base_dir, "03_models/aux-functions/alfa_referencia.R"))
reglas_ccona <- c("actual", "sin_umbral_tamano")

# ============================================================
# Costos de cada modelo (por miembro y mes)
# ============================================================

leer_costos <- function(dir, modelo) {
  switch(modelo,
    CoCA  = readRDS(file.path(dir, "coca_results.rds")) %>%
      distinct(ciudad, fecha, Demo_Group, Sex, costo = cost_day),
    CoNA  = readRDS(file.path(dir, "cona_results.rds"))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, costo = cona_cost),
    CoRD  = readRDS(file.path(dir, "cord_results.rds"))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, costo = cost_day),
    CoAHD = readRDS(file.path(dir, "coahd_results.rds"))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, costo = cost_day),
    "CC-CoNA" = readRDS(file.path(dir, "ccona_results.rds"))$cost %>%
      filter(alpha_val == ALFA_REFERENCIA) %>%
      select(ciudad, fecha, Demo_Group, Sex, costo = cost_day)
  ) %>% mutate(fecha = as.Date(fecha))
}

percapita <- function(df) {
  df %>%
    group_by(ciudad, fecha) %>% summarise(hogar = sum(costo), .groups = "drop") %>%
    group_by(ciudad) %>% summarise(pc = mean(hogar) / 3, .groups = "drop")
}

# La regla adoptada del CC-CoNA es el resultado del proyecto
dir_regla <- function(regla, modelo) {
  if (modelo == "CC-CoNA" && regla == "actual") file.path(base_dir, "03_models/ccona")
  else file.path(sens_dir, regla)
}

costos <- expand_grid(regla = reglas, modelo = c(modelos, "CC-CoNA")) %>%
  filter(modelo != "CC-CoNA" | regla %in% reglas_ccona) %>%
  mutate(pc = map2(regla, modelo, ~ percapita(leer_costos(dir_regla(.x, .y), .y)))) %>%
  unnest(pc)

# La regla "actual" debe reproducir los resultados del proyecto
dir_modelos <- c(CoCA = "coca", CoNA = "cona", CoRD = "cord", CoAHD = "coahd")
validacion <- map_dfr(modelos, function(m) {
  real <- leer_costos(file.path(base_dir, "03_models", dir_modelos[[m]]), m)
  prueba <- leer_costos(file.path(sens_dir, "actual"), m)
  j <- inner_join(real, prueba, by = c("ciudad", "fecha", "Demo_Group", "Sex"), suffix = c("_real", "_regla"))
  tibble(modelo = m, filas_proyecto = nrow(real), filas_regla = nrow(prueba),
         dif_maxima = max(abs(j$costo_real - j$costo_regla)))
})

# ============================================================
# Cobertura de fechas
# ============================================================

cobertura <- readRDS(file.path(sens_dir, "cobertura.rds"))$cobertura

diagnostico_ciudad <- cobertura %>%
  group_by(ciudad = city) %>%
  summarise(vistos = n(), pasan = sum(pasa_actual), no_pasan = sum(!pasa_actual), .groups = "drop") %>%
  mutate(nunca_vistos = n_distinct(cobertura$sipsa_name) - vistos) %>%
  arrange(desc(pasan))

alimentos_por_regla <- map_dfr(c("actual", "grupo85", "base"), function(r) {
  pasan <- filter(cobertura, .data[[paste0("pasa_", r)]])
  tibble(regla = r, pares = nrow(pasan), alimentos = n_distinct(pasan$sipsa_name),
         frutas_ibague = sum(pasan$city == "Ibagué" & pasan$es_fruta),
         frutas_manizales = sum(pasan$city == "Manizales" & pasan$es_fruta))
})

alimentos_ciudad <- cobertura %>%
  group_by(ciudad = city) %>%
  summarise(across(starts_with("pasa_"), sum), .groups = "drop")

# ============================================================
# Umbral de tamano: sku elegido y precio con y sin umbral
# ============================================================

leer_lista <- function(dir) {
  read.xlsx(file.path(dir, "lista_alimentos/lista_alimentos.xlsx")) %>%
    transmute(sipsa_name, city, sku_code = as.character(sku_code), gramos_sku)
}

con_umbral <- leer_lista(file.path(base_dir, "output"))
sin_umbral <- leer_lista(file.path(sens_dir, "sin_umbral_tamano/insumos"))

cambio_sku <- inner_join(con_umbral, sin_umbral, by = c("sipsa_name", "city"), suffix = c("_con", "_sin"),
                         relationship = "many-to-many") %>%
  filter(!is.na(gramos_sku_con)) %>%
  distinct(sipsa_name, city, .keep_all = TRUE) %>%
  summarise(pares = n(), cambian = sum(sku_code_con != sku_code_sin))

precios_tamano <- inner_join(
  readRDS(file.path(base_dir, "output/paneles/panel_mensual_cities_tcac.rds")) %>%
    select(articulo, ciudad, fecha, precio_100g),
  readRDS(file.path(sens_dir, "sin_umbral_tamano/insumos/paneles/panel_mensual_cities_tcac.rds")) %>%
    select(articulo, ciudad, fecha, precio_100g),
  by = c("articulo", "ciudad", "fecha"), suffix = c("_con", "_sin")
) %>%
  mutate(razon = precio_100g_sin / precio_100g_con) %>%
  filter(is.finite(razon))

cambio_precio_alimento <- precios_tamano %>%
  group_by(articulo) %>%
  summarise(razon = mean(razon, na.rm = TRUE), .groups = "drop") %>%
  arrange(razon)

resumen_precio_tamano <- precios_tamano %>%
  summarise(observaciones = n(), sin_cambio = sum(abs(razon - 1) < 0.005),
            mediana = median(razon, na.rm = TRUE),
            q10 = quantile(razon, 0.10, na.rm = TRUE), q90 = quantile(razon, 0.90, na.rm = TRUE))

saveRDS(list(
  costos = costos, validacion = validacion,
  diagnostico_ciudad = diagnostico_ciudad,
  alimentos_por_regla = alimentos_por_regla, alimentos_ciudad = alimentos_ciudad,
  cambio_sku = cambio_sku, cambio_precio_alimento = cambio_precio_alimento,
  resumen_precio_tamano = resumen_precio_tamano
), file.path(sens_dir, "tablas.rds"))

print(validacion)
