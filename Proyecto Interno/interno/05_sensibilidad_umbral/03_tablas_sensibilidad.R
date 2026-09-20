########################################################
## SCRIPT 05_sensibilidad_umbral/03_tablas_sensibilidad.R
## Junta los resultados de 02_correr_regla.R (una carpeta por regla)
## y la cobertura de 01_cobertura_alimentos.R en las tablas que usa
## docs/reporte_proyecto.Rmd.
##
## Reads:  sens_dir/cobertura.rds
##         sens_dir/<regla>/ (resultados de cada modelo)
##         03_models/ (resultados del proyecto, para validar "actual")
## Writes: sens_dir/tablas.rds
########################################################

library(tidyverse)

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
sens_dir <- file.path(base_dir, "output/sensibilidad_umbral")

reglas  <- c("actual", "base", "fruta75", "fruta80", "semanal", "quincenal")
modelos <- c("CoCA", "CoNA", "CoRD", "CoAHD")

norm_ciudad <- function(x) toupper(iconv(x, from = "", to = "ASCII//TRANSLIT"))

# ============================================================
# Costos de cada modelo (por miembro y mes) y sus alimentos
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
      select(ciudad, fecha, Demo_Group, Sex, costo = cost_day)
  ) %>% mutate(fecha = as.Date(fecha))
}

leer_alimentos <- function(dir, modelo) {
  switch(modelo,
    CoCA  = readRDS(file.path(dir, "coca_results.rds")) %>% filter(quantity > 0),
    CoNA  = readRDS(file.path(dir, "cona_results.rds"))$comp %>% filter(quantity > 0),
    CoRD  = readRDS(file.path(dir, "cord_results.rds"))$comp,
    CoAHD = readRDS(file.path(dir, "coahd_results.rds"))$comp
  ) %>% distinct(ciudad, Food)
}

percapita <- function(df) {
  df %>%
    group_by(ciudad, fecha) %>% summarise(hogar = sum(costo), .groups = "drop") %>%
    group_by(ciudad) %>% summarise(pc = mean(hogar) / 3, .groups = "drop")
}

costos <- expand_grid(regla = reglas, modelo = modelos) %>%
  mutate(pc = map2(regla, modelo, ~ percapita(leer_costos(file.path(sens_dir, .x), .y)))) %>%
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
# Cobertura: qué pasa y qué no pasa la regla actual
# ============================================================

cob <- readRDS(file.path(sens_dir, "cobertura.rds"))
cobertura <- cob$cobertura
no_pasan  <- filter(cobertura, !pasa_actual)

diagnostico_ciudad <- cobertura %>%
  group_by(ciudad = city) %>%
  summarise(vistos = n(), pasan = sum(pasa_actual), no_pasan = sum(!pasa_actual),
            con_semanal = sum(!pasa_actual & pasa_semanal),
            con_quincenal = sum(!pasa_actual & pasa_quincenal), .groups = "drop") %>%
  mutate(nunca_vistos = n_distinct(cobertura$sipsa_name) - vistos) %>%
  arrange(desc(pasan))

dias_no_pasan <- no_pasan %>%
  mutate(rango = cut(n_dias, c(0, 5, 12, 25, 40, 55, Inf),
                     labels = c("1 a 5", "6 a 12", "13 a 25", "26 a 40", "41 a 55", "56 o más"))) %>%
  count(rango)

resumen_no_pasan <- list(
  total          = nrow(no_pasan),
  hasta_12       = sum(no_pasan$n_dias <= 12),
  hasta_5        = sum(no_pasan$n_dias <= 5),
  desde_41       = sum(no_pasan$n_dias >= 41),
  desde_41_3mes  = sum(no_pasan$n_dias >= 41 & no_pasan$n_meses == 3),
  desde_56       = sum(no_pasan$n_dias >= 56),
  solo_semanal   = sum(no_pasan$pasa_semanal),
  solo_semanal_3mes = sum(no_pasan$pasa_semanal & no_pasan$n_meses == 3),
  solo_quincenal = sum(!no_pasan$pasa_semanal & no_pasan$pasa_quincenal),
  solo_quincenal_3mes = sum(!no_pasan$pasa_semanal & no_pasan$pasa_quincenal & no_pasan$n_meses == 3)
)

alimentos_por_regla <- map_dfr(reglas, function(r) {
  pasan <- filter(cobertura, .data[[paste0("pasa_", r)]])
  tibble(regla = r, pares = nrow(pasan), alimentos = n_distinct(pasan$sipsa_name),
         frutas_ibague = sum(pasan$city == "Ibagué" & pasan$es_fruta),
         frutas_manizales = sum(pasan$city == "Manizales" & pasan$es_fruta))
})

alimentos_ciudad <- cobertura %>%
  group_by(ciudad = city) %>%
  summarise(across(starts_with("pasa_"), sum), .groups = "drop")

frutas_ibague_no_pasan <- cobertura %>%
  filter(city == "Ibagué", es_fruta, !pasa_actual) %>%
  arrange(desc(n_dias)) %>%
  transmute(sipsa_name, n_dias, n_meses, umbral = floor(0.85 * dias_grupo))

# ============================================================
# Alimentos que eligen los modelos y no pasarían la regla actual
# ============================================================

obs <- cobertura %>% mutate(ciudad = norm_ciudad(city)) %>% select(ciudad, Food = sipsa_name, n_dias, pasa_actual)

elegidos <- expand_grid(regla = c("semanal", "quincenal"), modelo = modelos) %>%
  mutate(x = map2(regla, modelo, ~ leer_alimentos(file.path(sens_dir, .x), .y))) %>%
  unnest(x) %>%
  left_join(obs, by = c("ciudad", "Food"))

confiabilidad <- elegidos %>%
  group_by(regla, modelo) %>%
  summarise(pares = n(), no_pasan_actual = sum(!pasa_actual), minimo_dias = min(n_dias), .groups = "drop") %>%
  mutate(pct = 100 * no_pasan_actual / pares)

poco_observados <- elegidos %>%
  filter(regla == "quincenal", n_dias < 10) %>%
  arrange(n_dias) %>% select(modelo, ciudad, Food, n_dias)

saveRDS(list(
  costos = costos, validacion = validacion,
  diagnostico_ciudad = diagnostico_ciudad, dias_no_pasan = dias_no_pasan,
  resumen_no_pasan = resumen_no_pasan, alimentos_por_regla = alimentos_por_regla, alimentos_ciudad = alimentos_ciudad,
  frutas_ibague_no_pasan = frutas_ibague_no_pasan,
  confiabilidad = confiabilidad, poco_observados = poco_observados
), file.path(sens_dir, "tablas.rds"))

message("Validación de la regla actual contra 03_models:")
print(validacion)
message("Listo. tablas.rds guardado en ", sens_dir)
