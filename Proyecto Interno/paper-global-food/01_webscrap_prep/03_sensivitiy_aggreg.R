########################################################
## SCRIPT 01_webscrap_prep/03_sensibilidad_agregacion.R
##
## Sensibilidad y validacion de la regla con que se pasa de los precios
## de varios productos al precio de un alimento en cada ciudad y mes.
## Reune lo que antes hacian 03_sensibilidad_agregacion.R y
## 04_validacion_reglas.R.
##
## Regla principal (01_panel_precios.R): mediana diaria de los
## productos elegibles, sin atipicos (factor 3) y con preferencia por
## empaques de hasta 500 g o 1.000 ml. Alternativas:
##
##   representativo  un producto: el de precio del periodo mas cercano a
##                   la mediana (en empate, el mas barato)
##   geometrica      media geometrica diaria entre productos
##   p25             percentil 25 diario entre productos
##   minimo          producto mas barato cada dia
##   mediana_sin     mediana sin preferencia de tamano
##   mediana_f2.5    mediana con factor de atipicos 2,5
##   mediana_f4      mediana con factor de atipicos 4
##
## En todas, el precio mensual es la mediana de los precios diarios.
##
## Parte A. Precio mensual con cada regla y diferencia frente a la
##          regla principal.
## Parte B. Validacion con criterios que no dependen de ninguna regla
##          (solo las cinco reglas con los mismos productos):
##   B1. Fiabilidad entre mitades de fechas: cada regla se calcula por
##       separado en dos mitades alternas de las fechas de cada mes.
##   B2. Robustez al surtido: en cada alimento-ciudad con dos productos
##       o mas, se quita un producto a la vez y se recalcula el precio.
##
## Reads:  output_panel_dir/raw_mensual/precios_producto.rds
##         output_panel_dir/raw_mensual/lista_por_ciudad.rds
## Writes: output_dir/sensibilidad_agregacion/panel_reglas.rds
##         output_dir/sensibilidad_agregacion/sensibilidad_reglas.xlsx
########################################################

library(tidyverse)
library(writexl)

# ============================================================
# Rutas y parametros
# ============================================================

base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/paper-global-food"

output_dir       <- file.path(base_dir, "output")
output_panel_dir <- file.path(output_dir, "paneles")
sens_dir         <- file.path(output_dir, "sensibilidad_agregacion")

dir.create(sens_dir, recursive = TRUE, showWarnings = FALSE)

# Los mismos de 01_panel_precios.R
min_skus_atipicos <- 4
factor_atipico    <- 3

reglas_centrales <- c("mediana", "representativo", "geometrica", "p25", "minimo")
reglas_todas     <- c(reglas_centrales, "mediana_sin", "mediana_f2.5", "mediana_f4")

# ============================================================
# Cargar insumos
# ============================================================

lista_por_ciudad <- readRDS(file.path(output_panel_dir, "raw_mensual/lista_por_ciudad.rds"))

precios <- readRDS(file.path(output_panel_dir, "raw_mensual/precios_producto.rds")) %>%
  mutate(mes = format(fecha, "%Y-%m")) %>%
  semi_join(lista_por_ciudad, by = c("city", "sipsa_name"))

# ============================================================
# Funciones
# ============================================================

# Igual que en 01_panel_precios.R: atipicos y preferencia de tamano
aplicar_reglas <- function(df, factor = factor_atipico, preferir_tamano = TRUE) {
  df %>%
    mutate(atipico = !is.na(veces_mediana) &
             n_skus_alimento >= min_skus_atipicos &
             (veces_mediana > factor | veces_mediana < 1 / factor)) %>%
    group_by(sipsa_name, city) %>%
    filter(!atipico | all(atipico)) %>%
    filter(!preferir_tamano | is.na(cumple_tamano) | cumple_tamano |
             !any(cumple_tamano, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-atipico)
}

# Precio mensual con las cinco reglas centrales, a partir de los
# precios diarios de los productos (antes de aplicar las reglas).
# En los alimentos-ciudad sin productos con tamano (cumple_tamano NA),
# el representativo es la mediana diaria entre productos.
precio_reglas <- function(df) {
  
  df <- aplicar_reglas(df)
  
  con_tamano <- df %>% group_by(sipsa_name, city) %>% filter(any(!is.na(cumple_tamano))) %>% ungroup()
  sin_tamano <- df %>% group_by(sipsa_name, city) %>% filter(all(is.na(cumple_tamano))) %>% ungroup()
  
  elegido <- con_tamano %>%
    group_by(sipsa_name, city, sku_code) %>%
    summarise(precio_periodo = median(precio_base), .groups = "drop") %>%
    group_by(sipsa_name, city) %>%
    mutate(distancia = abs(precio_periodo - median(precio_periodo))) %>%
    arrange(distancia, precio_periodo, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(sipsa_name, city, sku_code)
  
  representativo <- bind_rows(
    con_tamano %>% semi_join(elegido, by = c("sipsa_name", "city", "sku_code")),
    sin_tamano
  ) %>%
    group_by(sipsa_name, city, mes, fecha) %>%
    summarise(representativo = median(precio_base), .groups = "drop")
  
  alternativas <- df %>%
    group_by(sipsa_name, city, mes, fecha) %>%
    summarise(
      mediana    = median(precio_base),
      geometrica = exp(mean(log(precio_base))),
      p25        = unname(quantile(precio_base, 0.25)),
      minimo     = min(precio_base),
      .groups = "drop"
    )
  
  full_join(representativo, alternativas, by = c("sipsa_name", "city", "mes", "fecha")) %>%
    group_by(sipsa_name, city, mes) %>%
    summarise(across(all_of(reglas_centrales), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(all_of(reglas_centrales), names_to = "regla", values_to = "precio") %>%
    filter(is.finite(precio))
}

# Mediana mensual de la mediana diaria entre productos
mediana_mensual <- function(df, nombre) {
  df %>%
    group_by(sipsa_name, city, mes, fecha) %>%
    summarise(p = median(precio_base), .groups = "drop") %>%
    group_by(sipsa_name, city, mes) %>%
    summarise(precio = median(p), .groups = "drop") %>%
    mutate(regla = nombre)
}

resumir <- function(df) {
  df %>%
    group_by(regla, grupo) %>%
    summarise(
      observaciones = n(),
      dif_mediana   = 100 * median(dif),
      dif_p90       = 100 * quantile(dif, 0.90),
      pct_mas_de_10 = 100 * mean(dif > log(1.10)),
      .groups = "drop"
    ) %>%
    mutate(regla = factor(regla, levels = reglas_centrales)) %>%
    arrange(grupo, regla)
}

# Numero de productos que entran al precio de cada alimento-ciudad
n_productos <- aplicar_reglas(precios) %>%
  group_by(sipsa_name, city) %>%
  summarise(n_productos = n_distinct(sku_code), .groups = "drop")

# ============================================================
# Parte A. Precio con cada regla y diferencia frente a la principal
# ============================================================

panel_reglas <- bind_rows(
  precio_reglas(precios),
  mediana_mensual(aplicar_reglas(precios, preferir_tamano = FALSE), "mediana_sin"),
  mediana_mensual(aplicar_reglas(precios, factor = 2.5), "mediana_f2.5"),
  mediana_mensual(aplicar_reglas(precios, factor = 4), "mediana_f4")
) %>%
  mutate(regla = factor(regla, levels = reglas_todas)) %>%
  arrange(regla, sipsa_name, city, mes)

comparacion <- panel_reglas %>%
  filter(regla != "mediana") %>%
  inner_join(panel_reglas %>% filter(regla == "mediana") %>%
               select(sipsa_name, city, mes, precio_principal = precio),
             by = c("sipsa_name", "city", "mes")) %>%
  mutate(razon = precio / precio_principal) %>%
  left_join(n_productos, by = c("sipsa_name", "city"))

frente_principal <- comparacion %>%
  group_by(regla) %>%
  summarise(
    observaciones  = n(),
    razon_media    = mean(razon),
    razon_p05      = quantile(razon, 0.05),
    razon_p50      = median(razon),
    razon_p95      = quantile(razon, 0.95),
    pct_mas_5      = 100 * mean(razon > 1.05),
    pct_menos_5    = 100 * mean(razon < 0.95),
    .groups = "drop"
  )

por_alimento <- comparacion %>%
  group_by(regla, sipsa_name) %>%
  summarise(razon_mediana = median(razon), .groups = "drop") %>%
  pivot_wider(names_from = regla, values_from = razon_mediana) %>%
  arrange(sipsa_name)

# ============================================================
# Parte B1. Fiabilidad entre mitades de fechas
# ============================================================

mitades <- precios %>%
  distinct(mes, fecha) %>%
  arrange(fecha) %>%
  group_by(mes) %>%
  mutate(mitad = if_else(row_number() %% 2 == 1, "A", "B")) %>%
  ungroup()

precios_mitad <- precios %>% left_join(mitades, by = c("mes", "fecha"))

fiabilidad <- inner_join(
  precio_reglas(filter(precios_mitad, mitad == "A")) %>% rename(precio_a = precio),
  precio_reglas(filter(precios_mitad, mitad == "B")) %>% rename(precio_b = precio),
  by = c("sipsa_name", "city", "mes", "regla")
) %>%
  left_join(n_productos, by = c("sipsa_name", "city")) %>%
  mutate(dif   = abs(log(precio_a / precio_b)),
         grupo = if_else(n_productos >= 2, "dos productos o mas", "un producto"))

fiabilidad_resumen <- bind_rows(resumir(fiabilidad),
                                resumir(mutate(fiabilidad, grupo = "todos")))

# ============================================================
# Parte B2. Robustez al surtido: quitar un producto a la vez
# ============================================================
# veces_mediana no se recalcula al quitar un producto: el cambio en la
# mediana del alimento en 13 ciudades es despreciable.

multi <- precios %>%
  semi_join(filter(n_productos, n_productos >= 2), by = c("sipsa_name", "city"))

base_multi <- precio_reglas(multi)

grupos <- split(multi, list(multi$sipsa_name, multi$city), drop = TRUE)

message("Robustez al surtido sobre ", length(grupos), " alimentos-ciudad. Puede tardar unos minutos.")

jackknife <- map_dfr(grupos, function(g) {
  map_dfr(unique(g$sku_code), function(sku) {
    precio_reglas(filter(g, sku_code != sku))
  })
}) %>%
  inner_join(base_multi %>% rename(precio_completo = precio),
             by = c("sipsa_name", "city", "mes", "regla")) %>%
  left_join(n_productos, by = c("sipsa_name", "city")) %>%
  mutate(dif   = abs(log(precio / precio_completo)),
         grupo = case_when(n_productos == 2 ~ "2 productos",
                           n_productos <= 4 ~ "3-4 productos",
                           TRUE             ~ "5 o mas productos"))

jackknife_resumen <- bind_rows(resumir(jackknife),
                               resumir(mutate(jackknife, grupo = "todos")))

# ============================================================
# Guardar
# ============================================================

saveRDS(panel_reglas, file.path(sens_dir, "panel_reglas.rds"))

salida <- list(frente_principal = frente_principal, por_alimento = por_alimento,
               fiabilidad = fiabilidad_resumen, robustez_surtido = jackknife_resumen)

write_xlsx(salida, file.path(sens_dir, "sensibilidad_reglas.xlsx"))

print(frente_principal)
print(fiabilidad_resumen, n = Inf)
print(jackknife_resumen, n = Inf)

message("Done.")