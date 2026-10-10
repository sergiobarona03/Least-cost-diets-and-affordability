########################################################
## SCRIPT 02_dataprep/04_auditoria_productos.R
## Revisa producto por producto el precio que entra a los modelos:
## rehace el precio por 100 g del sku elegido desde los datos
## originales y marca los casos que hay que mirar a mano.
##
## Marcas:
##   dif_original     el precio del panel no coincide con el que sale
##                    de los datos originales (mas de 1%)
##   caro_barato_sku  el sku elegido cuesta mas del doble o menos de la
##                    mitad por gramo que la mediana de los skus con
##                    medida de ese alimento en la ciudad
##   caro_barato_ciudades  el precio por 100 g de la ciudad es 1,5 veces
##                    mayor o menor que la mediana de las ciudades
##   sin_coincidencia el nombre del producto no comparte ninguna palabra
##                    con el nombre del alimento
##   sin_medida       el precio se calculo por kg o por unidad
##
## Reads:  panel_dir/data/*.rds
##         output_dir/lista_alimentos/lista_alimentos.xlsx
##         output_dir/lista_alimentos/skus_todos.rds
##         output_dir/paneles/panel_mensual_cities_tcac.rds
## Writes: output_dir/auditoria/auditoria_productos.xlsx
########################################################

library(tidyverse)
library(lubridate)
library(openxlsx)
library(stringi)

base_dir   <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
output_dir <- file.path(base_dir, "output")
raw_dir    <- file.path(base_dir, "01_webscrap_prep/data")
audit_dir  <- file.path(output_dir, "auditoria")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Sku elegido y precio del panel
# ============================================================

elegidos <- read.xlsx(file.path(output_dir, "lista_alimentos/lista_alimentos.xlsx")) %>%
  mutate(sku_code = as.character(sku_code))

panel <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  transmute(sipsa_name = articulo, city = ciudad, mes = format(as.Date(fecha), "%Y-%m"),
            precio_100g, parte_comestible)

# ============================================================
# Precio por 100 g rehecho desde los datos originales
# ============================================================

leer_original <- function(ruta) {
  readRDS(ruta) %>%
    transmute(
      sipsa_name = as.character(sipsa_name),
      city       = str_squish(as.character(city)),
      sku_code   = as.character(sku_code),
      price      = suppressWarnings(parse_number(as.character(price))),
      fecha      = dmy(str_extract(archivo_origen, "\\d{2}-\\d{2}-\\d{4}"))
    ) %>%
    mutate(city = case_when(
      str_to_lower(city) %in% c("bogotá", "bogotá, d.c.", "bogota", "bogota, d.c.") ~ "Bogotá",
      str_detect(str_to_lower(city), "^cartagena") ~ "Cartagena",
      TRUE ~ city
    )) %>%
    filter(price > 0) %>%
    semi_join(elegidos, by = c("sipsa_name", "city", "sku_code"))
}

original <- map_dfr(list.files(raw_dir, pattern = "\\.rds$", full.names = TRUE), leer_original)

precio_original <- original %>%
  mutate(mes = format(fecha, "%Y-%m")) %>%
  inner_join(elegidos %>% filter(!is.na(gramos_sku)) %>% select(sipsa_name, city, sku_code, gramos_sku),
             by = c("sipsa_name", "city", "sku_code")) %>%
  group_by(sipsa_name, city, mes, fecha) %>%
  summarise(p = median(price / gramos_sku * 100), .groups = "drop_last") %>%
  summarise(precio_100g_original = median(p), .groups = "drop")

# ============================================================
# Contexto: skus con medida de cada alimento-ciudad
# ============================================================

skus <- readRDS(file.path(output_dir, "lista_alimentos/skus_todos.rds")) %>%
  filter(!is.na(precio_gramo_sku)) %>%
  group_by(sipsa_name, city) %>%
  summarise(mediana_skus_gramo = median(precio_gramo_sku), n_skus = n(), .groups = "drop")

# ============================================================
# Marcas
# ============================================================

palabras <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_extract_all("[a-z]{4,}")
}

coincide <- map2_lgl(palabras(elegidos$sipsa_name), palabras(elegidos$exito_name),
                     ~ length(intersect(.x, .y)) > 0)

precio_ciudad <- panel %>%
  group_by(sipsa_name, city) %>%
  summarise(precio_100g = mean(precio_100g, na.rm = TRUE), .groups = "drop") %>%
  group_by(sipsa_name) %>%
  mutate(razon_ciudades = precio_100g / median(precio_100g, na.rm = TRUE)) %>%
  ungroup()

precio_panel <- panel %>%
  group_by(sipsa_name, city, mes) %>%
  summarise(precio_100g = first(precio_100g), parte_comestible = first(parte_comestible), .groups = "drop") %>%
  left_join(precio_original, by = c("sipsa_name", "city", "mes")) %>%
  mutate(precio_100g_original = precio_100g_original / parte_comestible) %>%
  group_by(sipsa_name, city) %>%
  summarise(dif_original = max(abs(precio_100g / precio_100g_original - 1), na.rm = TRUE), .groups = "drop") %>%
  mutate(dif_original = ifelse(is.infinite(dif_original), NA, dif_original))

auditoria <- elegidos %>%
  mutate(coincide_nombre = coincide) %>%
  left_join(skus, by = c("sipsa_name", "city")) %>%
  left_join(precio_ciudad, by = c("sipsa_name", "city")) %>%
  left_join(precio_panel, by = c("sipsa_name", "city")) %>%
  inner_join(distinct(panel, sipsa_name, city), by = c("sipsa_name", "city")) %>%
  mutate(
    razon_sku = precio_gramo_sku / mediana_skus_gramo,
    marca_dif_original         = !is.na(dif_original) & dif_original > 0.01,
    marca_caro_barato_sku      = !is.na(razon_sku) & (razon_sku > 2 | razon_sku < 0.5),
    marca_caro_barato_ciudades = !is.na(razon_ciudades) & (razon_ciudades > 1.5 | razon_ciudades < 1 / 1.5),
    marca_sin_coincidencia     = !coincide_nombre,
    marca_sin_medida           = is.na(gramos_sku)
  ) %>%
  mutate(n_marcas = marca_dif_original + marca_caro_barato_sku + marca_caro_barato_ciudades +
           marca_sin_coincidencia) %>%
  arrange(desc(n_marcas), sipsa_name, city)

revisar <- auditoria %>% filter(n_marcas > 0)

write.xlsx(list(seleccionados = auditoria, revisar = revisar),
           file.path(audit_dir, "auditoria_productos.xlsx"), overwrite = TRUE)

saveRDS(auditoria, file.path(audit_dir, "auditoria_productos.rds"))
