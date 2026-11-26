########################################################
## SCRIPT 02_dataprep/05_auditoria_precios.R
## Revisa el precio que entra a los modelos, alimento por alimento y
## ciudad por ciudad, y marca los casos que conviene mirar a mano.
## Reemplaza a 04_auditoria_productos.R, que revisaba el producto
## representativo; ahora el precio de cada alimento es la mediana de sus
## productos elegibles.
##
## Marcas por alimento-ciudad
##   dif_recalculo        el precio por 100 g del panel no coincide (mas de
##                        1%) con el que se recalcula desde los precios de
##                        cada producto: el panel quedo desactualizado
##   dif_formula          algun producto con tamano en el nombre tiene un
##                        precio_base que no coincide con
##                        price / (gramos / densidad) * objetivo_base
##   caro_barato_ciudades el precio por 100 g de la ciudad es mas de 1,5
##                        veces, o menos de 1/1,5, la mediana de las ciudades
##   sin_coincidencia     algun producto elegible no comparte ninguna
##                        palabra con el nombre del alimento
##   un_producto          el precio sale de un solo producto
##   sin_medida           el precio se calculo por kg o por unidad
##
## Reads:  output_dir/paneles/raw_mensual/precios_producto.rds
##         output_dir/lista_alimentos/lista_productos.xlsx
##         output_dir/paneles/panel_mensual_cities_tcac.rds
## Writes: output_dir/auditoria/auditoria_precios.xlsx
##         output_dir/auditoria/auditoria_precios.rds
########################################################

library(tidyverse)
library(openxlsx)
library(stringi)

# ============================================================
# Rutas y parametros (los mismos de 01_panel_precios.R)
# ============================================================

base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/paper-global-food"

output_dir <- file.path(base_dir, "output")
audit_dir  <- file.path(output_dir, "auditoria")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

min_skus_atipicos <- 4
factor_atipico    <- 3

# ============================================================
# Insumos
# ============================================================

precios <- readRDS(file.path(output_dir, "paneles/raw_mensual/precios_producto.rds")) %>%
  mutate(mes = format(fecha, "%Y-%m"))

productos <- read.xlsx(file.path(output_dir, "lista_alimentos/lista_productos.xlsx")) %>%
  mutate(sku_code = as.character(sku_code))

panel <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  transmute(sipsa_name = articulo, city = ciudad, mes = format(as.Date(fecha), "%Y-%m"),
            precio_100g, parte_comestible)

# Igual que en 01_panel_precios.R
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

entran <- aplicar_reglas(precios)

# ============================================================
# 1. Precio por 100 g recalculado desde los productos
# ============================================================

recalculo <- entran %>%
  group_by(sipsa_name, city, mes, fecha) %>%
  summarise(p = round(median(precio_base), 0),
            objetivo_base = first(objetivo_base), densidad = first(densidad),
            .groups = "drop") %>%
  group_by(sipsa_name, city, mes) %>%
  summarise(precio_base = round(median(p), 0),
            objetivo_base = first(objetivo_base), densidad = first(densidad),
            .groups = "drop") %>%
  inner_join(panel, by = c("sipsa_name", "city", "mes")) %>%
  mutate(precio_100g_recalculo = precio_base / (objetivo_base * densidad / 100 * parte_comestible)) %>%
  group_by(sipsa_name, city) %>%
  summarise(dif_recalculo = max(abs(precio_100g / precio_100g_recalculo - 1), na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dif_recalculo = if_else(is.infinite(dif_recalculo), NA_real_, dif_recalculo))

# ============================================================
# 2. Formula de estandarizacion en los productos con tamano
# ============================================================

formula <- entran %>%
  inner_join(productos %>% filter(!is.na(gramos_sku)) %>% select(sipsa_name, city, sku_code, gramos_sku),
             by = c("sipsa_name", "city", "sku_code")) %>%
  mutate(precio_base_formula = price / (gramos_sku / densidad) * objetivo_base) %>%
  group_by(sipsa_name, city) %>%
  summarise(dif_formula = max(abs(precio_base / precio_base_formula - 1)), .groups = "drop")

# ============================================================
# 3. Precio de la ciudad frente a la mediana de las ciudades
# ============================================================

precio_ciudad <- panel %>%
  group_by(sipsa_name, city) %>%
  summarise(precio_100g = mean(precio_100g, na.rm = TRUE), .groups = "drop") %>%
  group_by(sipsa_name) %>%
  mutate(razon_ciudades = precio_100g / median(precio_100g, na.rm = TRUE)) %>%
  ungroup()

# ============================================================
# 4. Nombre del producto frente al nombre del alimento
# ============================================================

palabras <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_extract_all("[a-z]{4,}")
}

productos_entran <- productos %>%
  semi_join(distinct(entran, sipsa_name, city, sku_code), by = c("sipsa_name", "city", "sku_code")) %>%
  mutate(coincide_nombre = map2_lgl(palabras(sipsa_name), palabras(product_name),
                                    ~ length(intersect(.x, .y)) > 0))

sin_coincidencia <- productos_entran %>%
  filter(!coincide_nombre) %>%
  distinct(sipsa_name, product_name) %>%
  arrange(sipsa_name)

# ============================================================
# 5. Numero de productos y alimentos sin medida
# ============================================================

composicion_precio <- entran %>%
  group_by(sipsa_name, city) %>%
  summarise(n_productos = n_distinct(sku_code),
            sin_medida  = all(is.na(cumple_tamano)),
            .groups = "drop")

# ============================================================
# Auditoria
# ============================================================

auditoria <- distinct(panel, sipsa_name, city) %>%
  left_join(composicion_precio, by = c("sipsa_name", "city")) %>%
  left_join(recalculo, by = c("sipsa_name", "city")) %>%
  left_join(formula, by = c("sipsa_name", "city")) %>%
  left_join(precio_ciudad %>% select(sipsa_name, city, razon_ciudades), by = c("sipsa_name", "city")) %>%
  left_join(productos_entran %>% group_by(sipsa_name, city) %>%
              summarise(productos_sin_coincidencia = sum(!coincide_nombre), .groups = "drop"),
            by = c("sipsa_name", "city")) %>%
  mutate(
    marca_dif_recalculo        = !is.na(dif_recalculo) & dif_recalculo > 0.01,
    marca_dif_formula          = !is.na(dif_formula) & dif_formula > 0.01,
    marca_caro_barato_ciudades = !is.na(razon_ciudades) & (razon_ciudades > 1.5 | razon_ciudades < 1 / 1.5),
    marca_sin_coincidencia     = coalesce(productos_sin_coincidencia, 0L) > 0,
    marca_un_producto          = n_productos == 1,
    marca_sin_medida           = sin_medida,
    n_marcas = marca_dif_recalculo + marca_dif_formula + marca_caro_barato_ciudades +
      marca_sin_coincidencia
  ) %>%
  arrange(desc(n_marcas), sipsa_name, city)

revisar <- auditoria %>% filter(n_marcas > 0)

resumen <- auditoria %>%
  summarise(
    alimentos_ciudad     = n(),
    dif_recalculo        = sum(marca_dif_recalculo),
    dif_formula          = sum(marca_dif_formula),
    caro_barato_ciudades = sum(marca_caro_barato_ciudades),
    sin_coincidencia     = sum(marca_sin_coincidencia),
    un_producto          = sum(marca_un_producto, na.rm = TRUE),
    sin_medida           = sum(marca_sin_medida, na.rm = TRUE)
  )

write.xlsx(list(resumen = resumen, revisar = revisar, todos = auditoria,
                nombres_sin_coincidencia = sin_coincidencia),
           file.path(audit_dir, "auditoria_precios.xlsx"), overwrite = TRUE)

saveRDS(auditoria, file.path(audit_dir, "auditoria_precios.rds"))

print(resumen)
message("Done.")
