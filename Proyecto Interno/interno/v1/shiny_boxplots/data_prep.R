########################################################
## SCRIPT v1/shiny_boxplots/data_prep.R
##
## Data prep para el boxplot de precio por gramo bajo el metodo
## v1 (sku mas cercano a 500g/1000ml). Comparar contra el boxplot
## de la version en produccion (02_dataprep/03_shiny_boxplots/).
##
## Ahora que v1 tiene su propio pipeline corrido hasta CoNA (ver
## v1/01_webscrap_prep, v1/02_dataprep, v1/03_models), este script
## usa directamente v1/output/paneles/panel_mensual_cities_tcac.rds
## (ya trae precio_500g, precio_100g y grupos_gabas) y la
## composicion CoNA propia de v1 (v1/03_models/cona), en vez de
## reutilizar los datos de la version en produccion.
##
## Reads:  v1_output_dir/paneles/panel_mensual_cities_tcac.rds
##         v1_cona_dir/cona_results.rds  (elemento $comp)
## Writes: shiny_dir/data_boxplots.rds  (list con: all, selected_foods, resumen)
########################################################

library(tidyverse)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"

v1_output_dir <- file.path(base_dir, "v1/output")
v1_cona_dir   <- file.path(base_dir, "v1/03_models/cona")
shiny_dir     <- file.path(base_dir, "v1/shiny_boxplots")

dir.create(shiny_dir, recursive = TRUE, showWarnings = FALSE)

ALIMENTO_EXCLUIR <- "Bocadillo veleño"

# ============================================================
# Panel de precios (mensual, ya con grupos_gabas)
# ============================================================

panel <- readRDS(file.path(v1_output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  mutate(
    fecha        = as.Date(fecha),
    precio_gramo = precio_500g / 500
  ) %>%
  filter(!is.na(precio_gramo), precio_gramo > 0) %>%
  select(articulo, ciudad, fecha, grupos_gabas, precio_500g, precio_100g, precio_gramo)

message(sprintf("Panel v1 cargado: %d filas | %d alimentos | %d ciudades",
                nrow(panel), n_distinct(panel$articulo), n_distinct(panel$ciudad)))

# ============================================================
# Alimentos seleccionados: solucion optima del CoNA bajo v1
# ============================================================

cona_comp <- readRDS(file.path(v1_cona_dir, "cona_results.rds"))$comp %>%
  filter(quantity > 0)

alimentos_seleccionados_todos <- sort(unique(cona_comp$Food))
alimentos_seleccionados <- setdiff(alimentos_seleccionados_todos, ALIMENTO_EXCLUIR)

message(sprintf(
  "Alimentos seleccionados (CoNA v1): %d totales | %d despues de eliminar '%s'",
  length(alimentos_seleccionados_todos), length(alimentos_seleccionados), ALIMENTO_EXCLUIR))

# ============================================================
# Data para el boxplot + resumen min/mediana/max
# ============================================================

data_all <- panel %>%
  mutate(seleccionado = articulo %in% alimentos_seleccionados)

resumen_precio_gramo <- data_all %>%
  group_by(articulo, grupos_gabas, seleccionado) %>%
  summarise(
    n          = n(),
    min_pg     = min(precio_gramo, na.rm = TRUE),
    mediana_pg = median(precio_gramo, na.rm = TRUE),
    max_pg     = max(precio_gramo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(min_pg)

# ============================================================
# Guardar
# ============================================================

saveRDS(
  list(
    all            = data_all,
    selected_foods = alimentos_seleccionados,
    excluded_food  = ALIMENTO_EXCLUIR,
    resumen        = resumen_precio_gramo
  ),
  file.path(shiny_dir, "data_boxplots.rds")
)

cat("\nListo. data_boxplots.rds (metodo v1) guardado en", shiny_dir, "\n")
