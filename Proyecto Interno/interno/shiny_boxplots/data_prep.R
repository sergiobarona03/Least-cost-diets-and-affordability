########################################################
## SCRIPT 02_dataprep/03_shiny_boxplots/data_prep_v2.R
##
## VERSION 2 - > data prep para la app de Shiny con los boxplots
## de precio por gramo.
##
## "Alimentos seleccionados" = los alimentos que aparecen con
## cantidad > 0 en la solucion optima del CoNA (cona_results$comp),
## en cualquier ciudad/miembro/fecha del trimestre 
##
## Tambien arma un resumen min/mediana/max de precio por gramo
## por alimento, para apoyar la eleccion del umbral usado en 
## 01_webscrap_prep/01_construccion_panel.R.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         cona_dir/cona_results.rds  (elemento $comp)
## Writes: shiny_dir/data_boxplots.rds
##         (list con: all, selected_foods, resumen)
########################################################

library(tidyverse)

# ============================================================
# Rutas
# ============================================================

#base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"

output_dir <- file.path(base_dir, "output")
cona_dir   <- file.path(base_dir, "03_models/cona")
shiny_dir  <- file.path(base_dir, "shiny_boxplots")

dir.create(shiny_dir, recursive = TRUE, showWarnings = FALSE)

ALIMENTO_EXCLUIR <- "Bocadillo veleño"

# ============================================================
# Cargar panel de precios (precio_500g / precio_100g, nivel
# alimento-ciudad-mes) y construir precio por gramo
# ============================================================

panel <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  mutate(
    fecha       = as.Date(fecha),
    precio_gramo = precio_500g / 500
  ) %>%
  filter(!is.na(precio_gramo), precio_gramo > 0) %>%
  select(articulo, ciudad, fecha, grupos_gabas, precio_500g, precio_100g, precio_gramo)

message(sprintf("Panel cargado: %d filas | %d alimentos | %d ciudades",
                nrow(panel), n_distinct(panel$articulo), n_distinct(panel$ciudad)))

# ============================================================
# Alimentos seleccionados: los que aparecen en la solucion
# optima del CoNA (quantity > 0), sin importar ciudad/miembro/
# fecha -- mismo universo que 04_figures/03_fig_cona_composicion.R
# ============================================================

cona_comp <- readRDS(file.path(cona_dir, "cona_results.rds"))$comp %>%
  filter(quantity > 0)

alimentos_seleccionados_todos <- sort(unique(cona_comp$Food))
alimentos_seleccionados <- setdiff(alimentos_seleccionados_todos, ALIMENTO_EXCLUIR)

message(sprintf(
  "Alimentos seleccionados (CoNA): %d totales | %d despues de eliminar '%s'",
  length(alimentos_seleccionados_todos), length(alimentos_seleccionados), ALIMENTO_EXCLUIR))

foods_sin_precio <- setdiff(alimentos_seleccionados, unique(panel$articulo))
if (length(foods_sin_precio) > 0) {
  warning("Alimentos seleccionados sin precio en el panel: ",
          paste(foods_sin_precio, collapse = ", "))
}

# ============================================================
# Data para el boxplot: todos los alimentos, con bandera de
# "seleccionado" (alimentos seleccionados del CoNA, sin Bocadillo)
# ============================================================

data_all <- panel %>%
  mutate(seleccionado = articulo %in% alimentos_seleccionados)

# ============================================================
# Resumen min/mediana/max de precio por gramo, por alimento
## (pedido de Sergio: "mapeo -- quedarnos con el menor precio
## por gramo (min, mediano, maximo)")
# ============================================================

resumen_precio_gramo <- data_all %>%
  group_by(articulo, grupos_gabas, seleccionado) %>%
  summarise(
    n         = n(),
    min_pg    = min(precio_gramo, na.rm = TRUE),
    mediana_pg = median(precio_gramo, na.rm = TRUE),
    max_pg    = max(precio_gramo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(min_pg)

# ============================================================
# Guardar
# ============================================================

saveRDS(
  list(
    all              = data_all,
    selected_foods   = alimentos_seleccionados,
    excluded_food    = ALIMENTO_EXCLUIR,
    resumen          = resumen_precio_gramo
  ),
  file.path(shiny_dir, "data_boxplots.rds")
)

cat("\nListo. data_boxplots.rds guardado en", shiny_dir, "\n")
