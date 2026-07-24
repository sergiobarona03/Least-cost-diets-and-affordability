########################################################
## SCRIPT 01_webscrap_prep/02b_panel_balanceado.R
## Arma el panel balanceado: se queda solo con las combinaciones
## ciudad/alimento que cumplieron el umbral de fechas en 02a.
##
## Reads:  output_panel_dir/panel_v2.rds
##         output_panel_dir/lista_por_ciudad.rds
## Writes: output_panel_dir/panel_balanceado.rds
########################################################

library(tidyverse)
library(dplyr)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"

output_dir       <- file.path(base_dir, "output")
output_panel_dir <- file.path(output_dir, "paneles")

# ============================================================
# Cargar insumos (output de 02a_lista_alimentos.R)
# ============================================================

panel_filtrado   <- readRDS(file.path(output_panel_dir, "raw_mensual/panel_v2.rds"))
lista_por_ciudad <- readRDS(file.path(output_panel_dir, "raw_mensual/lista_por_ciudad.rds"))

# ============================================================
# Panel balanceado
# ============================================================

panel_balanceado <- panel_filtrado %>%
  semi_join(lista_por_ciudad, by = c("city", "sku_code", "sipsa_name"))

cat("\nFilas panel balanceado:", nrow(panel_balanceado), "\n")
cat("Ciudades:", n_distinct(panel_balanceado$city), "\n")
cat("Alimentos:", n_distinct(panel_balanceado$sipsa_name), "\n")

# ============================================================
# Guardar
# ============================================================

saveRDS(panel_balanceado, file.path(output_panel_dir, "panel_balanceado.rds"))

cat("\nListo. panel_balanceado_final.rds guardado en", output_panel_dir, "\n")