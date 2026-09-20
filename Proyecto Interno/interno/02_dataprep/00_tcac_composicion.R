########################################################
## SCRIPT 02_dataprep/00_tcac_composicion.R
## Cruza la lista total de alimentos (output de 01_webscrap_prep)
## con la tabla TCAC de composición nutricional y guarda la
## composición nutricional por alimento (sipsa_name).
##
## Reads:  output_dir/lista_alimentos/lista_total_alimentos.xlsx
##         proyecto_dir/composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx
##         01_webscrap_prep/aux-functions/mapeo_tcac.R
## Writes: output_dir/tcac/composicion_270726.xlsx
##         output_dir/tcac/composicion_270726.rds
########################################################

library(tidyverse)
library(openxlsx)
library(stringi)
library(janitor)

# ============================================================
# Rutas
# ============================================================

proyecto_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
interno_dir  <- file.path(proyecto_dir, "interno")
output_dir   <- file.path(interno_dir, "output")

ruta_lista  <- file.path(output_dir, "lista_alimentos")
ruta_tcac   <- file.path(proyecto_dir, "composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx")
ruta_output <- file.path(output_dir, "tcac")

dir.create(ruta_output, recursive = TRUE, showWarnings = FALSE)

source(file.path(interno_dir, "01_webscrap_prep/aux-functions/mapeo_tcac.R"), encoding = "UTF-8")

# ============================================================
# Cargar lista total de alimentos y cruzar con TCAC
# ============================================================

lista_total <- read.xlsx(file.path(ruta_lista, "lista_total_alimentos.xlsx"))

tcac <- leer_mapeo_tcac(ruta_tcac)

lista_con_nut <- unir_mapeo_tcac(lista_total, tcac)

# ============================================================
# Guardar outputs
# ============================================================

lista_con_nut_clean <- lista_con_nut %>%
  clean_names()

write.xlsx(lista_con_nut_clean,
           file.path(ruta_output, "composicion_270726.xlsx"),
           overwrite = TRUE)

saveRDS(lista_con_nut_clean,
        file.path(ruta_output, "composicion_270726.rds"))

cat("\nListo. composicion_270726.rds guardado en", ruta_output, "\n")
