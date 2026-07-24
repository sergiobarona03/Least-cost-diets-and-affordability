########################################################
## SCRIPT 01_webscrap_prep/00_csv_a_rds.R
## Consolida los csv crudos del scraping (uno por día) en un
## solo .rds comprimido por carpeta. Detecta automáticamente
## las subcarpetas de raw_input_dir, así que al pasar de meses
## sueltos a un trimestre con 13 ciudades no hay que tocar nada.
##
## Reads:  panel_dir/input panel/agosto-julio.septiembre.csv
## Writes: panel_dir/data/agosto-julio.septiembre.rds
########################################################

library(tidyverse)
library(janitor)

# ============================================================
# Rutas
# ============================================================

base_dir  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
panel_dir <- file.path(base_dir, "01_webscrap_prep")

raw_input_dir   <- file.path("C:/Users/danie/OneDrive/Documentos/input panel")
raw_dir <- file.path(panel_dir, "data")

dir.create(raw_mensual_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Detectar subcarpetas disponibles
# ============================================================

carpetas <- list.dirs(raw_input_dir, recursive = FALSE, full.names = TRUE)

if (length(carpetas) == 0) {
  stop("No se encontraron subcarpetas dentro de raw_input_dir: ", raw_input_dir)
}

cat("Carpetas detectadas:\n")
print(basename(carpetas))

# ============================================================
# Función: consolidar todos los csv de una carpeta en un solo df
# ============================================================

consolidar_csv <- function(ruta_carpeta) {
  
  archivos_csv <- list.files(
    path = ruta_carpeta,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  if (length(archivos_csv) == 0) {
    warning("No se encontraron csv en: ", ruta_carpeta)
    return(NULL)
  }
  
  map_dfr(archivos_csv, function(archivo) {
    read_csv(
      file = archivo,
      locale = locale(encoding = "UTF-8"),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) %>%
      clean_names() %>%
      mutate(archivo_origen = basename(archivo))
  })
}

# ============================================================
# Consolidar y guardar cada carpeta como rds comprimido
# ============================================================

for (ruta_carpeta in carpetas) {
  
  nombre <- basename(ruta_carpeta)
  cat("\nProcesando:", nombre, "...\n")
  
  datos <- consolidar_csv(ruta_carpeta)
  
  if (!is.null(datos)) {
    saveRDS(
      datos,
      file = file.path(raw_dir, paste0(nombre, ".rds")),
      compress = "xz"
    )
    cat("  ->", nrow(datos), "filas guardadas en", paste0(nombre, ".rds"), "\n")
  }
}

cat("\nListo. RDS guardados en:", raw_dir, "\n")