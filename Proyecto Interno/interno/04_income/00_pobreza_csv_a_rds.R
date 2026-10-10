########################################################
## SCRIPT 04_income/00_pobreza_csv_a_rds.R
## Convierte los microdatos de pobreza monetaria del DANE (GEIH) de un
## anio, uno de hogares y otro de personas, de csv a rds en 04_income/input/.
## Es el mismo procedimiento con el que se prepararon los archivos de
## 2018 a 2024 del paper (Pobreza/hogares/poverty_h.R y
## Pobreza/personas/poverty_p.R).
##
## Los csv se descargan del DANE (Pobreza monetaria > microdatos > anio)
## y se guardan, con el anio en el nombre del archivo, en:
##   csv_hogares_dir:  Documentos/Pobreza/Hogares/
##   csv_personas_dir: Documentos/Pobreza/Personas/
##
## Reads:  csv_hogares_dir/<archivo con el anio en el nombre>.csv
##         csv_personas_dir/<archivo con el anio en el nombre>.csv
##
## Writes: input_dir/hogares_pobreza_<anio>.rds
##         input_dir/personas_pobreza_<anio>.rds
########################################################

library(tidyverse)
library(readr)

# ============================================================
# Rutas y parametros
# ============================================================

base_dir  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
input_dir <- file.path(base_dir, "04_income/input")

csv_hogares_dir  <- "C:/Users/danie/OneDrive/Documentos/Pobreza/Hogares"
csv_personas_dir <- "C:/Users/danie/OneDrive/Documentos/Pobreza/Personas"

ANIO <- 2025

dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Un csv por anio: todo como texto y las variables P#### a numero
# ============================================================

convertir <- function(carpeta, prefijo) {

  archivos <- tibble(archivo = list.files(carpeta, pattern = "\\.csv$", full.names = TRUE)) %>%
    mutate(anio = parse_number(basename(archivo))) %>%
    filter(anio == ANIO)

  if (nrow(archivos) != 1) {
    stop("En ", carpeta, " hay ", nrow(archivos), " archivos csv de ", ANIO,
         " y debe haber uno.")
  }

  datos <- read_delim(
    file = archivos$archivo,
    delim = NULL,
    col_types = cols(.default = col_character()),
    na = c("", "NA", "N/A", ".", " ", "NULL"),
    show_col_types = FALSE
  ) %>%
    mutate(Year = ANIO) %>%
    mutate(across(matches("^(P|p)\\d{4,}$"), ~ parse_number(.x)))

  saveRDS(datos, file.path(input_dir, sprintf("%s_pobreza_%d.rds", prefijo, ANIO)))
  message(sprintf("%s: %d filas, %d columnas", prefijo, nrow(datos), ncol(datos)))
}

convertir(csv_hogares_dir,  "hogares")
convertir(csv_personas_dir, "personas")

message("Done. Run 01_geih_deciles.R next.")
