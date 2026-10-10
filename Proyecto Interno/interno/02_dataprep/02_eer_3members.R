########################################################
## SCRIPT 02_dataprep/02_eer_3members.R
## Prepara los inputs de EER para el hogar representativo de
## 3 miembros (adulto hombre 31-50, adulta mujer 31-50, niña
## 9-13). Toma como insumo el agregado de EER que
## produce 03-EER-agregados.R.
##
## Representative household:
##   Adult male   31-50
##   Adult female 31-50
##   Female child 9-13 (el EER se promedia sobre las edades de 10 a 13 anos,
##                      como en el review; los limites de nutrientes son los de 9 a 13)
##
## Variable de entorno VARIANTE_HOGAR:
##   vacia: version base (minimos y maximos de macronutrientes de FoodpriceR en gramos)
##   "esc": los gramos de proteina, lipidos y carbohidratos se escalan a la energia de
##          cada miembro (prueba de sensibilidad); archivos con sufijo _esc
##
## Reads:  eer_output_dir/230726_agg_eer.xlsx
##         FoodpriceR::EER_LL
##         FoodpriceR::UL
##
## Writes: household_dir/household_eer.rds
##         household_dir/household_eer_ll.rds
##         household_dir/household_ul.rds
########################################################

library(tidyverse)
library(readxl)
library(FoodpriceR)

# ============================================================
# Rutas
# ============================================================

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

eer_output_dir <- file.path(base_dir, "interno/02_dataprep/eer/output")
household_dir  <- file.path(base_dir, "interno/02_dataprep/household eer")

dir.create(household_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(base_dir, "interno/02_dataprep/aux-functions/eer_hogar.R"), encoding = "UTF-8")

# -----------------------------------------------------------------------
# Miembros del hogar
# -----------------------------------------------------------------------

miembros <- c(
  "Hombre adulto (31-50)",
  "Mujer adulta (31-50)",
  "Niña (9-13)"
)

variante_hogar <- Sys.getenv("VARIANTE_HOGAR", unset = "")
stopifnot(variante_hogar %in% c("", "esc"))
sufijo <- if (nzchar(variante_hogar)) paste0("_", variante_hogar) else ""

# -----------------------------------------------------------------------
# Insumos
# -----------------------------------------------------------------------

message("Preparing EER inputs...")

agg_eer <- read_excel(file.path(eer_output_dir, "230726_agg_eer.xlsx")) %>%
  mutate(cod_mun = as.character(cod_mun))

# El hogar de 3 no tiene gestante: el incremento de energia de la gestacion no se usa
insumos <- insumos_eer_hogar(miembros, agg_eer, incremento_gestacion = 0,
                             eer_ll_base = FoodpriceR::EER_LL,
                             ul_base     = FoodpriceR::UL,
                             escalar_macros = variante_hogar == "esc")

message(sprintf("  %d household members x %d cities",
                length(miembros), n_distinct(insumos$household_eer$ciudad)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

saveRDS(insumos$household_eer,    file.path(household_dir, paste0("household_eer", sufijo, ".rds")))
saveRDS(insumos$household_eer_ll, file.path(household_dir, paste0("household_eer_ll", sufijo, ".rds")))
saveRDS(insumos$household_ul,     file.path(household_dir, paste0("household_ul", sufijo, ".rds")))

message("Done. household_eer", sufijo, ", household_eer_ll", sufijo, " y household_ul", sufijo,
        " guardados en ", household_dir)
