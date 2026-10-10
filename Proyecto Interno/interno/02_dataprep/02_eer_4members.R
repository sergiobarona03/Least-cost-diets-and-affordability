########################################################
## SCRIPT 02_dataprep/02_eer_4members.R
## Prepara los inputs de EER para el hogar de 4 miembros: hombre adulto
## 31-50, mujer gestante 31-50, adolescente mujer 14-18 y niña 9-13.
## Toma como insumo el agregado de EER que produce 03-EER-agregados.R y el
## incremento de energia de la gestacion de 05-EER-gestantes-mayores.R.
##
## Representative household:
##   Adult male        31-50
##   Pregnant female   31-50
##   Female adolescent 14-18
##   Female child      9-13
##
## Incremento de energia de la gestacion:
##   base: promedio de 0, 340 y 452 kcal/dia (trimestres 1, 2 y 3) ponderado por el
##         trimestre de las gestantes de la ENSIN 2015
##   VARIANTE_HOGAR = "t3": 452 kcal/dia, el del tercer trimestre, como en
##         Yoshioka et al. (2025); archivos con sufijo _4m_t3
## VARIANTE_HOGAR = "esc": los gramos de proteina, lipidos y carbohidratos de FoodpriceR
##         se escalan a la energia de cada miembro (sufijo _4m_esc)
##
## Reads:  eer_output_dir/230726_agg_eer.xlsx
##         eer_output_dir/041026_gestantes_incremento.rds
##         FoodpriceR::EER_LL
##         FoodpriceR::UL
##
## Writes: household_dir/household_eer_4m.rds
##         household_dir/household_eer_ll_4m.rds
##         household_dir/household_ul_4m.rds
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
  "Mujer gestante (31-50)",
  "Adolescente mujer (14-18)",
  "Niña (9-13)"
)

variante_hogar <- Sys.getenv("VARIANTE_HOGAR", unset = "")
stopifnot(variante_hogar %in% c("", "t3", "esc"))
sufijo <- paste0("_4m", if (nzchar(variante_hogar)) paste0("_", variante_hogar))

# -----------------------------------------------------------------------
# Insumos
# -----------------------------------------------------------------------

message("Preparing EER inputs...")

agg_eer <- read_excel(file.path(eer_output_dir, "230726_agg_eer.xlsx")) %>%
  mutate(cod_mun = as.character(cod_mun))

gest <- readRDS(file.path(eer_output_dir, "041026_gestantes_incremento.rds"))
incremento <- if (variante_hogar == "t3") 452 else gest$incremento

insumos <- insumos_eer_hogar(miembros, agg_eer, incremento,
                             eer_ll_base = FoodpriceR::EER_LL,
                             ul_base     = FoodpriceR::UL,
                             escalar_macros = variante_hogar == "esc")

message(sprintf("  %d household members x %d cities | incremento de gestacion: %.0f kcal/dia",
                length(miembros), n_distinct(insumos$household_eer$ciudad), incremento))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

saveRDS(insumos$household_eer,    file.path(household_dir, paste0("household_eer", sufijo, ".rds")))
saveRDS(insumos$household_eer_ll, file.path(household_dir, paste0("household_eer_ll", sufijo, ".rds")))
saveRDS(insumos$household_ul,     file.path(household_dir, paste0("household_ul", sufijo, ".rds")))

message("Done. household_eer", sufijo, ", household_eer_ll", sufijo, " y household_ul", sufijo,
        " guardados en ", household_dir)
