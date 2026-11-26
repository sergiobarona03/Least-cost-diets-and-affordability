########################################################
## SCRIPT 02_dataprep/eer/07-comparacion-hierro-zinc.R
## Compara los minimos de hierro y zinc que trae FoodpriceR (EER_LL)
## con el EAR y el RDA del IOM (2001) para los grupos de 31 a 50 anos.
##
## Los valores del IOM se copian de las tablas de las Dietary Reference Intakes
## (IOM, 2001, Dietary Reference Intakes for Vitamin A, Vitamin K, Arsenic, Boron,
## Chromium, Copper, Iodine, Iron, Manganese, Molybdenum, Nickel, Silicon,
## Vanadium, and Zinc; National Academies Press, https://doi.org/10.17226/10026).
##
## Reads:  FoodpriceR::EER_LL
## Writes: eer/output/101026_comparacion_hierro_zinc.xlsx/.rds
########################################################

library(tidyverse)
library(FoodpriceR)
library(writexl)

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)
base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("Ninguno de los directorios existe")
out_dir <- file.path(base_dir, "interno/02_dataprep/eer/output")

# EAR y RDA del IOM (mg/dia), 31 a 50 anos
iom <- tribble(
  ~grupo,                      ~Sex, ~Age,                      ~hierro_EAR, ~hierro_RDA, ~zinc_EAR, ~zinc_RDA,
  "Hombre 31-50",              0,    "31 a 50 a\u00f1os",       6.0,         8,           9.4,       11,
  "Mujer 31-50",               1,    "31 a 50 a\u00f1os",       8.1,         18,          6.8,       8,
  "Gestante 31-50",            1,    "gestantes 31 a 50 a\u00f1os", 22,      27,          9.5,       11,
  "Lactante 31-50",            1,    "lactantes 31 a 50 a\u00f1os", 6.5,     9,           10.4,      12
)

fp <- FoodpriceR::EER_LL %>%
  select(Sex, Age, hierro_paquete = Iron, zinc_paquete = Zinc)

comp <- iom %>%
  left_join(fp, by = c("Sex", "Age")) %>%
  mutate(razon_hierro = hierro_paquete / hierro_EAR,
         razon_zinc   = zinc_paquete / zinc_EAR) %>%
  select(grupo, hierro_paquete, hierro_EAR, hierro_RDA, razon_hierro,
         zinc_paquete, zinc_EAR, zinc_RDA, razon_zinc)

stopifnot(!anyNA(comp$hierro_paquete))
print(comp, width = Inf)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(comp, file.path(out_dir, "101026_comparacion_hierro_zinc.rds"))
write_xlsx(list(comparacion = comp), file.path(out_dir, "101026_comparacion_hierro_zinc.xlsx"))
