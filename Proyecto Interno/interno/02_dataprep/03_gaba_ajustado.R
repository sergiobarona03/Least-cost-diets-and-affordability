########################################################
## SCRIPT 02_dataprep/03_gaba_ajustado.R
## Ajusta los intercambios GABA al EER de cada ciudad para los
## miembros del hogar representativo.
## factor_ajuste = EER de la ciudad / recomendacion energetica GABA
##
## Hogar de 3 miembros por defecto; con la variable de entorno N_MIEMBROS = 4 o 5
## usa el hogar de 02_eer_4members.R o 02_eer_5members.R y agrega el sufijo
## _4m o _5m a los archivos.
##
## Reads:  output_dir/gabas/gaba_exchanges_base.rds
##         output_dir/gabas/gaba_energy_base.rds
##         household_dir/household_eer<sufijo>.rds
##
## Writes: output_dir/gabas/gaba_exchanges_adj<sufijo>.rds
##         output_dir/gabas/gaba_exchanges_adj<sufijo>.xlsx
########################################################

library(tidyverse)
library(writexl)

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

output_dir    <- file.path(base_dir, "interno/output")
household_dir <- file.path(base_dir, "interno/02_dataprep/household eer")

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------

message("Loading inputs...")

# Tamano del hogar: 3 (base), 4 o 5 miembros
n_miembros   <- as.integer(Sys.getenv("N_MIEMBROS", unset = "3"))
stopifnot(n_miembros %in% 3:5)
# VARIANTE_HOGAR pruebas de sensibilidad de los requerimientos; vacia es la version base:
#   "t3"  hogar de 4 con el incremento de energia del tercer trimestre de la gestacion
#   "esc" minimos y maximos de macronutrientes escalados a la energia de cada miembro
variante_hogar <- Sys.getenv("VARIANTE_HOGAR", unset = "")
stopifnot(variante_hogar %in% c("", "t3", "esc"), !(variante_hogar == "t3" && n_miembros != 4L))
sufijo_hogar <- paste0(if (n_miembros == 3L) "" else paste0("_", n_miembros, "m"),
                       if (nzchar(variante_hogar)) paste0("_", variante_hogar))

exchanges_base <- readRDS(file.path(output_dir, "gabas/gaba_exchanges_base.rds"))
energy_base    <- readRDS(file.path(output_dir, "gabas/gaba_energy_base.rds"))
household_eer  <- readRDS(file.path(household_dir, paste0("household_eer", sufijo_hogar, ".rds")))

# Grupos de edad del EER en los grupos de edad de GABA
map_edad <- tribble(
  ~rango,         ~edad,
  "[10, 14)",     "10 to 13",
  "[14,19)",      "14 to 17",
  "[31,51)",      "19 to 59",
  "[31,51) gest", "19 to 59",
  "[51,70)",      "> 60"
)

map_sexo <- tribble(
  ~sexo,    ~sex,
  "male",   "Masculino",
  "female", "Femenino"
)

# -----------------------------------------------------------------------
# Factor de ajuste y intercambios ajustados
# -----------------------------------------------------------------------

eer_gaba <- energy_base %>%
  filter(tipo_fila == "recomendacion") %>%
  inner_join(map_sexo, by = "sexo") %>%
  select(edad, sex, eer_gabas = energia_kcal)

exchanges <- exchanges_base %>%
  inner_join(map_sexo, by = "sexo") %>%
  select(edad, sex, grupo_principal, n_exchanges, e_kcal)

exchanges_adj <- household_eer %>%
  left_join(map_edad, by = "rango") %>%
  left_join(eer_gaba, by = c("edad", "sex")) %>%
  mutate(factor_ajuste = eer / eer_gabas) %>%
  left_join(exchanges, by = c("edad", "sex"), relationship = "many-to-many") %>%
  mutate(
    n_exchanges_adj = n_exchanges * factor_ajuste,
    e_kcal_adj      = e_kcal * factor_ajuste
  ) %>%
  select(cod_mun, ciudad, sex, rango, grupo_principal,
         n_exchanges, e_kcal, factor_ajuste, n_exchanges_adj, e_kcal_adj)

message(sprintf("  exchanges_adj: %d rows | %d cities",
                nrow(exchanges_adj), n_distinct(exchanges_adj$ciudad)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

saveRDS(exchanges_adj, file.path(output_dir, paste0("gabas/gaba_exchanges_adj", sufijo_hogar, ".rds")))
write_xlsx(exchanges_adj, file.path(output_dir, paste0("gabas/gaba_exchanges_adj", sufijo_hogar, ".xlsx")))

message("Done. Run 03_models/03_cord.R next.")
