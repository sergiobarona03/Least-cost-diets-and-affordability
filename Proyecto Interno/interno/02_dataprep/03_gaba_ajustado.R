########################################################
## SCRIPT 02_dataprep/03_gaba_ajustado.R
## Ajusta los intercambios GABA al EER de cada ciudad para los
## miembros del hogar representativo.
## factor_ajuste = EER de la ciudad / recomendacion energetica GABA
##
## Reads:  output_dir/gabas/gaba_exchanges_base.rds
##         output_dir/gabas/gaba_energy_base.rds
##         household_dir/household_eer.rds
##
## Writes: output_dir/gabas/gaba_exchanges_adj.rds
##         output_dir/gabas/gaba_exchanges_adj.xlsx
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

exchanges_base <- readRDS(file.path(output_dir, "gabas/gaba_exchanges_base.rds"))
energy_base    <- readRDS(file.path(output_dir, "gabas/gaba_energy_base.rds"))
household_eer  <- readRDS(file.path(household_dir, "household_eer.rds"))

# Grupos de edad del EER en los grupos de edad de GABA
map_edad <- tribble(
  ~rango,     ~edad,
  "[10, 14)", "10 to 13",
  "[31,51)",  "19 to 59"
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

saveRDS(exchanges_adj, file.path(output_dir, "gabas/gaba_exchanges_adj.rds"))
write_xlsx(exchanges_adj, file.path(output_dir, "gabas/gaba_exchanges_adj.xlsx"))

message("Done. Run 03_models/03_cord.R next.")
