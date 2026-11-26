########################################################
## SCRIPT 02_dataprep/03_gaba_ajustado.R
## Ajusta los intercambios de las GABA al EER de cada ciudad para los
## miembros de cada hogar y variante, en una sola corrida.
## factor_ajuste = EER de la ciudad / energia de referencia de las GABA
##
## Antes se corria una vez por hogar con las variables de entorno
## N_MIEMBROS y VARIANTE_HOGAR. Los archivos de salida conservan los
## mismos nombres.
##
## Reads:  output_dir/gabas/gaba_exchanges_base.rds
##         output_dir/gabas/gaba_energy_base.rds
##         household_dir/household_eer<sufijo>.rds
## Writes: output_dir/gabas/gaba_exchanges_adj<sufijo>.rds
##         output_dir/gabas/gaba_exchanges_adj<sufijo>.xlsx
##   con sufijo "", "_esc", "_4m", "_4m_t3", "_4m_esc", "_5m", "_5m_esc"
########################################################

library(tidyverse)
library(writexl)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/paper-global-food"

output_dir    <- file.path(base_dir, "output")
household_dir <- file.path(base_dir, "02_dataprep/household eer")

sufijos <- c("", "_esc", "_4m", "_4m_t3", "_4m_esc", "_5m", "_5m_esc")

# ============================================================
# Insumos comunes
# ============================================================

exchanges_base <- readRDS(file.path(output_dir, "gabas/gaba_exchanges_base.rds"))
energy_base    <- readRDS(file.path(output_dir, "gabas/gaba_energy_base.rds"))

# Grupos de edad del EER en los grupos de edad de las GABA
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

eer_gaba <- energy_base %>%
  filter(tipo_fila == "recomendacion") %>%
  inner_join(map_sexo, by = "sexo") %>%
  select(edad, sex, eer_gabas = energia_kcal)

exchanges <- exchanges_base %>%
  inner_join(map_sexo, by = "sexo") %>%
  select(edad, sex, grupo_principal, n_exchanges, e_kcal)

# ============================================================
# Factor de ajuste e intercambios ajustados, por hogar y variante
# ============================================================

for (sufijo in sufijos) {

  household_eer <- readRDS(file.path(household_dir, paste0("household_eer", sufijo, ".rds")))

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

  stopifnot(!anyNA(exchanges_adj$factor_ajuste))

  saveRDS(exchanges_adj, file.path(output_dir, paste0("gabas/gaba_exchanges_adj", sufijo, ".rds")))
  write_xlsx(exchanges_adj, file.path(output_dir, paste0("gabas/gaba_exchanges_adj", sufijo, ".xlsx")))

  message(sprintf("  %s: %d filas | %d ciudades",
                  if (nzchar(sufijo)) sufijo else "base",
                  nrow(exchanges_adj), n_distinct(exchanges_adj$ciudad)))
}

message("Done.")
