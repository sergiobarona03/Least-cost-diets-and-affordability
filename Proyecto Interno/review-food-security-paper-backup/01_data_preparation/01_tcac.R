########################################################
## SCRIPT 01_data_preparation/01_tcac_nutrition.R
## Prepara la tabla maestra de composición nutricional
## (TCAC) — time-invariant, independiente de precios.
##
## Reads:  BASE_DIR/composicion-nut/Copia_DANE_4_DIC_2025act.xlsx
## Writes: PREP_DIR/tcac_master.rds
##         PREP_DIR/tcac_master.csv
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(janitor)

# -----------------------------------------------------------------------
# Input
# -----------------------------------------------------------------------
in_tcac <- file.path(BASE_DIR,
                     "composicion-nut/Copia_DANE_4_DIC_2025act.xlsx")

# -----------------------------------------------------------------------
# Process TCAC
# -----------------------------------------------------------------------
message("Processing TCAC nutritional composition table...")

tcac_raw <- read_excel(in_tcac) %>% clean_names()

tcac_master <- tcac_raw %>%
  rename(articulo = articulo_dane) %>%
  mutate(
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
    articulo        = as.character(articulo),
    grupos_gabas    = as.character(grupos_gabas),
    subgrupos_gabas = as.character(subgrupos_gabas)
  ) %>%
  select(any_of(c(
    "codigo_articulo", "articulo",
    "parte_comestible_percent",
    "energia_kcal", "proteina_g", "lipidos_g",
    "carbohidratos_totales_g",
    "vitamina_c_mg", "folatos_mcg", "vitamina_a_er",
    "tiamina_mg", "riboflavina_mg", "niacina_mg",
    "vitamina_b12_mcg", "magnesio_mg", "fosforo_mg",
    "sodio_mg", "calcio_mg", "hierro_mg", "zinc_mg",
    "grupos_gabas", "subgrupos_gabas",
    "gramos_g_1_intercambio_1_intercambio",
    "subclase_ipc"
  ))) %>%
  distinct()

message(sprintf("  %d food items in TCAC", nrow(tcac_master)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
saveRDS(tcac_master, file.path(PREP_DIR, "tcac_master.rds"))
write_csv(tcac_master, file.path(PREP_DIR, "tcac_master.csv"))

message("Done. Run 02_food_table.R next.")
