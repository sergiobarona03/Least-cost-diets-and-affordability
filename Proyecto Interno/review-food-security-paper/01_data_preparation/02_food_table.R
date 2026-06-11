########################################################
## SCRIPT 01_data_preparation/02_food_table.R
## Une precios estimados (S5) con composición nutricional
## para producir el panel de alimentos del paper.
##
## Fuente de precios: FORECAST_DIR/prices_paper.csv
##   → construido en 00_pipeline/05_build_prices.R
##   → precios en COP/500g, período 2019–2024
##   → NO usa prices_extended_city_article_month.csv
##
## Reads:  FORECAST_DIR/prices_paper.csv
##         PREP_DIR/tcac_master.rds
##
## Writes: PREP_DIR/panel_food_paper.rds   ← input para modelos
##         PREP_DIR/panel_food_paper.csv
##         PREP_DIR/appendix_food_items.xlsx  ← tabla apéndice
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)

# -----------------------------------------------------------------------
# Foods to exclude (ultra-processed, condiments, composites)
# -----------------------------------------------------------------------
FOODS_EXCLUDE <- c(
  "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO",
  "CHOCOLATE INSTANTANEO", "CHORIZO",
  "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS",
  "HELADOS DE CREMA", "JAMÓN",
  "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MAYONESA", "MERMELADA", "MORTADELA",
  "PIZZA", "SALCHICHAS", "SALCHICHÓN", "SALSA DE TOMATE",
  "SOPAS", "YOGOURT", "CREMA DE LECHE", "PAPAS FRITAS",
  "CILANTRO", "COLOR", "COMINOS", "LAUREL", "MOSTAZA",
  "PIMIENTA", "TOMILLO", "REVUELTO VERDE",
  "ALMUERZO CORRIENTE O EJECUTIVO",
  "ALMUERZO ESPECIAL O A LA CARTA",
  "CHOCOLATE EN PASTA", "CAFÉ INSTANTANEO", "CAFÉ MOLIDO",
  "COMBOS", "CREMAS", "TINTO",
  "HAMBURGUESA", "KUMIS", "JUGOS NATURALES", "SUERO",
  "ARROZ PARA SOPA"
)

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

# Prices from 00_pipeline/05_build_prices.R
# precio_500g = S5 extrapolated price (COP/500g)
prices_paper <- read_csv(
  file.path(FORECAST_DIR, "prices_paper.csv"),
  show_col_types = FALSE) %>%
  mutate(fecha = as.Date(fecha))

# Nutritional composition (time-invariant)
tcac_master <- readRDS(file.path(PREP_DIR, "tcac_master.rds"))

message(sprintf(
  "  prices_paper: %d rows | tcac: %d items",
  nrow(prices_paper), nrow(tcac_master)))

# -----------------------------------------------------------------------
# Normalise city names to match model expectations
# -----------------------------------------------------------------------
normalise_city <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  case_when(
    x == "BOGOTA D.C." ~ "BOGOTA",
    x == "BOGOTÁ D.C." ~ "BOGOTA",
    x == "MEDELLIN"    ~ "MEDELLIN",
    x == "MEDELLÍN"    ~ "MEDELLIN",
    x == "CALI"        ~ "CALI",
    TRUE               ~ x)
}

# -----------------------------------------------------------------------
# Merge prices + nutritional composition
# -----------------------------------------------------------------------
message("Building food panel...")

panel <- prices_paper %>%
  mutate(ciudad = normalise_city(ciudad)) %>%
  # Drop subclase_ipc from prices — use the one from tcac_master
  select(-any_of("subclase_ipc")) %>%
  left_join(tcac_master %>% select(-codigo_articulo),
            by = "articulo") %>%
  # Compute precio_100g via edible portion
  mutate(
    pc = case_when(
      is.na(parte_comestible_percent)  ~ NA_real_,
      parte_comestible_percent > 1     ~ parte_comestible_percent / 100,
      TRUE                             ~ parte_comestible_percent),
    precio_100g = (precio_500g / 5) / pc,
    ano     = year(fecha),
    mes_num = month(fecha),
    gramos_g_1_intercambio_1_intercambio =
      suppressWarnings(
        as.numeric(gramos_g_1_intercambio_1_intercambio))
  ) %>%
  # Remove excluded foods
  filter(!articulo %in% FOODS_EXCLUDE) %>%
  # Keep only rows with valid price and energy
  filter(!is.na(precio_100g), precio_100g > 0,
         !is.na(energia_kcal)) %>%
  # Paper period filter (redundant but explicit)
  filter(fecha >= PAPER_START, fecha <= PAPER_END)

message(sprintf(
  "  Panel: %d rows | %d items | %d cities | %s–%s",
  nrow(panel),
  n_distinct(panel$articulo),
  n_distinct(panel$ciudad),
  min(panel$fecha), max(panel$fecha)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
saveRDS(panel, file.path(PREP_DIR, "panel_food_paper.rds"))
write_csv(panel, file.path(PREP_DIR, "panel_food_paper.csv"))

message(sprintf("  Saved to: %s", PREP_DIR))

# -----------------------------------------------------------------------
# Appendix table: food items with nutritional composition
# -----------------------------------------------------------------------
message("Building appendix food table...")

appendix_table <- panel %>%
  group_by(articulo) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    articulo, subclase_ipc,
    grupos_gabas, subgrupos_gabas,
    parte_comestible_percent,
    energia_kcal, proteina_g, lipidos_g,
    carbohidratos_totales_g,
    calcio_mg, hierro_mg, zinc_mg,
    magnesio_mg, fosforo_mg,
    vitamina_c_mg, tiamina_mg, riboflavina_mg,
    niacina_mg, folatos_mcg,
    vitamina_b12_mcg, vitamina_a_er, sodio_mg
  ) %>%
  arrange(grupos_gabas, subgrupos_gabas, articulo)

writexl::write_xlsx(appendix_table,
                    file.path(PREP_DIR, "appendix_food_items.xlsx"))

message("Done. Run 03_gaba_base.R next.")