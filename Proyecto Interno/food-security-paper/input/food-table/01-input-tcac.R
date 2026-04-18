#################################################
## Preparación de la base de datos con la
## composición nutricional de cada alimento
################################################

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)
library(FoodpriceR)

# -----------------------------
# 1. Directorios
# -----------------------------

# Directorio base
dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

# Mapeo - composición nutricional
in_tcac_map <- file.path(
  base_dir,
  "composicion-nut/Copia_DANE_4_DIC_2025act.xlsx"
)

# Output path
out_dir   <- file.path(base_dir, "food-security-paper/output/tcac_food_table")

# Cities
cities_use <- c("CALI", "BOGOTÁ D.C.", "MEDELLÍN")

# Price scenario(s)
escenarios <- c("precio_100g")

# -----------------------------
# 2. Funciones auxiliares
# -----------------------------
safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# -----------------------------
# 3. Procesar TCAC
# -----------------------------
tcac_raw <- read_excel(in_tcac_map) %>%
  clean_names()

tcac <- tcac_raw %>%
  dplyr::rename(articulo = articulo_dane) %>%
  mutate(
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
    articulo = as.character(articulo),
    grupos_gabas = as.character(grupos_gabas),
    subgrupos_gabas = as.character(subgrupos_gabas)
  )

# Keep only needed columns (keep many nutrients; safe if extras exist)
keep_cols <- c(
  "codigo_articulo","articulo",
  "parte_comestible_percent",
  # energy + macro + micros (names from your earlier CoNA rename block)
  "energia_kcal","proteina_g","lipidos_g","carbohidratos_totales_g",
  "vitamina_c_mg","folatos_mcg","vitamina_a_er","tiamina_mg","riboflavina_mg",
  "niacina_mg","vitamina_b12_mcg","magnesio_mg","fosforo_mg","sodio_mg",
  "calcio_mg","hierro_mg","zinc_mg",
  # group CoRD
  "grupos_gabas", "subgrupos_gabas", "gramos_g_1_intercambio_1_intercambio"
)

tcac_master <- tcac %>%
  select(any_of(keep_cols)) %>%
  distinct()


saveRDS(tcac_master, file.path(out_dir, "tcac_master.rds"))
write_csv(tcac_master, file.path(out_dir, "tcac_master.csv"))

