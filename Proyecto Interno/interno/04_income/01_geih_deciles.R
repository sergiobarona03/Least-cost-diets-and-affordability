########################################################
## SCRIPT 04_income/01_geih_deciles.R
## Deciles de ingreso per capita y gasto en alimentos de los hogares de
## las 13 ciudades, por mes del trimestre jul-sep 2025, a partir de la
## GEIH con el modulo de pobreza monetaria (personas y hogares).
##
## Los dos archivos de entrada (los crea 00_pobreza_csv_a_rds.R) van en
## 04_income/input/.
##
## Reads:  input_dir/personas_pobreza_2025.rds
##         input_dir/hogares_pobreza_2025.rds
##
## Writes: income_dir/deciles_2025.rds
##         income_dir/deciles_2025.csv
##         income_dir/pobreza_2025.rds
########################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(Hmisc)

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

income_dir <- file.path(base_dir, "interno/04_income")
input_dir  <- file.path(income_dir, "input")

dir.create(income_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Parametros
# ============================================================

ANIO  <- 2025
MESES <- 7:9

CIUDADES <- c("BARRANQUILLA", "BOGOTA", "BUCARAMANGA", "CALI", "CARTAGENA",
              "CUCUTA", "IBAGUE", "MANIZALES", "MEDELLIN", "MONTERIA",
              "PASTO", "PEREIRA", "VILLAVICENCIO")

# Participacion del gasto en alimentos en el ingreso del hogar, por par de
# deciles. Son las del paper (review-food-security-paper); hay que confirmar
# de que encuesta salen y si siguen vigentes para 2025.
SHARE_ALIMENTOS <- tibble::tribble(
  ~deciles, ~share,
  1, 0.39, 2, 0.39,
  3, 0.36, 4, 0.36,
  5, 0.35, 6, 0.35,
  7, 0.32, 8, 0.32,
  9, 0.26, 10, 0.26
)

normalizar_dominio <- function(x) {
  toupper(iconv(trimws(x), from = "", to = "ASCII//TRANSLIT"))
}

# Decil ponderado de cada observacion. Los extremos de los cortes son -Inf e
# Inf para que el ingreso maximo no quede por fuera del ultimo decil.
asignar_decil <- function(x, w) {
  cortes <- wtd.quantile(x, weights = w, probs = seq(0, 1, 0.1), na.rm = TRUE)
  cortes[c(1, length(cortes))] <- c(-Inf, Inf)
  as.integer(cut(x, breaks = cortes, labels = 1:10, right = TRUE))
}

# ============================================================
# Cargar personas y hogares
# ============================================================

message("Loading inputs...")

personas <- readRDS(file.path(input_dir, sprintf("personas_pobreza_%d.rds", ANIO))) %>%
  clean_names()
hogares <- readRDS(file.path(input_dir, sprintf("hogares_pobreza_%d.rds", ANIO))) %>%
  clean_names()

renombrar <- function(df, de, a) {
  if (de %in% names(df) && !(a %in% names(df))) names(df)[names(df) == de] <- a
  df
}
personas <- renombrar(personas, "dpto", "depto")
hogares  <- hogares %>% renombrar("dpto", "depto") %>% renombrar("ingtotarr", "ingtotugarr")

hogares_aux <- hogares %>%
  mutate(id_hogar = paste0(directorio, "-", secuencia_p),
         nper = as.numeric(nper), npersug = as.numeric(npersug),
         dominio = normalizar_dominio(dominio)) %>%
  filter(clase == 1, dominio %in% CIUDADES, as.numeric(mes) %in% MESES) %>%
  select(year, mes, dominio, id_hogar, nper, npersug, ingtotug, ingtotugarr,
         ingpcug, li, lp, pobre, indigente)

personas_aux <- personas %>%
  mutate(id_hogar = paste0(directorio, "-", secuencia_p),
         id = paste0(directorio, "-", secuencia_p, "-", orden),
         dominio = normalizar_dominio(dominio)) %>%
  filter(clase == 1, dominio %in% CIUDADES, as.numeric(mes) %in% MESES) %>%
  select(year, mes, id, id_hogar, clase, dominio, depto, ingtot, fex_c)

datos <- merge(personas_aux, hogares_aux, by = c("year", "mes", "dominio", "id_hogar")) %>%
  mutate(across(c(fex_c, ingpcug, ingtotugarr, lp, li),
                ~ as.numeric(gsub(",", ".", .x))),
         pobre = as.numeric(pobre), indigente = as.numeric(indigente))

faltan <- setdiff(CIUDADES, unique(datos$dominio))
if (length(faltan) > 0) {
  stop("Sin hogares en: ", paste(faltan, collapse = ", "))
}

# ============================================================
# Deciles ponderados por ciudad x mes y gasto en alimentos
# ============================================================

ingreso <- datos %>%
  mutate(fecha = ymd(paste(year, mes, "01", sep = "-"))) %>%
  group_by(dominio, year, mes) %>%
  mutate(deciles = asignar_decil(ingpcug, fex_c)) %>%
  ungroup() %>%
  left_join(SHARE_ALIMENTOS, by = "deciles") %>%
  mutate(
    food_exp          = share * ingtotugarr,
    food_exp_pc       = food_exp / npersug,
    food_exp_pc_month = food_exp_pc,
    food_exp_pc_year  = food_exp_pc * 12,
    per_capita_income = ingpcug,
    income            = ingtotugarr,
    nug               = as.numeric(npersug),
    deciles_label     = paste0("Decil ", deciles)
  ) %>%
  select(year, mes, fecha, dominio, id_hogar, nug, income, per_capita_income,
         deciles, deciles_label, share, food_exp, food_exp_pc, food_exp_pc_month,
         food_exp_pc_year, fex_c)

stopifnot(!anyNA(ingreso$deciles), !anyNA(ingreso$share))

# ============================================================
# Pobreza monetaria (para contrastar con las cifras oficiales)
# ============================================================

pobreza <- datos %>%
  mutate(fecha = ymd(paste(year, mes, "01", sep = "-")),
         bajo_lp = if_else(ingpcug < lp, 1, 0),
         bajo_li = if_else(ingpcug < li, 1, 0)) %>%
  group_by(fecha, year, mes, dominio) %>%
  summarise(pm  = 100 * sum(bajo_lp * fex_c, na.rm = TRUE) / sum(fex_c, na.rm = TRUE),
            pme = 100 * sum(bajo_li * fex_c, na.rm = TRUE) / sum(fex_c, na.rm = TRUE),
            hogares = n_distinct(id_hogar), .groups = "drop")

message(sprintf("  %d hogares x persona-fila | %d ciudades | meses: %s",
                n_distinct(ingreso$id_hogar), n_distinct(ingreso$dominio),
                paste(sort(unique(ingreso$mes)), collapse = ", ")))

# ============================================================
# Save
# ============================================================

saveRDS(ingreso, file.path(income_dir, "deciles_2025.rds"))
write_csv(ingreso, file.path(income_dir, "deciles_2025.csv"))
saveRDS(pobreza, file.path(income_dir, "pobreza_2025.rds"))

message("Done. Run 02_asequibilidad.R next.")
