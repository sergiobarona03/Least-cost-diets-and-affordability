# ============================================================
# DEFLECTAR PRECIOS SIPSA (mayoristas) 
# usando IPC DANE por GRUPO + CIUDAD + AÑO-MES
# Ciudades: CALI, MEDELLIN, BOGOTA, D.C.
# Base: 2018-12
# ============================================================

library(dplyr)
library(tidyverse)
library(lubridate)
library(fpp3)
library(stringi)
library(readxl)
library(writexl)
library(stringr)

# ---------------- RUTAS ----------------
base_dir   <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios al por mayor"
path_bases <- file.path(base_dir, "Bases historicas")

ruta_ipc   <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios al por mayor/Series/IPC_SIPSA.xls"
ruta_mapeo <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Time-series/mapeo_retail_sipsa_v3.xlsx"
ruta_corr  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/var-ipc/correlativa_ipc.xlsx"

out_dir    <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios al por mayor/Series"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ruta_salida <- file.path(out_dir, "sipsa_deflactado_base2018_12.xlsx")

# ---------------- CONFIG ----------------
CIUDADES_OBJ <- c("Bogota", "Medellin", "Cali")
MES_BASE     <- "2018-12"
meses_abr    <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")

# 🔧 PATRÓN del IPC agregado de alimentos (AJUSTA si tu archivo usa otro texto)
PATRON_ALIMENTOS <- "ALIMENT"   # sirve para ALIMENTOS / ALIMENTACION / etc

# ---------------- HELPERS ----------------
norm_names <- function(df){
  n <- names(df)
  n <- iconv(n, from = "", to = "ASCII//TRANSLIT")
  n <- tolower(n)
  n <- gsub("[^a-z0-9]+", "_", n)
  n <- gsub("^_|_$", "", n)
  names(df) <- n
  df
}

clean_key <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- str_squish(x)
  x
}

to_num_es <- function(x){
  x <- as.character(x)
  x <- str_replace(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

norm_ciudad <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_squish(x)
  x <- case_when(
    x %in% c("Bogota", "Bogota D.C.", "Bogota, D.C.", "BOGOTA, D.C.", "BOGOTA D.C.") ~ "Bogota",
    x %in% c("MEDELLIN", "Medellin") ~ "Medellin",
    x %in% c("CALI", "Cali") ~ "Cali",
    TRUE ~ x
  )
  x
}

# ============================================================
# 1) SIPSA mensual
# ============================================================
files_rds <- list.files(path_bases, pattern = "\\.rds$", full.names = TRUE)
dataset   <- files_rds %>% lapply(readRDS) %>% bind_rows()

sipsa_raw <- dataset %>%
  mutate(
    Fecha        = as.Date(Fecha),
    Precio_kg    = as.numeric(Precio_kg),
    Ciudad_raw   = trimws(sub(",.*", "", Mercado)),
    Ciudad       = norm_ciudad(Ciudad_raw),
    Grupo_sipsa  = stringi::stri_trans_general(Grupo, "Latin-ASCII") %>% toupper() %>% trimws(),
    Alimento_raw = as.character(Alimento),
    Alimento_key = clean_key(Alimento_raw)
  ) %>%
  filter(!is.na(Fecha), !is.na(Precio_kg), Ciudad %in% CIUDADES_OBJ)

sipsa_ts <- sipsa_raw %>%
  mutate(
    Year_Month = yearmonth(Fecha),
    anio_mes   = format(as.Date(Year_Month), "%Y-%m")
  ) %>%
  group_by(anio_mes, Ciudad, Grupo_sipsa, Alimento_raw, Alimento_key) %>%
  summarise(Precio_kg_prom = mean(Precio_kg, na.rm = TRUE), .groups = "drop")

# ============================================================
# 2) Mapeo v3: SIPSA -> retail
# ============================================================
mapeo <- read_excel(ruta_mapeo) %>% norm_names() %>%
  transmute(
    retail_raw = as.character(retail),
    sipsa_raw  = as.character(sipsa),
    retail_key = clean_key(retail_raw),
    sipsa_key  = clean_key(sipsa_raw)
  ) %>%
  filter(!is.na(retail_key), !is.na(sipsa_key)) %>%
  distinct(sipsa_key, retail_key, retail_raw, sipsa_raw)

sipsa_ts2 <- sipsa_ts %>%
  semi_join(mapeo %>% distinct(sipsa_key), by = c("Alimento_key" = "sipsa_key")) %>%
  left_join(mapeo %>% distinct(sipsa_key, retail_key, retail_raw), by = c("Alimento_key" = "sipsa_key"))

# ============================================================
# 3) Correlativa: si logra subclase6 por nombre (rara vez)
# ============================================================
corr <- read_excel(ruta_corr) %>% norm_names()
candidatas_nombre <- intersect(names(corr), c("dane","articulo","producto","descripcion","nombre","bien","item"))
col_nombre <- if (length(candidatas_nombre) == 0) NA_character_ else candidatas_nombre[1]

if (!is.na(col_nombre)) {
  corr2 <- corr %>%
    transmute(
      subclase6 = str_extract(as.character(subclase), "\\d+") %>% str_pad(6, pad="0"),
      nombre_key = clean_key(as.character(.data[[col_nombre]]))
    ) %>%
    filter(!is.na(subclase6), !is.na(nombre_key)) %>%
    distinct(nombre_key, subclase6)
  
  sipsa_ts3 <- sipsa_ts2 %>%
    left_join(corr2, by = c("retail_key" = "nombre_key")) %>%
    rename(subclase6_directa = subclase6)
} else {
  sipsa_ts3 <- sipsa_ts2 %>% mutate(subclase6_directa = NA_character_)
}

# ============================================================
# 4) IPC: subclase6 + IPC "ALIMENTOS" agregado
# ============================================================
ipc_raw <- read_excel(ruta_ipc) %>% norm_names()

ipc_full <- ipc_raw %>%
  mutate(
    mes = tolower(as.character(mes)),
    mes_num = match(mes, meses_abr),
    anio_mes = sprintf("%04d-%02d", as.integer(ano), mes_num),
    Ciudad   = norm_ciudad(ciudad),
    grupo_txt = clean_key(grupo),
    grupo_codigo8 = str_extract(as.character(grupo), "\\d{8}"),
    subclase6 = if_else(!is.na(grupo_codigo8), substr(grupo_codigo8, 1, 6), NA_character_),
    ipc = to_num_es(numero_indice)
  ) %>%
  filter(Ciudad %in% CIUDADES_OBJ, !is.na(mes_num), !is.na(ipc))

# IPC por subclase
ipc_sub <- ipc_full %>%
  filter(!is.na(subclase6)) %>%
  select(anio_mes, Ciudad, subclase6, ipc) %>%
  distinct()

ipc_base_sub <- ipc_sub %>%
  filter(anio_mes == MES_BASE) %>%
  select(Ciudad, subclase6, ipc_base_sub = ipc) %>%
  distinct()

# IPC agregado "alimentos" por ciudad+mes (fallback universal)
ipc_alim <- ipc_full %>%
  filter(str_detect(grupo_txt, PATRON_ALIMENTOS)) %>%
  group_by(anio_mes, Ciudad) %>%
  summarise(ipc_alim = mean(ipc, na.rm = TRUE), .groups = "drop")

ipc_base_alim <- ipc_alim %>%
  filter(anio_mes == MES_BASE) %>%
  select(Ciudad, ipc_base_alim = ipc_alim) %>%
  distinct()

# ============================================================
# 5) Deflactar con if/else:
#   - si subclase6_directa existe => usa ipc_sub
#   - si no => usa ipc_alim (fallback)
# ============================================================
sipsa_defl <- sipsa_ts3 %>%
  left_join(ipc_sub,      by = c("anio_mes","Ciudad","subclase6_directa"="subclase6")) %>%
  left_join(ipc_base_sub, by = c("Ciudad","subclase6_directa"="subclase6")) %>%
  left_join(ipc_alim,     by = c("anio_mes","Ciudad")) %>%
  left_join(ipc_base_alim,by = "Ciudad") %>%
  mutate(
    usa_subclase = !is.na(subclase6_directa) & !is.na(ipc) & !is.na(ipc_base_sub),
    usa_alim     =  is.na(subclase6_directa) & !is.na(ipc_alim) & !is.na(ipc_base_alim),
    
    ipc_usado      = if_else(usa_subclase, ipc, ipc_alim),
    ipc_base_usado = if_else(usa_subclase, ipc_base_sub, ipc_base_alim),
    
    motivo = case_when(
      is.na(retail_key) ~ "1_sin_mapeo_v3",
      usa_subclase ~ "OK_subclase",
      usa_alim ~ "OK_fallback_alimentos",
      TRUE ~ "2_sin_ipc_incluso_alimentos"
    ),
    
    fila_valida = motivo %in% c("OK_subclase","OK_fallback_alimentos"),
    precio_real_base2018_12 = if_else(fila_valida, Precio_kg_prom * (ipc_base_usado / ipc_usado), NA_real_)
  )

# ============================================================
# 6) Export mínimo (3 hojas)
# ============================================================
deflactado <- sipsa_defl %>%
  filter(fila_valida) %>%
  select(Ciudad, anio_mes, Grupo_sipsa, Alimento_raw, retail_raw,
         Precio_kg_prom, precio_real_base2018_12, motivo)

diagnostico <- sipsa_defl %>%
  count(Ciudad, motivo, sort = TRUE)

sin_match <- sipsa_defl %>%
  filter(!fila_valida) %>%
  select(Ciudad, anio_mes, Grupo_sipsa, Alimento_raw, retail_raw, motivo) %>%
  distinct() %>%
  arrange(Ciudad, motivo, Alimento_raw, anio_mes)

write_xlsx(
  list(deflactado = deflactado,
       diagnostico   = diagnostico,
       sin_match     = sin_match),
  ruta_salida
)

message("✅ Guardado: ", ruta_salida)
print(diagnostico)
