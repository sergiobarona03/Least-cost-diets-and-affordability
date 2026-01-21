# ============================================================
# AGREGAR GRUPO SIPSA A DANE DEFLACTADO USANDO MAPEO v3 (retail <-> sipsa)
# Sin duplicar filas: retail -> grupo (colapsado)
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(writexl)

# ---- rutas ----
ruta_precios <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_IPC_1999_2018.xlsx"
ruta_ipc     <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/IPC_DANE.xls"
ruta_corr    <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/var-ipc/correlativa_ipc.xlsx"

# tu mapeo v3 (retail <-> sipsa). retail son los nombres DANE
ruta_mapeo_v3 <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Time-series/mapeo_retail_sipsa_v3.xlsx"

# salida
ruta_salida <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_DANE_deflactados_base2018_12_con_grupo.xlsx"

# ---- config ----
mes_base <- "2018-12"

meses_full <- c("enero","febrero","marzo","abril","mayo","junio",
                "julio","agosto","septiembre","octubre","noviembre","diciembre")
meses_abr  <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")

ciudades_std <- c("CALI", "MEDELLIN", "BOGOTA, D.C.")

# ---- helpers ----
norm_names <- function(df){
  n <- names(df)
  n <- iconv(n, from = "", to = "ASCII//TRANSLIT")
  n <- tolower(n)
  n <- gsub("[^a-z0-9]+", "_", n)
  n <- gsub("^_|_$", "", n)
  names(df) <- n
  df
}

to_num_es <- function(x){
  x <- as.character(x)
  x <- str_replace(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

norm_ciudad <- function(x){
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\.", "", x)
  x <- gsub(" ,", ",", x)
  
  dplyr::case_when(
    str_detect(x, "^BOGOTA")   ~ "BOGOTA, D.C.",
    str_detect(x, "^MEDELLIN") ~ "MEDELLIN",
    str_detect(x, "^CALI")     ~ "CALI",
    TRUE ~ x
  )
}

clean_key <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- str_squish(x)
  x
}

# ============================================================
# A) TABLA "subclase6 -> grupo_alimento" (tipo SIPSA)
# ============================================================
# 1) Si tu IPC tiene jerarquia: grupo/subgrupo/clase/subclase, ideal.
# Vamos a intentar detectarlo. Si no existe, hacemos fallback por rangos.
ipc_raw0 <- read_excel(ruta_ipc) %>% norm_names()

# Detecta si hay alguna columna textual util para grupo macro
cand_grupo_txt <- intersect(names(ipc_raw0), c("division","grupo_texto","grupo_nombre","nombre_grupo","agrupacion","grupo"))
# Nota: en tu IPC mayorista, "grupo" era codigo 8. Aqui puede variar.
# Si no hay textual, construimos grupo macro por subclase6 (fallback).

# Construye ipc (anio_mes, ciudad, subclase6, ipc)
ipc <- ipc_raw0 %>%
  mutate(
    ciudad = norm_ciudad(ciudad),
    mes = tolower(as.character(mes)),
    mes_num = match(mes, meses_abr),
    anio_mes = sprintf("%04d-%02d", as.integer(ano), mes_num),
    subclase6 = substr(str_extract(as.character(subclase), "\\d+"), 1, 6),
    ipc = to_num_es(numero_indice)
  ) %>%
  filter(ciudad %in% ciudades_std, !is.na(mes_num), !is.na(ipc), !is.na(subclase6)) %>%
  select(anio_mes, ciudad, subclase6, ipc) %>%
  distinct()

ipc_base <- ipc %>%
  filter(anio_mes == mes_base) %>%
  select(ciudad, subclase6, ipc_base = ipc) %>%
  distinct()

# --- Macrogrupo tipo SIPSA por SUBCLASE6 ---
# Ajusta estos rangos si tu codificacion es distinta; es el esquema COICOP 01.* (alimentos)
macro_grupo_from_subclase <- function(subclase6_chr){
  # subclase6 suele ser "011101", etc.
  sc <- suppressWarnings(as.integer(subclase6_chr))
  # si viene NA, devuelve NA
  ifelse(is.na(sc), NA_character_,
         dplyr::case_when(
           # 0111 Pan y cereales
           sc >= 11100 & sc < 11200 ~ "GRANOS Y CEREALES",
           # 0112 Carnes
           sc >= 11200 & sc < 11300 ~ "CARNES",
           # 0113 Pescados
           sc >= 11300 & sc < 11400 ~ "PESCADOS",
           # 0114 Leche, queso y huevos
           sc >= 11400 & sc < 11500 ~ "LACTEOS Y HUEVOS",
           # 0115 Aceites y grasas
           sc >= 11500 & sc < 11600 ~ "PROCESADOS",
           # 0116 Frutas
           sc >= 11600 & sc < 11700 ~ "FRUTAS",
           # 0117 Verduras y hortalizas
           sc >= 11700 & sc < 11800 ~ "VERDURAS Y HORTALIZAS",
           # 0118 Azucar, confites, etc (procesados)
           sc >= 11800 & sc < 11900 ~ "PROCESADOS",
           # 0119 Otros alimentos (procesados)
           sc >= 11900 & sc < 12000 ~ "PROCESADOS",
           TRUE ~ NA_character_
         )
  )
}

# ============================================================
# B) CORRELATIVA gasto_basico -> subclase6
# ============================================================
corr <- read_excel(ruta_corr) %>% norm_names() %>%
  transmute(
    gasto_basico = as.integer(str_extract(as.character(gasto_basico), "\\d+")),
    subclase6    = str_pad(str_extract(as.character(subclase), "\\d+"), 6, pad = "0")
  ) %>%
  filter(!is.na(gasto_basico), !is.na(subclase6)) %>%
  distinct(gasto_basico, subclase6)

# ============================================================
# C) MAPE0 v3 DANE(reail/articulo) -> SIPSA(sipsa) + grupo SIPSA (desde SIPSA base historica)
# ============================================================
# 1) lee mapeo v3 (retail, sipsa)
mapeo_v3 <- read_excel(ruta_mapeo_v3) %>% norm_names() %>%
  transmute(
    retail_raw = as.character(retail),
    sipsa_raw  = as.character(sipsa),
    retail_key = clean_key(retail_raw),
    sipsa_key  = clean_key(sipsa_raw)
  ) %>%
  filter(!is.na(retail_key), retail_key != "", !is.na(sipsa_key), sipsa_key != "") %>%
  distinct(retail_key, sipsa_key, retail_raw, sipsa_raw)

# 2) construye diccionario SIPSA: sipsa_key -> grupo_sipsa
#    (toma los RDS historicos como ya haces en mayoristas)
base_dir_sipsa <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios al por mayor"
path_bases <- file.path(base_dir_sipsa, "Bases historicas")
files_rds  <- list.files(path_bases, pattern = "\\.rds$", full.names = TRUE)
dataset_sipsa <- files_rds %>% lapply(readRDS) %>% bind_rows()

dicc_sipsa_grupo <- dataset_sipsa %>%
  transmute(
    sipsa_raw = as.character(Alimento),
    sipsa_key = clean_key(sipsa_raw),
    grupo_sipsa = stringi::stri_trans_general(as.character(Grupo), "Latin-ASCII") %>% toupper() %>% str_squish()
  ) %>%
  filter(!is.na(sipsa_key), sipsa_key != "", !is.na(grupo_sipsa), grupo_sipsa != "") %>%
  distinct(sipsa_key, grupo_sipsa)

# 3) arma retail_key -> grupo_sipsa (via mapeo v3 + dicc_sipsa_grupo)
retail_to_grupo_sipsa <- mapeo_v3 %>%
  left_join(dicc_sipsa_grupo, by = "sipsa_key") %>%
  filter(!is.na(grupo_sipsa)) %>%
  # si un retail mapea a varios grupos (raro), tomamos el mas frecuente
  count(retail_key, retail_raw, grupo_sipsa, sort = TRUE) %>%
  group_by(retail_key, retail_raw) %>%
  slice(1) %>%
  ungroup() %>%
  select(retail_key, retail_raw, grupo_sipsa)

# ============================================================
# D) PRECIOS + DEFLACTACION + grupo_alimento (B)
# ============================================================
precios_raw <- read_excel(ruta_precios) %>% norm_names()

precios <- precios_raw %>%
  mutate(
    nombre_ciudad = norm_ciudad(nombre_ciudad),
    mes = tolower(as.character(mes)),
    mes_num = match(mes, meses_full),
    anio_mes = sprintf("%04d-%02d", as.integer(ano), mes_num),
    precio = to_num_es(precio),
    precio = ifelse(!is.na(precio) & precio == 0, NA_real_, precio),
    gasto_basico = as.integer(substr(str_extract(as.character(codigo_articulo), "\\d+"), 1, 5)),
    articulo_raw = as.character(articulo),
    retail_key = clean_key(articulo_raw)
  ) %>%
  filter(nombre_ciudad %in% ciudades_std, !is.na(mes_num), !is.na(gasto_basico))

# deflacta y agrega grupo
precios_defl <- precios %>%
  left_join(corr, by = "gasto_basico") %>%
  left_join(ipc, by = c("anio_mes" = "anio_mes", "nombre_ciudad" = "ciudad", "subclase6" = "subclase6")) %>%
  left_join(ipc_base, by = c("nombre_ciudad" = "ciudad", "subclase6" = "subclase6")) %>%
  mutate(
    precio_real_base2018_12 = precio * (ipc_base / ipc),
    grupo_alimento_cod = macro_grupo_from_subclase(subclase6)
  ) %>%
  # fallback por mapeo v3: retail -> grupo_sipsa (de tus RDS SIPSA)
  left_join(retail_to_grupo_sipsa, by = c("retail_key" = "retail_key")) %>%
  mutate(
    # prioridad: codigo (subclase6) -> si NA, usa grupo_sipsa
    grupo_alimento = coalesce(grupo_alimento_cod, grupo_sipsa),
    grupo_alimento = if_else(is.na(grupo_alimento), "SIN_GRUPO", grupo_alimento),
    fila_valida = !is.na(precio) & !is.na(subclase6) & !is.na(ipc) & !is.na(ipc_base)
  )

# ============================================================
# E) FILTRAR (como lo hacias): deja solo articulos con al menos 1 fila valida
# ============================================================
articulos_con_alguna_valida <- precios_defl %>%
  group_by(codigo_articulo) %>%
  summarise(tiene_alguna = any(fila_valida), .groups = "drop") %>%
  filter(tiene_alguna) %>%
  pull(codigo_articulo)

precios_ok <- precios_defl %>%
  filter(codigo_articulo %in% articulos_con_alguna_valida) %>%
  filter(fila_valida)

# ============================================================
# F) EXPORTAR (solo lo importante: 1 hoja)
# ============================================================
# Si quieres, puedes guardar aparte un diagnostico minimo
diag_grupos <- precios_ok %>%
  count(nombre_ciudad, grupo_alimento, sort = TRUE)

write_xlsx(
  list(
    precios_deflactados = precios_ok,
    diagnostico_grupos  = diag_grupos
  ),
  ruta_salida
)

message("✅ Listo. Guardé: ", ruta_salida)
