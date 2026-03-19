# ============================================================
# AGREGAR GRUPO SIPSA A DANE YA DEFLACTADO USANDO MAPEO v3
# Sin duplicar filas: retail -> grupo (colapsado)
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(writexl)

# ---- rutas ----
ruta_precios_defl <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018_deflactados_base2018_12.xlsx"
ruta_mapeo_v3 <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Time-series/mapeo_retail_sipsa_v3.xlsx"
ruta_salida <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018_deflactados_base2018_12_con_grupo.xlsx"

# ============================================================
# HELPERS
# ============================================================
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

macro_grupo_from_subclase <- function(subclase6_chr){
  sc <- suppressWarnings(as.integer(subclase6_chr))
  ifelse(
    is.na(sc), NA_character_,
    dplyr::case_when(
      sc >= 11100 & sc < 11200 ~ "GRANOS Y CEREALES",
      sc >= 11200 & sc < 11300 ~ "CARNES",
      sc >= 11300 & sc < 11400 ~ "PESCADOS",
      sc >= 11400 & sc < 11500 ~ "LACTEOS Y HUEVOS",
      sc >= 11500 & sc < 11600 ~ "PROCESADOS",
      sc >= 11600 & sc < 11700 ~ "FRUTAS",
      sc >= 11700 & sc < 11800 ~ "VERDURAS Y HORTALIZAS",
      sc >= 11800 & sc < 11900 ~ "PROCESADOS",
      sc >= 11900 & sc < 12000 ~ "PROCESADOS",
      TRUE ~ NA_character_
    )
  )
}

# ============================================================
# A) LEER PRECIOS YA DEFLACTADOS
# ============================================================
sheets_defl <- excel_sheets(ruta_precios_defl)

sheet_use <- if ("precios_deflactados" %in% sheets_defl) {
  "precios_deflactados"
} else {
  sheets_defl[1]
}

precios_defl <- read_excel(ruta_precios_defl, sheet = sheet_use) %>%
  norm_names()

req_cols <- c("articulo", "codigo_articulo", "subclase6", "precio_500g_real_base2018_12")
faltan <- setdiff(req_cols, names(precios_defl))
if(length(faltan) > 0){
  stop("Faltan columnas requeridas en el archivo de precios deflactados: ",
       paste(faltan, collapse = ", "))
}

precios_defl <- precios_defl %>%
  mutate(
    articulo_raw = as.character(articulo),
    retail_key = clean_key(articulo_raw),
    subclase6 = str_pad(as.character(subclase6), 6, pad = "0"),
    grupo_alimento_cod = macro_grupo_from_subclase(subclase6)
  )

# ============================================================
# B) LEER MAPEO v3: retail -> sipsa
# ============================================================
mapeo_v3 <- read_excel(ruta_mapeo_v3) %>%
  norm_names() %>%
  transmute(
    retail_raw = as.character(retail),
    sipsa_raw  = as.character(sipsa),
    retail_key = clean_key(retail_raw),
    sipsa_key  = clean_key(sipsa_raw)
  ) %>%
  filter(!is.na(retail_key), retail_key != "", !is.na(sipsa_key), sipsa_key != "") %>%
  distinct(retail_key, sipsa_key, retail_raw, sipsa_raw)

# ============================================================
# C) DICCIONARIO SIPSA: sipsa -> grupo_sipsa
# ============================================================
base_dir_sipsa <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios al por mayor"
path_bases <- file.path(base_dir_sipsa, "Bases historicas")
files_rds <- list.files(path_bases, pattern = "\\.rds$", full.names = TRUE)

if(length(files_rds) == 0){
  stop("No encontré archivos .rds en: ", path_bases)
}

dataset_sipsa <- files_rds %>%
  lapply(readRDS) %>%
  bind_rows()

dicc_sipsa_grupo <- dataset_sipsa %>%
  transmute(
    sipsa_raw = as.character(Alimento),
    sipsa_key = clean_key(sipsa_raw),
    grupo_sipsa = stringi::stri_trans_general(as.character(Grupo), "Latin-ASCII") %>%
      toupper() %>%
      str_squish()
  ) %>%
  filter(!is.na(sipsa_key), sipsa_key != "", !is.na(grupo_sipsa), grupo_sipsa != "") %>%
  distinct(sipsa_key, grupo_sipsa)

# ============================================================
# D) ARMAR retail -> grupo_sipsa
# ============================================================
retail_to_grupo_sipsa <- mapeo_v3 %>%
  left_join(dicc_sipsa_grupo, by = "sipsa_key") %>%
  filter(!is.na(grupo_sipsa)) %>%
  count(retail_key, retail_raw, grupo_sipsa, sort = TRUE, name = "n_match") %>%
  group_by(retail_key, retail_raw) %>%
  arrange(desc(n_match), grupo_sipsa, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(retail_key, retail_raw, grupo_sipsa, n_match)

# ============================================================
# E) AGREGAR GRUPO ALIMENTO
# ============================================================
precios_con_grupo <- precios_defl %>%
  left_join(retail_to_grupo_sipsa, by = "retail_key") %>%
  mutate(
    grupo_alimento = coalesce(grupo_alimento_cod, grupo_sipsa),
    grupo_alimento = if_else(is.na(grupo_alimento), "SIN_GRUPO", grupo_alimento)
  )

# ============================================================
# F) DIAGNOSTICOS
# ============================================================
diag_grupos <- precios_con_grupo %>%
  count(nombre_ciudad, grupo_alimento, sort = TRUE)

diag_match <- precios_con_grupo %>%
  summarise(
    n_total = n(),
    n_con_grupo_cod = sum(!is.na(grupo_alimento_cod)),
    n_con_grupo_sipsa = sum(!is.na(grupo_sipsa)),
    n_con_grupo_final = sum(!is.na(grupo_alimento) & grupo_alimento != "SIN_GRUPO"),
    n_sin_grupo = sum(grupo_alimento == "SIN_GRUPO")
  )

diag_sin_grupo <- precios_con_grupo %>%
  filter(grupo_alimento == "SIN_GRUPO") %>%
  distinct(codigo_articulo, articulo, retail_key, subclase6)

# ============================================================
# G) EXPORTAR
# ============================================================
write_xlsx(
  list(
    precios_deflactados_con_grupo = precios_con_grupo,
    diagnostico_grupos = diag_grupos,
    diagnostico_match = diag_match,
    sin_grupo = diag_sin_grupo
  ),
  ruta_salida
)

message("✅ Listo. Guardé: ", ruta_salida)