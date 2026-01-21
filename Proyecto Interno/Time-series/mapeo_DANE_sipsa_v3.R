# ============================================================
# MAPEO SIPSA - DANE
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(writexl)

# --------- RUTAS ----------
ruta_in <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Time-series/ipc_sipsa_ts_V2.xlsx"
ruta_out <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Time-series/mapeo_retail_sipsa_v3.xlsx"

# --------- HELPERS ----------
norm_names <- function(df){
  n <- names(df)
  n <- iconv(n, from = "", to = "ASCII//TRANSLIT")
  n <- tolower(n)
  n <- gsub("[^a-z0-9]+", "_", n)
  n <- gsub("^_|_$", "", n)
  names(df) <- n
  df
}

clean_txt <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_squish(x)
  x
}

# --------- 1) LEER ----------
sh <- excel_sheets(ruta_in)

# intenta escoger una hoja "lógica"; si no, usa la primera
sheet_use <- sh[1]
if (any(str_detect(tolower(sh), "ipc"))) sheet_use <- sh[str_detect(tolower(sh), "ipc")][1]
if (any(str_detect(tolower(sh), "ts")))  sheet_use <- sh[str_detect(tolower(sh), "ts")][1]

df <- read_excel(ruta_in, sheet = sheet_use) %>% norm_names()

# --------- 2) VALIDAR COLUMNAS ----------
need <- c("articulo", "sipsa")
if (!all(need %in% names(df))) {
  stop("No encuentro columnas 'articulo' y 'sipsa'. Columnas disponibles: ",
       paste(names(df), collapse = ", "))
}

# --------- 3) EXTRAER MAPE0 ----------
mapeo <- df %>%
  transmute(
    articulo_raw = articulo,
    sipsa_raw    = sipsa,
    articulo     = clean_txt(articulo_raw),
    sipsa        = clean_txt(sipsa_raw)
  ) %>%
  filter(!is.na(articulo), articulo != "", !is.na(sipsa), sipsa != "") %>%
  distinct(articulo, sipsa) %>%
  arrange(articulo, sipsa)

diag <- mapeo %>%
  count(articulo, name = "n_sipsa") %>%
  arrange(desc(n_sipsa), articulo)

# --------- 4) EXPORTAR ----------
write_xlsx(
  list(
    mapeo = mapeo,
    diagnostico = diag
  ),
  ruta_out
)

message("✅ Listo. Guardé el mapeo en:\n", ruta_out)