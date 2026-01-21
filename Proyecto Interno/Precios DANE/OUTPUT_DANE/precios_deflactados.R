# ============================================================
# DEFLECTAR PRECIOS MINORISTAS DANE (1999–2018)
# usando IPC DANE por SUBCLASE + CIUDAD + AÑO-MES
# Ciudades: CALI, MEDELLIN, BOGOTA, D.C.
# Base: 2018-12
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# ---- rutas ----
ruta_precios <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_IPC_1999_2018.xlsx"
ruta_ipc     <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/IPC_DANE.xls"
ruta_corr    <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/var-ipc/correlativa_ipc.xlsx"

# ---- config ----
mes_base <- "2018-12"

meses_full <- c("enero","febrero","marzo","abril","mayo","junio",
                "julio","agosto","septiembre","octubre","noviembre","diciembre")
meses_abr  <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")

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

# IPC y precios: coma decimal -> punto
to_num_es <- function(x){
  x <- as.character(x)
  x <- str_replace(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

# NORMALIZA CIUDAD 
norm_ciudad <- function(x){
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")  
  x <- toupper(trimws(x))
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\.", "", x)                           
  x <- gsub(" ,", ",", x)
  
  dplyr::case_when(
    str_detect(x, "^BOGOTA") ~ "BOGOTA, D.C.",
    str_detect(x, "^MEDELLIN") ~ "MEDELLIN",
    str_detect(x, "^CALI") ~ "CALI",
    TRUE ~ x
  )
}

ciudades_std <- c("CALI", "MEDELLIN", "BOGOTA, D.C.")

# =========================
# 1) correlativa: gasto_basico -> subclase(6)
# =========================
corr <- read_excel(ruta_corr) %>% norm_names() %>%
  transmute(
    gasto_basico = as.integer(str_extract(as.character(gasto_basico), "\\d+")),
    subclase6    = str_pad(str_extract(as.character(subclase), "\\d+"), 6, pad = "0")
  ) %>%
  filter(!is.na(gasto_basico), !is.na(subclase6)) %>%
  distinct(gasto_basico, subclase6)

# =========================
# 2) IPC
# =========================
ipc_raw <- read_excel(ruta_ipc) %>% norm_names()

# (diagnóstico: mira cómo viene Bogotá)
print(sort(unique(ipc_raw$ciudad))[1:min(200, length(unique(ipc_raw$ciudad)))])

ipc <- ipc_raw %>%
  mutate(
    ciudad = norm_ciudad(ciudad),
    mes = tolower(as.character(mes)),
    mes_num = match(mes, meses_abr),
    anio_mes = sprintf("%04d-%02d", as.integer(ano), mes_num),
    subclase6 = substr(str_extract(as.character(subclase), "\\d+"), 1, 6),
    ipc = to_num_es(numero_indice)
  ) %>%
  filter(ciudad %in% ciudades_std, !is.na(mes_num), !is.na(ipc), !is.na(subclase6)) %>%
  select(anio_mes, ciudad, subclase6, ipc)

ipc_base <- ipc %>%
  filter(anio_mes == mes_base) %>%
  select(ciudad, subclase6, ipc_base = ipc) %>%
  distinct()

# =========================
# 3) PRECIOS
# =========================
precios_raw <- read_excel(ruta_precios) %>% norm_names()

# (diagnóstico: mira cómo viene Bogotá)
print(sort(unique(precios_raw$nombre_ciudad))[1:min(200, length(unique(precios_raw$nombre_ciudad)))])

precios <- precios_raw %>%
  mutate(
    nombre_ciudad = norm_ciudad(nombre_ciudad),
    mes = tolower(as.character(mes)),
    mes_num = match(mes, meses_full),
    anio_mes = sprintf("%04d-%02d", as.integer(ano), mes_num),
    precio = to_num_es(precio),
    precio = ifelse(!is.na(precio) & precio == 0, NA_real_, precio),
    gasto_basico = as.integer(substr(str_extract(as.character(codigo_articulo), "\\d+"), 1, 5))
  ) %>%
  filter(nombre_ciudad %in% ciudades_std, !is.na(mes_num), !is.na(gasto_basico))

# =========================
# 4) DEFLACTAR (manteniendo todo, para diagnosticar)
# =========================
precios_defl <- precios %>%
  left_join(corr, by = "gasto_basico") %>%
  left_join(ipc, by = c("anio_mes" = "anio_mes", "nombre_ciudad" = "ciudad", "subclase6" = "subclase6")) %>%
  left_join(ipc_base, by = c("nombre_ciudad" = "ciudad", "subclase6" = "subclase6")) %>%
  mutate(precio_real_base2018_12 = precio * (ipc_base / ipc))

# =========================
# 5) Eliminar articulos que NUNCA tienen match (en ningún mes)
# =========================
precios_defl <- precios_defl %>%
  mutate(fila_valida = !is.na(precio) & !is.na(subclase6) & !is.na(ipc) & !is.na(ipc_base))

articulos_con_alguna_valida <- precios_defl %>%
  group_by(codigo_articulo) %>%
  summarise(tiene_alguna = any(fila_valida), .groups = "drop") %>%
  filter(tiene_alguna) %>%
  pull(codigo_articulo)

precios <- precios_defl %>%
  filter(codigo_articulo %in% articulos_con_alguna_valida) %>%
  filter(fila_valida)

articulos <- precios_defl %>%
  group_by(codigo_articulo) %>%
  summarise(
    articulo = first(articulo),
    nombre_ciudad = first(nombre_ciudad),
    n_filas = n(),
    n_validas = sum(fila_valida, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_validas == 0) %>%
  arrange(desc(n_filas))

meses_perdidos <- precios_defl %>%
  filter(codigo_articulo %in% articulos_con_alguna_valida) %>%
  filter(!fila_valida) %>%
  mutate(
    motivo = case_when(
      is.na(subclase6) ~ "sin correlativa",
      is.na(ipc) ~ "sin ipc mes",
      is.na(ipc_base) ~ "sin ipc base",
      is.na(precio) ~ "sin precio",
      TRUE ~ "otro"
    )
  ) %>%
  select(codigo_articulo, articulo, nombre_ciudad, anio_mes, motivo)

# =========================
# 6) EXPORTAR
# =========================
ruta_salida <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_DANE_deflactados_base2018_12.xlsx"

write_xlsx(
  list(
    precios_deflactados = precios,
    articulos_eliminados = articulos,
    meses_eliminados = meses_perdidos
  ),
  ruta_salida
)

