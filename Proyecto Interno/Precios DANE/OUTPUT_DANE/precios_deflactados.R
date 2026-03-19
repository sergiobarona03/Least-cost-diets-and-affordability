# ============================================================
# DEFLACTAR PRECIOS MINORISTAS DANE (1999–2018)
# usando IPC DANE por SUBCLASE + CIUDAD + AÑO-MES
# Ciudades: CALI, MEDELLIN, BOGOTA, D.C.
# Base: 2018-12
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# ---- rutas ----
ruta_precios <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx"
ruta_ipc     <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/IPC_DANE.xls"
ruta_corr    <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/var-ipc/correlativa_ipc.xlsx"

# ---- salida ----
ruta_salida <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018_deflactados_base2018_12.xlsx"

# ---- config ----
mes_base <- "2018-12"

meses_full <- c("enero","febrero","marzo","abril","mayo","junio",
                "julio","agosto","septiembre","octubre","noviembre","diciembre")
meses_abr  <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")

ciudades_std <- c("CALI", "MEDELLIN", "BOGOTA, D.C.")

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

to_num_safe <- function(x){
  if (is.numeric(x)) return(as.numeric(x))
  
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  
  # si tiene coma, asumir formato latino:
  # 1.234,56 -> 1234.56
  tiene_coma <- str_detect(x, ",")
  
  x[tiene_coma] <- str_replace_all(x[tiene_coma], "\\.", "")
  x[tiene_coma] <- str_replace_all(x[tiene_coma], ",", ".")
  
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
    str_detect(x, "^BOGOTA")    ~ "BOGOTA, D.C.",
    str_detect(x, "^MEDELLIN")  ~ "MEDELLIN",
    str_detect(x, "^CALI")      ~ "CALI",
    TRUE ~ x
  )
}

# ============================================================
# 1) CORRELATIVA: gasto_basico -> subclase(6)
# ============================================================
corr <- read_excel(ruta_corr) %>%
  norm_names() %>%
  transmute(
    gasto_basico = as.integer(str_extract(as.character(gasto_basico), "\\d+")),
    subclase6    = str_pad(str_extract(as.character(subclase), "\\d+"), 6, pad = "0")
  ) %>%
  filter(!is.na(gasto_basico), !is.na(subclase6)) %>%
  distinct(gasto_basico, subclase6)

# ============================================================
# 2) IPC
# ============================================================
ipc_raw <- read_excel(ruta_ipc) %>% norm_names()

print("=== ciudades encontradas en IPC ===")
print(sort(unique(ipc_raw$ciudad))[1:min(200, length(unique(ipc_raw$ciudad)))])

ipc <- ipc_raw %>%
  mutate(
    ciudad    = norm_ciudad(ciudad),
    mes       = tolower(as.character(mes)),
    mes_num   = match(mes, meses_abr),
    anio_mes  = sprintf("%04d-%02d", as.integer(ano), mes_num),
    subclase6 = str_pad(substr(str_extract(as.character(subclase), "\\d+"), 1, 6), 6, pad = "0"),
    ipc       = to_num_safe(numero_indice)
  ) %>%
  filter(
    ciudad %in% ciudades_std,
    !is.na(mes_num),
    !is.na(ipc),
    !is.na(subclase6)
  ) %>%
  select(anio_mes, ciudad, subclase6, ipc)

ipc_base <- ipc %>%
  filter(anio_mes == mes_base) %>%
  select(ciudad, subclase6, ipc_base = ipc) %>%
  distinct()

# ============================================================
# 3) PRECIOS
# ============================================================
precios_raw <- read_excel(ruta_precios) %>% norm_names()

print("=== ciudades encontradas en PRECIOS ===")
print(sort(unique(precios_raw$nombre_ciudad))[1:min(200, length(unique(precios_raw$nombre_ciudad)))])

precios <- precios_raw %>%
  mutate(
    nombre_ciudad = norm_ciudad(nombre_ciudad),
    mes           = tolower(as.character(mes)),
    mes_num       = match(mes, meses_full),
    anio_mes      = sprintf("%04d-%02d", as.integer(ano), mes_num),
    
    # variable a deflactar
    precio_500g   = to_num_safe(precio_500g),
    precio_500g   = ifelse(!is.na(precio_500g) & precio_500g == 0, NA_real_, precio_500g),
    
    gasto_basico  = as.integer(substr(str_extract(as.character(codigo_articulo), "\\d+"), 1, 5))
  ) %>%
  filter(
    nombre_ciudad %in% ciudades_std,
    !is.na(mes_num),
    !is.na(gasto_basico)
  )

# ============================================================
# 4) DEFLACTAR
# ============================================================
precios_defl <- precios %>%
  left_join(corr, by = "gasto_basico") %>%
  left_join(
    ipc,
    by = c(
      "anio_mes" = "anio_mes",
      "nombre_ciudad" = "ciudad",
      "subclase6" = "subclase6"
    )
  ) %>%
  left_join(
    ipc_base,
    by = c(
      "nombre_ciudad" = "ciudad",
      "subclase6" = "subclase6"
    )
  ) %>%
  mutate(
    precio_500g_real_base2018_12 = precio_500g * (ipc_base / ipc)
  )

# ============================================================
# 4.1) DIAGNOSTICOS RAPIDOS
# ============================================================
cat("\n================ RESUMEN IPC ================\n")
print(summary(ipc$ipc))

cat("\n================ RESUMEN IPC_BASE ================\n")
print(summary(ipc_base$ipc_base))

cat("\n================ RESUMEN PRECIO_500G ================\n")
print(summary(precios$precio_500g))

cat("\n================ RESUMEN PRECIO REAL ================\n")
print(summary(precios_defl$precio_500g_real_base2018_12))

cat("\n================ TOP 20 PRECIO REAL ================\n")
print(
  precios_defl %>%
    select(anio_mes, nombre_ciudad, articulo, precio_500g, ipc, ipc_base, precio_500g_real_base2018_12) %>%
    arrange(desc(precio_500g_real_base2018_12)) %>%
    head(20)
)

cat("\n================ PRIMERAS 20 FILAS DE CONTROL ================\n")
print(
  precios_defl %>%
    select(anio_mes, nombre_ciudad, articulo, precio_500g, ipc, ipc_base, precio_500g_real_base2018_12) %>%
    head(20)
)

# ============================================================
# 5) ELIMINAR ARTICULOS QUE NUNCA TIENEN MATCH
# ============================================================
precios_defl <- precios_defl %>%
  mutate(
    fila_valida = !is.na(precio_500g) &
      !is.na(subclase6) &
      !is.na(ipc) &
      !is.na(ipc_base)
  )

articulos_con_alguna_valida <- precios_defl %>%
  dplyr::group_by(codigo_articulo) %>%
  dplyr::summarise(tiene_alguna = any(fila_valida), .groups = "drop") %>%
  filter(tiene_alguna) %>%
  pull(codigo_articulo)

precios_final <- precios_defl %>%
  filter(codigo_articulo %in% articulos_con_alguna_valida) %>%
  filter(fila_valida)

articulos_eliminados <- precios_defl %>%
  dplyr::group_by(codigo_articulo) %>%
  dplyr::summarise(
    articulo = first(articulo),
    nombre_ciudad = first(nombre_ciudad),
    n_filas = n(),
    n_validas = sum(fila_valida, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_validas == 0) %>%
  arrange(desc(n_filas))

meses_eliminados <- precios_defl %>%
  filter(codigo_articulo %in% articulos_con_alguna_valida) %>%
  filter(!fila_valida) %>%
  dplyr::mutate(
    motivo = case_when(
      is.na(subclase6)   ~ "sin correlativa",
      is.na(ipc)         ~ "sin ipc mes",
      is.na(ipc_base)    ~ "sin ipc base",
      is.na(precio_500g) ~ "sin precio_500g",
      TRUE               ~ "otro"
    )
  ) %>%
  select(codigo_articulo, articulo, nombre_ciudad, anio_mes, motivo)

# ============================================================
# 6) EXPORTAR
# ============================================================
write_xlsx(
  list(
    precios_deflactados   = precios_final,
    articulos_eliminados  = articulos_eliminados,
    meses_eliminados      = meses_eliminados
  ),
  ruta_salida
)

cat("\nArchivo exportado en:\n", ruta_salida, "\n")
