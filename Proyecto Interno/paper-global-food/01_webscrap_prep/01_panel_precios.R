########################################################
## SCRIPT 01_webscrap_prep/01_panel_precios.R
##
## Construye, en un solo script, el panel diario de precios por
## alimento y ciudad a partir de la recoleccion en linea de julio a
## septiembre de 2025. Reune lo que antes hacian 00_csv_a_rds.R,
## 01_construccion_panel.R, 02a_lista_alimentos.R, 02b_panel_balanceado.R
## y las funciones de aux-functions (seleccionar_sku_mediano.R y
## umbral_fechas_mes.R). El precio de cada alimento es la mediana de
## sus productos elegibles, y no el de un producto representativo.
##
## Pasos
##   0. (Opcional) Consolida los csv diarios en un .rds por mes.
##   1. Lee y limpia los registros: tipifica columnas, unifica ciudades
##      y descarta filas sin precio o con columnas desplazadas.
##   2. Excluye los alimentos que no son alimentos base y los productos
##      que no representan su alimento.
##   3. Calcula el tamano de cada producto a partir de su nombre.
##   4. Productos elegibles de cada alimento y ciudad: precio en al
##      menos dos fechas de cada mes y precio por gramo relativo a la
##      mediana del alimento en las 13 ciudades.
##   5. Lleva el precio de cada producto a 500 g (solidos) o 1.000 ml
##      (liquidos). Los alimentos vendidos por unidad toman su peso de
##      la tabla de gramos por unidad, con un solo peso por alimento.
##   6. Panel diario: mediana de los precios de los productos elegibles,
##      sin atipicos (mas de 3 veces o menos de un tercio de la mediana
##      del alimento) y con preferencia por empaques de hasta 500 g o
##      1.000 ml.
##   7. Conserva en cada ciudad los alimentos con precio en al menos
##      dos fechas de cada mes.
##
## El precio por 100 g de parte comestible se calcula despues, en
## 02_dataprep: precio_base / (objetivo_base * densidad / 100 * pc).
##
## Reads:  raw_input_dir/<mes>/*.csv            (solo si reconstruir_rds)
##         panel_dir/data/<mes>.rds
##         dataprep_dir/unidades/lista_unidades gramos.xlsx
## Writes: panel_dir/data/<mes>.rds              (solo si reconstruir_rds)
##         output_panel_dir/raw_mensual/panel_v1.rds
##         output_panel_dir/raw_mensual/lista_por_ciudad.rds
##         output_panel_dir/panel_balanceado.rds
##         output_panel_dir/raw_mensual/precios_producto.rds
##         output_lista_dir/skus_todos.rds
##         output_lista_dir/lista_productos.xlsx
##         output_lista_dir/lista_unidades.xlsx
##         output_lista_dir/productos_excluidos.xlsx
##         output_lista_dir/conteo_alimentos.xlsx
##         output_lista_dir/lista_total_alimentos.xlsx
##         output_lista_dir/alimentos_por_ciudad_detalle.xlsx
########################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(openxlsx)
library(stringi)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/paper-global-food"

panel_dir    <- file.path(base_dir, "01_webscrap_prep")
raw_dir      <- file.path(panel_dir, "data")
dataprep_dir <- file.path(base_dir, "01_webscrap_prep")

output_dir       <- file.path(base_dir, "output")
output_panel_dir <- file.path(output_dir, "paneles")
output_lista_dir <- file.path(output_dir, "lista_alimentos")

ruta_gramos_unidad <- file.path(dataprep_dir, "unidades/lista_unidades gramos.xlsx")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_panel_dir, "raw_mensual"), recursive = TRUE, showWarnings = FALSE)
dir.create(output_lista_dir, recursive = TRUE, showWarnings = FALSE)

# Paso 0: poner en TRUE solo para volver a consolidar los csv diarios.
# Los csv estan en una carpeta por mes dentro de raw_input_dir.
reconstruir_rds <- FALSE
raw_input_dir   <- "C:/Users/danie/OneDrive/Documentos/input panel"

# ============================================================
# Parametros
# ============================================================

umbral_gramos     <- 500    # tamano maximo preferido, solidos (g)
umbral_ml         <- 1000   # tamano maximo preferido, liquidos (ml)
min_fechas_mes    <- 2      # fechas con precio exigidas en cada mes
min_skus_atipicos <- 4      # productos minimos del alimento para aplicar la regla de atipicos
factor_atipico    <- 3      # atipico: mas de 3 veces o menos de 1/3 de la mediana del alimento

# Densidad (g/ml) de los alimentos que se venden en ml o litros.
# Aceites: Codex Stan 210-1999 (densidad relativa a 20 C: 0,918-0,923
# en girasol y 0,919-0,925 en soya; la mezcla se toma igual).
densidad_g_ml <- c(
  "Aceite girasol"        = 0.92,
  "Aceite soya"           = 0.92,
  "Aceite vegetal mezcla" = 0.92
)

# Alimentos que no son alimentos base
alimentos_excluir <- c(
  "Color (bolsita)",
  "Mayonesa doy pack", "Mostaza doy pack", "Salsa de tomate doy pack",
  "Jugo instantáneo (sobre)", "Galletas saladas", "Gelatina", "Margarina",
  "Chocolate instantáneo", "Chocolate amargo", "Chocolate dulce",
  "Vinagre", "Bocadillo veleño"
)

# Productos que no representan su alimento: cortes de lujo de la punta
# de anca (Certified Angus Beef, choice) y el platano maduro "SW", que
# se vende como unidad pero a un precio de varios platanos.
productos_no_representativos <- "angus|choice|\\bsw\\b"

# Tamano en el nombre del producto. "kg" va antes de "g" para que
# "3 kg" no se lea como "3 g".
regex_medida <- "\\d+(?:[\\.,]\\d+)?\\s*(kg|kilo|kilos|g|gr|gramo|gramos|ml|mililitro|mililitros|litro|litros)"
regex_unidad <- "kg|kilo|kilos|g|gr|gramo|gramos|ml|mililitro|mililitros|litro|litros"

normalizar_nombre <- function(x) {
  x %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_squish()
}

# ============================================================
# Paso 0. Consolidar los csv diarios en un .rds por mes (opcional)
# ============================================================

if (reconstruir_rds) {
  
  carpetas <- list.dirs(raw_input_dir, recursive = FALSE, full.names = TRUE)
  
  if (length(carpetas) == 0) {
    stop("No se encontraron subcarpetas dentro de raw_input_dir: ", raw_input_dir)
  }
  
  for (ruta_carpeta in carpetas) {
    
    archivos_csv <- list.files(ruta_carpeta, pattern = "\\.csv$", full.names = TRUE)
    if (length(archivos_csv) == 0) next
    
    datos <- map_dfr(archivos_csv, function(archivo) {
      read_csv(archivo,
               locale = locale(encoding = "UTF-8"),
               col_types = cols(.default = col_character()),
               show_col_types = FALSE) %>%
        clean_names() %>%
        mutate(archivo_origen = basename(archivo))
    })
    
    saveRDS(datos, file.path(raw_dir, paste0(basename(ruta_carpeta), ".rds")),
            compress = "xz")
  }
}

# ============================================================
# Paso 1. Leer y limpiar los registros
# ============================================================

archivos_rds <- list.files(raw_dir, pattern = "\\.rds$", full.names = TRUE)

if (length(archivos_rds) == 0) {
  stop("No hay .rds en ", raw_dir, ". Pon reconstruir_rds <- TRUE.")
}

# El nombre del producto se renombra a product_name al leerlo, para
# que ninguna salida del proyecto identifique al minorista.
leer_mes <- function(ruta_rds, orden) {
  readRDS(ruta_rds) %>%
    select(any_of(c("sipsa_name", "city", "sku_code", "exito_name", "price",
                    "unit_price", "measurement_unit", "tcac_code")),
           archivo_origen) %>%
    rename(product_name = exito_name) %>%
    mutate(
      sipsa_name = str_squish(as.character(sipsa_name)),
      city = str_squish(as.character(city)),
      city = case_when(
        str_detect(str_to_lower(city), "^bogot") ~ "Bogotá",
        str_detect(str_to_lower(city), "^cartagena") ~ "Cartagena",
        TRUE ~ city
      ),
      sku_code         = as.character(sku_code),
      product_name     = as.character(product_name),
      price            = suppressWarnings(parse_number(price)),
      unit_price       = as.character(unit_price),
      measurement_unit = as.character(measurement_unit),
      tcac_code        = str_remove(as.character(tcac_code), ",.*$"),
      fecha            = dmy(str_extract(archivo_origen, "\\d{2}-\\d{2}-\\d{4}")),
      dia              = day(fecha),
      mes              = month(fecha),
      orden_mes        = orden
    ) %>%
    # Filas sin precio (producto no disponible) o con columnas desplazadas
    filter(
      !is.na(sipsa_name),
      !is.na(city),
      !is.na(sku_code),
      str_detect(sku_code, "^[0-9]+$"),
      !str_detect(city, "^http"),
      !str_detect(unit_price, "T|\\+00:00"),
      price > 0
    ) %>%
    select(-archivo_origen)
}

registros <- map2_dfr(archivos_rds, seq_along(archivos_rds), leer_mes) %>%
  arrange(orden_mes, fecha, sipsa_name, city, sku_code)

# ============================================================
# Paso 2. Exclusiones
# ============================================================

productos_excluidos <- registros %>%
  filter(str_detect(str_to_lower(product_name), productos_no_representativos)) %>%
  distinct(sipsa_name, product_name)

message("Productos excluidos por no representar su alimento: ", nrow(productos_excluidos))
print(productos_excluidos, n = Inf)

registros <- registros %>%
  filter(
    !normalizar_nombre(sipsa_name) %in% normalizar_nombre(alimentos_excluir),
    !str_detect(str_to_lower(product_name), productos_no_representativos)
  )

# ============================================================
# Paso 3. Tamano de cada producto
# ============================================================

meses_panel <- sort(unique(format(registros$fecha, "%Y-%m")))

# Menor numero de fechas con precio del producto en un mes del panel
fechas_mes_sku <- registros %>%
  mutate(mes_ym = format(fecha, "%Y-%m")) %>%
  distinct(sipsa_name, city, sku_code, fecha, mes_ym) %>%
  count(sipsa_name, city, sku_code, mes_ym) %>%
  complete(nesting(sipsa_name, city, sku_code), mes_ym = meses_panel, fill = list(n = 0L)) %>%
  group_by(sipsa_name, city, sku_code) %>%
  summarise(fechas_mes_min = min(n), .groups = "drop")

skus <- registros %>%
  mutate(
    medida            = str_extract(str_to_lower(product_name), regex_medida),
    cantidad_extraida = medida %>%
      str_extract("\\d+(?:[\\.,]\\d+)?") %>%
      str_replace(",", ".") %>%
      as.numeric(),
    unidad_extraida   = str_extract(medida, regex_unidad),
    es_liquido        = unidad_extraida %in% c("ml", "mililitro", "mililitros", "litro", "litros"),
    cantidad_base     = if_else(unidad_extraida %in% c("litro", "litros", "kg", "kilo", "kilos"),
                                cantidad_extraida * 1000, cantidad_extraida),
    densidad          = if_else(es_liquido, unname(densidad_g_ml[sipsa_name]), 1),
    objetivo_base     = if_else(es_liquido, 1000, 500),
    gramos_sku        = cantidad_base * densidad
  )

sin_densidad <- skus %>%
  filter(es_liquido, is.na(densidad)) %>%
  distinct(sipsa_name)

if (nrow(sin_densidad) > 0) {
  stop("Falta la densidad de: ", paste(sin_densidad$sipsa_name, collapse = ", "))
}

skus <- skus %>%
  group_by(sipsa_name, city, sku_code, product_name) %>%
  summarise(
    n_fechas       = n_distinct(fecha),
    gramos_sku     = first(gramos_sku),
    objetivo_base  = first(objetivo_base),
    densidad       = first(densidad),
    precio_mediano = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(fechas_mes_sku, by = c("sipsa_name", "city", "sku_code")) %>%
  mutate(precio_gramo_sku = precio_mediano / gramos_sku)

# ============================================================
# Paso 4. Productos elegibles de cada alimento y ciudad
# ============================================================
# En los alimentos-ciudad con al menos un producto con tamano en el
# nombre compiten solo esos productos, con dos condiciones:
#   4a. Cobertura: precio en al menos min_fechas_mes fechas de cada
#       mes; si ninguno cumple, compiten todos.
#   4b. Precio por gramo frente a la mediana del alimento en las 13
#       ciudades (veces_mediana). La regla de atipicos se aplica en
#       aplicar_reglas() (paso 6), para que la prueba de sensibilidad
#       pueda cambiar el factor sin rehacer este paso.
# En los alimentos-ciudad sin ningun producto con tamano (a granel o
# por unidad) compiten todos sus productos.

skus_flag <- skus %>%
  group_by(sipsa_name, city) %>%
  mutate(tiene_medida = any(!is.na(gramos_sku))) %>%
  ungroup()

limite_g  <- if (is.null(umbral_gramos)) Inf else umbral_gramos
limite_ml <- if (is.null(umbral_ml)) Inf else umbral_ml

sin_medida <- skus_flag %>%
  filter(!tiene_medida) %>%
  mutate(n_skus_alimento = NA_integer_, veces_mediana = NA_real_, cumple_tamano = NA)

con_medida <- skus_flag %>%
  filter(tiene_medida, !is.na(gramos_sku), !is.na(precio_gramo_sku)) %>%
  group_by(sipsa_name, city) %>%
  filter(fechas_mes_min >= min_fechas_mes | !any(fechas_mes_min >= min_fechas_mes)) %>%
  group_by(sipsa_name) %>%
  mutate(
    n_skus_alimento = n(),
    veces_mediana   = precio_gramo_sku / median(precio_gramo_sku)
  ) %>%
  ungroup() %>%
  mutate(cumple_tamano = gramos_sku / densidad <=
           if_else(objetivo_base == 1000, limite_ml, limite_g))

elegibles <- bind_rows(sin_medida, con_medida) %>%
  select(-tiene_medida) %>%
  distinct(sipsa_name, city, sku_code, .keep_all = TRUE)

# ============================================================
# Paso 5. Precio a 500 g (solidos) o 1.000 ml (liquidos)
# ============================================================

# El peso por unidad es propio del alimento y no cambia entre ciudades
# en la tabla, asi que se toma un solo valor por alimento.
gramos_unidad <- read.xlsx(ruta_gramos_unidad) %>%
  select(sipsa_name, cantidad_gramos = `cantidad.aproximada.gramos`) %>%
  mutate(sipsa_name = str_squish(sipsa_name)) %>%
  group_by(sipsa_name) %>%
  summarise(cantidad_gramos = median(cantidad_gramos, na.rm = TRUE), .groups = "drop")

precio_a_base <- function(df) {
  df %>%
    mutate(
      objetivo_base = coalesce(objetivo_base, 500),
      densidad      = coalesce(densidad, 1),
      unidad        = str_to_lower(measurement_unit),
      precio_base = case_when(
        # Tamano en el nombre
        !is.na(gramos_sku) ~ (price / (gramos_sku / densidad)) * objetivo_base,
        # A granel: el precio es por kilo
        unidad %in% c("kilogramo", "gramo") ~ (price / 1000) * 500,
        # Huevos: el precio es del paquete de varias unidades
        unidad == "unidad" & !is.na(cantidad_gramos) & str_detect(str_to_lower(sipsa_name), "huevo") ~
          (price / (cantidad_gramos * unidades_paquete)) * 500,
        # Por unidad
        unidad == "unidad" & !is.na(cantidad_gramos) ~ (price / cantidad_gramos) * 500,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-unidad)
}

precios_producto <- registros %>%
  inner_join(
    elegibles %>%
      select(sipsa_name, city, sku_code, gramos_sku, objetivo_base, densidad,
             n_skus_alimento, veces_mediana, cumple_tamano),
    by = c("sipsa_name", "city", "sku_code")
  ) %>%
  left_join(gramos_unidad, by = "sipsa_name") %>%
  mutate(
    unidades_paquete = coalesce(
      as.numeric(str_extract(product_name, "(?<=\\()\\d+(?=\\s*und\\))")),
      1
    )
  ) %>%
  precio_a_base()

# Alimentos vendidos por unidad que no tienen peso en la tabla
lista_unidades <- precios_producto %>%
  filter(is.na(gramos_sku), str_to_lower(measurement_unit) == "unidad", is.na(cantidad_gramos)) %>%
  distinct(sipsa_name, city, measurement_unit) %>%
  arrange(sipsa_name, city)

if (nrow(lista_unidades) > 0) {
  message("Alimentos vendidos por unidad sin peso en la tabla: ",
          paste(unique(lista_unidades$sipsa_name), collapse = ", "))
}

precios_producto <- precios_producto %>%
  filter(is.finite(precio_base)) %>%
  select(sipsa_name, city, sku_code, fecha, dia, mes, price, unit_price,
         measurement_unit, tcac_code, precio_base, objetivo_base, densidad,
         n_skus_alimento, veces_mediana, cumple_tamano)

# ============================================================
# Paso 6. Panel diario: mediana entre productos elegibles
# ============================================================
# aplicar_reglas() deja los productos que entran al precio del
# alimento:
#   - Atipicos: se excluyen los productos cuyo precio por gramo es mas
#     de factor_atipico veces, o menos de 1/factor_atipico, la mediana
#     del alimento en las 13 ciudades. Solo en los alimentos con al
#     menos min_skus_atipicos productos; si todos los de una ciudad son
#     atipicos, se conservan.
#   - Tamano: se prefieren los empaques de hasta 500 g o 1.000 ml; si
#     ninguno cumple, entran todos.
# El precio diario del alimento es la mediana de sus productos.

aplicar_reglas <- function(df, factor = factor_atipico, preferir_tamano = TRUE) {
  df %>%
    mutate(atipico = !is.na(veces_mediana) &
             n_skus_alimento >= min_skus_atipicos &
             (veces_mediana > factor | veces_mediana < 1 / factor)) %>%
    group_by(sipsa_name, city) %>%
    filter(!atipico | all(atipico)) %>%
    filter(!preferir_tamano | is.na(cumple_tamano) | cumple_tamano |
             !any(cumple_tamano, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-atipico)
}

productos_panel <- aplicar_reglas(precios_producto)

panel_v1 <- productos_panel %>%
  group_by(sipsa_name, city, fecha) %>%
  summarise(
    precio_base      = round(median(precio_base), 0),
    n_productos      = n_distinct(sku_code),
    objetivo_base    = first(objetivo_base),
    densidad         = first(densidad),
    price            = median(price),
    unit_price       = first(unit_price),
    measurement_unit = first(measurement_unit),
    tcac_code        = first(tcac_code),
    dia              = first(dia),
    mes              = first(mes),
    .groups = "drop"
  ) %>%
  arrange(fecha, sipsa_name, city)

# Lista de productos elegibles, con su condicion (uso interno)
lista_productos <- elegibles %>%
  mutate(atipico = !is.na(veces_mediana) & n_skus_alimento >= min_skus_atipicos &
           (veces_mediana > factor_atipico | veces_mediana < 1 / factor_atipico)) %>%
  select(sipsa_name, city, sku_code, product_name, gramos_sku, n_fechas,
         fechas_mes_min, precio_gramo_sku, veces_mediana, atipico, cumple_tamano) %>%
  arrange(sipsa_name, city, precio_gramo_sku)

# ============================================================
# Paso 7. Alimentos con precio en al menos dos fechas de cada mes
# ============================================================

meses_ciudad <- panel_v1 %>%
  mutate(mes_ym = format(fecha, "%Y-%m")) %>%
  distinct(city, mes_ym)

lista_por_ciudad <- panel_v1 %>%
  mutate(mes_ym = format(fecha, "%Y-%m")) %>%
  distinct(city, sipsa_name, fecha, mes_ym) %>%
  count(city, sipsa_name, mes_ym) %>%
  full_join(
    distinct(panel_v1, city, sipsa_name) %>%
      inner_join(meses_ciudad, by = "city", relationship = "many-to-many"),
    by = c("city", "sipsa_name", "mes_ym")
  ) %>%
  mutate(n = coalesce(n, 0L)) %>%
  group_by(city, sipsa_name) %>%
  summarise(n_fechas = sum(n), fechas_mes_min = min(n), .groups = "drop") %>%
  filter(fechas_mes_min >= min_fechas_mes)

panel_balanceado <- panel_v1 %>%
  semi_join(lista_por_ciudad, by = c("city", "sipsa_name"))

alimentos_por_ciudad <- lista_por_ciudad %>%
  count(city, name = "n_alimentos") %>%
  arrange(desc(n_alimentos))

print(alimentos_por_ciudad)

# ============================================================
# Guardar outputs
# ============================================================

saveRDS(panel_v1,         file.path(output_panel_dir, "raw_mensual/panel_v1.rds"))
saveRDS(lista_por_ciudad, file.path(output_panel_dir, "raw_mensual/lista_por_ciudad.rds"))
saveRDS(panel_balanceado, file.path(output_panel_dir, "panel_balanceado.rds"))
saveRDS(skus,             file.path(output_lista_dir, "skus_todos.rds"))
saveRDS(precios_producto, file.path(output_panel_dir, "raw_mensual/precios_producto.rds"))

write.xlsx(lista_productos,     file.path(output_lista_dir, "lista_productos.xlsx"), overwrite = TRUE)
write.xlsx(lista_unidades,      file.path(output_lista_dir, "lista_unidades.xlsx"), overwrite = TRUE)
write.xlsx(productos_excluidos, file.path(output_lista_dir, "productos_excluidos.xlsx"), overwrite = TRUE)
write.xlsx(alimentos_por_ciudad, file.path(output_lista_dir, "conteo_alimentos.xlsx"), overwrite = TRUE)
write.xlsx(distinct(lista_por_ciudad, sipsa_name) %>% arrange(sipsa_name),
           file.path(output_lista_dir, "lista_total_alimentos.xlsx"), overwrite = TRUE)

wb <- createWorkbook()
for (cd in sort(unique(lista_por_ciudad$city))) {
  addWorksheet(wb, sheetName = cd)
  writeData(wb, sheet = cd,
            lista_por_ciudad %>%
              filter(city == cd) %>%
              select(sipsa_name, n_fechas, fechas_mes_min) %>%
              arrange(sipsa_name))
}
saveWorkbook(wb, file.path(output_lista_dir, "alimentos_por_ciudad_detalle.xlsx"), overwrite = TRUE)

message("Done.")