########################################################
## SCRIPT 01_webscrap_prep/01_construccion_panel.R
##
## Construye el panel de precios a partir de los rds
## mensuales generados por 00_csv_a_rds.R: limpia, tipifica,
## elige el sku representativo por alimento/ciudad y
## estandariza el precio a 500 g (solidos) o a 1.000 ml (liquidos).
##
## precio_base es el precio de objetivo_base gramos o mililitros de
## producto. Los liquidos llevan su densidad (densidad_g_ml) para
## poder pasar despues el precio a gramos: el precio por 100 g es
## precio_base / (objetivo_base * densidad / 100).
##
## Sku representativo: entre los skus con al menos dos fechas por
## mes y de hasta umbral_gramos (500 g) o umbral_ml (1.000 ml), sin los
## precios por gramo atipicos
## (regla de Tukey), el de precio por gramo mediano.
## La funcion: aux-functions/seleccionar_sku_mediano.R
##
## Reads:  panel_dir/data/*.rds
##         dataprep_dir/unidades/lista_unidades gramos.xlsx
## Writes: output_panel_dir/panel_v1.rds  (diario, con
##         precio_base, objetivo_base y densidad)
##         output_lista_dir/lista_alimentos.xlsx
##         output_lista_dir/lista_unidades.xlsx
########################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(openxlsx)

# ============================================================
# Rutas
# ============================================================

base_dir  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
panel_dir <- file.path(base_dir, "01_webscrap_prep")
aux_dir   <- file.path(panel_dir, "aux-functions")

raw_mensual_dir <- file.path(panel_dir, "data")
dataprep_dir    <- file.path(base_dir, "02_dataprep")

output_dir       <- file.path(base_dir, "output")
output_panel_dir <- file.path(output_dir, "paneles/raw_mensual")
output_lista_dir <- file.path(output_dir, "lista_alimentos")

ruta_gramos_unidad <- file.path(dataprep_dir, "unidades/lista_unidades gramos.xlsx")

dir.create(output_panel_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_lista_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(aux_dir, "seleccionar_sku_mediano.R"))

# ============================================================
# Parametros de la seleccion del sku
# Umbral de tamano: 500 g para los solidos y 1.000 ml para los liquidos.
# Un umbral en NULL equivale a "ningun umbral" para ese tipo.
# ============================================================

umbral_gramos  <- 500
umbral_ml      <- 1000
min_fechas_mes <- 2

# Densidad (g/ml) de los alimentos que se venden en ml o litros.
# Aceites: Codex Stan 210-1999 (densidad relativa a 20 C: 0,918-0,923
# en girasol y 0,919-0,925 en soya; la mezcla se toma igual).
# Vinagre: 5% de acido acetico.
densidad_g_ml <- c(
  "Aceite girasol"        = 0.92,
  "Aceite soya"           = 0.92,
  "Aceite vegetal mezcla" = 0.92,
  "Vinagre"               = 1.01
)

# ============================================================
# Variables
# ============================================================

variables_panel <- c(
  "sipsa_name",
  "city",
  "sku_code",
  "exito_name",
  "price",
  "unit_price",
  "measurement_unit",
  "tcac_code"
)

# ============================================================
# Fecha desde el nombre del archivo original (columna archivo_origen)
# ============================================================

extraer_fecha_archivo <- function(archivo) {
  archivo %>%
    str_extract("\\d{2}-\\d{2}-\\d{4}") %>%
    dmy()
}

# ============================================================
# Construcción de panel a partir de un rds mensual (output del 00)
## (identico a v1)
# ============================================================

construir_panel <- function(ruta_rds, orden) {

  readRDS(ruta_rds) %>%
    select(any_of(variables_panel), archivo_origen) %>%

    mutate(
      sipsa_name = as.character(sipsa_name),

      city = str_squish(as.character(city)),
      city = case_when(
        str_to_lower(city) %in% c(
          "bogotá", "bogotá, d.c.", "bogota", "bogota, d.c."
        ) ~ "Bogotá",
        str_detect(str_to_lower(city), "^cartagena") ~ "Cartagena",
        TRUE ~ city
      ),

      sku_code = as.character(sku_code),
      exito_name = as.character(exito_name),
      price = parse_number(price),
      unit_price = as.character(unit_price),
      measurement_unit = as.character(measurement_unit),
      tcac_code = str_remove(as.character(tcac_code), ",.*$"),

      fecha = extraer_fecha_archivo(archivo_origen),
      dia = day(fecha),
      mes = month(fecha),
      orden_mes = orden
    ) %>%

    filter(
      !is.na(sipsa_name),
      !is.na(city),
      !is.na(sku_code),
      str_detect(sku_code, "^[0-9]+$"),
      !str_detect(city, "^http"),
      !str_detect(unit_price, "T|\\+00:00"),
      price > 0
    ) %>%

    select(-archivo_origen) %>%
    arrange(fecha, sipsa_name, city, sku_code)
}

# ============================================================
# Procesar todos los rds mensuales disponibles
# ============================================================

archivos_rds <- list.files(raw_mensual_dir, pattern = "\\.rds$", full.names = TRUE)

if (length(archivos_rds) == 0) {
  stop("No se encontraron rds en raw_mensual_dir. Corre primero 00_csv_a_rds.R")
}

nombres_carpeta <- tools::file_path_sans_ext(basename(archivos_rds))

paneles <- map2(archivos_rds, seq_along(archivos_rds), construir_panel)
names(paneles) <- nombres_carpeta

# ============================================================
# Panel temporal combinado
# ============================================================

panel_final_temp <- bind_rows(paneles) %>%
  arrange(orden_mes, fecha, sipsa_name, city, sku_code)

# ============================================================
# Info por sku: tamano en gramos (extraido del nombre exito, con
# densidad para los liquidos), precio mediano a lo largo de las
# fechas y menor numero de fechas con precio en un mes.
# ============================================================

meses_panel <- sort(unique(format(panel_final_temp$fecha, "%Y-%m")))

fechas_mes_sku <- panel_final_temp %>%
  mutate(mes_ym = format(fecha, "%Y-%m")) %>%
  distinct(sipsa_name, city, sku_code, fecha, mes_ym) %>%
  count(sipsa_name, city, sku_code, mes_ym) %>%
  complete(nesting(sipsa_name, city, sku_code), mes_ym = meses_panel, fill = list(n = 0L)) %>%
  group_by(sipsa_name, city, sku_code) %>%
  summarise(fechas_mes_min = min(n), .groups = "drop")

lista_representativa <- panel_final_temp %>%
  mutate(
    texto_exito = str_to_lower(exito_name),

    medida = str_extract(
      texto_exito,
      "\\d+(?:[\\.,]\\d+)?\\s*(g|gr|gramo|gramos|ml|mililitro|mililitros|litro|litros)"
    ),

    cantidad_extraida = medida %>%
      str_extract("\\d+(?:[\\.,]\\d+)?") %>%
      str_replace(",", ".") %>%
      as.numeric(),

    unidad_extraida = medida %>%
      str_extract("g|gr|gramo|gramos|ml|mililitro|mililitros|litro|litros"),

    es_liquido = unidad_extraida %in% c("ml", "mililitro", "mililitros", "litro", "litros"),

    cantidad_base = if_else(unidad_extraida %in% c("litro", "litros"),
                            cantidad_extraida * 1000, cantidad_extraida),

    densidad = if_else(es_liquido, unname(densidad_g_ml[sipsa_name]), 1),

    objetivo_base = if_else(es_liquido, 1000, 500),

    gramos_sku = cantidad_base * densidad
  )

sin_densidad <- lista_representativa %>%
  filter(es_liquido, is.na(densidad)) %>%
  distinct(sipsa_name)

if (nrow(sin_densidad) > 0) {
  stop("Falta la densidad de: ", paste(sin_densidad$sipsa_name, collapse = ", "))
}

lista_representativa <- lista_representativa %>%
  group_by(sipsa_name, city, sku_code, exito_name) %>%
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
# Elegir el sku representativo por alimento-ciudad
# ============================================================

lista_final <- seleccionar_sku_mediano(lista_representativa,
                                       umbral_gramos = umbral_gramos,
                                       umbral_ml = umbral_ml,
                                       min_fechas_mes = min_fechas_mes) %>%
  select(-precio_mediano)

# ============================================================
# Gramos, base y densidad del sku elegido por alimento-ciudad, y
# gramos por unidad para productos "por unidad"
# ============================================================

referencias <- lista_final %>%
  select(sipsa_name, city, gramos_sku, objetivo_base, densidad) %>%
  distinct(sipsa_name, city, .keep_all = TRUE)

gramos_unidad <- read.xlsx(ruta_gramos_unidad) %>%
  dplyr::select(sipsa_name, city, cantidad_gramos = `cantidad.aproximada.gramos`) %>%
  distinct(sipsa_name, city, cantidad_gramos)

# ============================================================
# Panel con sku representativo + gramos del sku pegados
# ============================================================

panel_con_cantidad <- panel_final_temp %>%
  inner_join(
    lista_final %>% select(sipsa_name, city, sku_code),
    by = c("sipsa_name", "city", "sku_code")
  ) %>%
  left_join(referencias, by = c("sipsa_name", "city")) %>%
  left_join(gramos_unidad, by = c("sipsa_name", "city")) %>%
  mutate(
    unidades_paquete = coalesce(
      as.numeric(str_extract(exito_name, "(?<=\\()\\d+(?=\\s*und\\))")),
      1
    )
  )

# ============================================================
# Alimentos vendidos por unidad sin cobertura en gramos_unidad
# ============================================================

lista_unidades <- panel_con_cantidad %>%
  filter(is.na(gramos_sku) & str_to_lower(measurement_unit) == "unidad" & is.na(cantidad_gramos)) %>%
  distinct(sipsa_name, city, price, measurement_unit) %>%
  arrange(sipsa_name, city)

write.xlsx(
  lista_unidades,
  file = file.path(output_lista_dir, "lista_unidades.xlsx"),
  overwrite = TRUE
)

# ============================================================
# Estandarizar precio a 500 g (solidos) o 1.000 ml (liquidos)
# ============================================================

panel_estandar <- panel_con_cantidad %>%
  mutate(
    objetivo_base = coalesce(objetivo_base, 500),
    densidad      = coalesce(densidad, 1),
    precio_base = case_when(
      !is.na(gramos_sku)                                           ~ (price / (gramos_sku / densidad)) * objetivo_base,
      str_to_lower(measurement_unit) %in% c("kilogramo", "gramo")  ~ (price / 1000) * 500,
      is.na(gramos_sku) & str_to_lower(measurement_unit) == "unidad" & !is.na(cantidad_gramos) &
        str_detect(str_to_lower(sipsa_name), "huevo") ~
        (price / (cantidad_gramos * unidades_paquete)) * 500,
      is.na(gramos_sku) & str_to_lower(measurement_unit) == "unidad" & !is.na(cantidad_gramos) ~
        (price / cantidad_gramos) * 500,
      TRUE ~ NA_real_
    )
  )

# ============================================================
# Panel final: mediana de precio_base por
## alimento-ciudad-FECHA (nivel diario).
# ============================================================

panel_final <- panel_estandar %>%
  group_by(sipsa_name, city, fecha) %>%
  summarise(
    precio_base       = round(median(precio_base, na.rm = TRUE), 0),
    objetivo_base     = first(objetivo_base),
    densidad          = first(densidad),
    price             = median(price, na.rm = TRUE),
    unit_price        = first(unit_price),
    measurement_unit  = first(measurement_unit),
    tcac_code         = first(tcac_code),
    dia               = first(dia),
    mes               = first(mes),
    .groups = "drop"
  ) %>%
  arrange(fecha, sipsa_name, city)

# ============================================================
# Guardar outputs
# ============================================================

write.xlsx(
  lista_final,
  file = file.path(output_lista_dir, "lista_alimentos.xlsx"),
  overwrite = TRUE
)

saveRDS(lista_representativa, file.path(output_lista_dir, "skus_todos.rds"))

saveRDS(panel_final, file.path(output_panel_dir, "panel_v1.rds"))
