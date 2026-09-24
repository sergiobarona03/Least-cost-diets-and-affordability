########################################################
## SCRIPT 01_webscrap_prep/01_construccion_panel.R
##
## Construye el panel de precios a partir de los rds 
## mensuales generados por 00_csv_a_rds.R: limpia, tipifica,
## elige el sku representativo por alimento/ciudad y 
## estandariza el precio a 500g/1000ml.
##
## Criterio de seleccion del sku representativo: primero se
## descartan los skus que no cumplirian el umbral de cobertura
## de fechas de su ciudad (85%, mismo criterio que usa despues
## 02a_lista_alimentos.R); entre los que SI cumplen, se elige el
## de MENOR precio por gramo, con preferencia por los que pesan
## por debajo de un umbral de tamano (umbral_gramos, default
## 500g), cayendo a comparar todos los tamanos si ninguno cumple.
## La funcion: aux-functions/seleccionar_sku_umbral.R
##
## Reads:  panel_dir/data/*.rds
##         dataprep_dir/unidades/lista_unidades gramos.xlsx
## Writes: output_panel_dir/panel_v1.rds  (diario, con
##         precio_500g ya calculado)
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

base_dir  <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
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

source(file.path(aux_dir, "seleccionar_sku_umbral.R"))

# ============================================================
# Umbral de tamano (gramos) para elegir sku de menor precio/gramo.
# umbral_gramos = NULL equivale a "ningun umbral": se compara el
# precio por gramo entre todos los skus del alimento-ciudad.
# ============================================================

umbral_gramos <- 500

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
  
  # Leer ruta
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
# Info por sku: tamaño de alimento (extraido del nombre exito) y
## precio mediano del sku a lo largo de las fechas (base para
## calcular precio por gramo antes de elegir el sku).
# ============================================================

lista_representativa <- panel_final_temp %>%
  mutate(
    # Pasar el text
    texto_exito = str_to_lower(exito_name),
    
    # Variable de medida
    medida = str_extract(
      texto_exito,
      "\\d+(?:[\\.,]\\d+)?\\s*(g|gr|gramo|gramos|ml|mililitro|mililitros|litro|litros)"
    ),
    
    # Cantidad extraida
    cantidad_extraida = medida %>%
      str_extract("\\d+(?:[\\.,]\\d+)?") %>%
      str_replace(",", ".") %>%
      as.numeric(),
    
    
    unidad_extraida = medida %>%
      str_extract("g|gr|gramo|gramos|ml|mililitro|mililitros|litro|litros"),

    objetivo = case_when(
      unidad_extraida %in% c("g", "gr", "gramo", "gramos") ~ "500 gramos",
      unidad_extraida %in% c("ml", "mililitro", "mililitros") ~ "1000 mililitros",
      unidad_extraida %in% c("litro", "litros") ~ "1000 mililitros",
      TRUE ~ NA_character_
    ),

    # Tamano del sku en GRAMOS (litros/ml se tratan 1:1 como gramos,
    # igual que el resto del pipeline -- no se distingue densidad).
    gramos_sku = case_when(
      unidad_extraida %in% c("g", "gr", "gramo", "gramos")    ~ cantidad_extraida,
      unidad_extraida %in% c("ml", "mililitro", "mililitros") ~ cantidad_extraida,
      unidad_extraida %in% c("litro", "litros")               ~ cantidad_extraida * 1000,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(sipsa_name, city, sku_code, exito_name) %>%
  summarise(
    n_fechas          = n_distinct(fecha),
    cantidad_extraida = first(cantidad_extraida),
    objetivo          = first(objetivo),
    gramos_sku        = first(gramos_sku),
    precio_mediano    = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    precio_gramo_sku = precio_mediano / gramos_sku
  )

# ============================================================
# Umbral de cobertura de fechas por ciudad (mismo calculo que
# 02a_lista_alimentos.R: 85% de las fechas disponibles de la
# ciudad). Se necesita ANTES de elegir el sku -- ver punto 0 de
# seleccionar_sku_umbral() -- para que un sku barato pero con
# pocos dias de datos no se lleve al alimento completo con el.
# ============================================================

umbral_fechas_ciudad <- panel_final_temp %>%
  group_by(city) %>%
  summarise(fechas_disponibles = n_distinct(fecha), .groups = "drop") %>%
  mutate(umbral_fechas = floor(0.85 * fechas_disponibles)) %>%
  select(city, umbral_fechas)

# ============================================================
# Elegir el sku representativo por alimento-ciudad
# ============================================================

lista_final <- seleccionar_sku_umbral(lista_representativa,
                                      umbral_gramos = umbral_gramos,
                                      umbral_fechas_ciudad = umbral_fechas_ciudad) %>%
  select(-precio_mediano, -precio_gramo_sku, -gramos_sku)

# ============================================================
# Referencias de cantidad/objetivo por alimento-ciudad (de
# lista_final) y gramos por unidad para productos "por unidad"
# ============================================================

referencias <- lista_final %>%
  select(sipsa_name, city, cantidad_extraida, objetivo) %>%
  rename(objetivo_texto = objetivo) %>%
  distinct(sipsa_name, city, .keep_all = TRUE)

gramos_unidad <- read.xlsx(ruta_gramos_unidad) %>%
  dplyr::select(sipsa_name, city, cantidad_gramos = `cantidad.aproximada.gramos`) %>%
  distinct(sipsa_name, city, cantidad_gramos)

# ============================================================
# Panel con sku representativo + cantidad/objetivo/gramos pegados
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
  filter(is.na(objetivo_texto) & str_to_lower(measurement_unit) == "unidad" & is.na(cantidad_gramos)) %>%
  distinct(sipsa_name, city, price, measurement_unit) %>%
  arrange(sipsa_name, city)

write.xlsx(
  lista_unidades,
  file = file.path(output_lista_dir, "lista_unidades.xlsx"),
  overwrite = TRUE
)

# ============================================================
# Estandarizar precio a 500g o 1000ml
# ============================================================

panel_estandar <- panel_con_cantidad %>%
  mutate(
    objetivo = case_when(
      objetivo_texto == "500 gramos"      ~ 500,
      objetivo_texto == "1000 mililitros" ~ 1000,
      TRUE ~ NA_real_
    ),
    precio_500g = case_when(
      !is.na(objetivo_texto)                                       ~ (price / cantidad_extraida) * objetivo,
      str_to_lower(measurement_unit) %in% c("kilogramo", "gramo")  ~ (price / 1000) * 500,
      is.na(objetivo_texto) & str_to_lower(measurement_unit) == "unidad" & !is.na(cantidad_gramos) &
        str_detect(str_to_lower(sipsa_name), "huevo") ~
        (price / (cantidad_gramos * unidades_paquete)) * 500,
      is.na(objetivo_texto) & str_to_lower(measurement_unit) == "unidad" & !is.na(cantidad_gramos) ~
        (price / cantidad_gramos) * 500,
      TRUE ~ NA_real_
    )
  )

# ============================================================
# Panel final: mediana de precio_500g por
## alimento-ciudad-FECHA (nivel diario).
# ============================================================

panel_final <- panel_estandar %>%
  group_by(sipsa_name, city, fecha) %>%
  summarise(
    precio_500g       = round(median(precio_500g, na.rm = TRUE), 0),
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


saveRDS(panel_final, file.path(output_panel_dir, "panel_v1.rds"))

cat("\nListo. panel_v1.rds y lista_alimentos.xlsx guardados (umbral_gramos =",
    ifelse(is.null(umbral_gramos), "NULL (sin umbral)", umbral_gramos), ")\n")
cat("Panel en:", output_panel_dir, "\n")
cat("(Sigue: correr 01_webscrap_prep/02a_lista_alimentos.R con este panel.)\n")
