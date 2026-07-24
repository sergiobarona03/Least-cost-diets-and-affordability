########################################################
## SCRIPT 01_webscrap_prep/01_construccion_panel.R
## Construye el panel de precios a partir de los rds mensuales
## generados por 00_csv_a_rds.R: limpia, tipifica, elige el sku
## representativo por alimento/ciudad (más cercano a 500g/1000ml)
## y arma el panel final.
##
## Reads:  panel_dir/data/agosto-julio.septiembre.rds
## Writes: output_panel_dir/panel_v1.rds
##         output_lista_dir/lista_alimentos.xlsx
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

raw_mensual_dir <- file.path(panel_dir, "data")

output_dir       <- file.path(base_dir, "output")
output_panel_dir <- file.path(output_dir, "paneles/raw_mensual")
output_lista_dir <- file.path(output_dir, "lista_alimentos")

dir.create(output_panel_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_lista_dir, recursive = TRUE, showWarnings = FALSE)

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

# Guardar cada panel individual (equivalente a panel_julio.rds, etc.)
iwalk(paneles, function(df, nombre) {
  saveRDS(df, file.path(output_panel_dir, paste0("panel_", nombre, ".rds")))
})

# ============================================================
# Panel temporal combinado
# ============================================================

panel_final_temp <- bind_rows(paneles) %>%
  arrange(orden_mes, fecha, sipsa_name, city, sku_code)

# ============================================================
# Lista representativa por alimento y ciudad
# (sku más cercano a 500g o 1000ml, priorizando más fechas)
# ============================================================

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
    
    objetivo = case_when(
      unidad_extraida %in% c("g", "gr", "gramo", "gramos") ~ "500 gramos",
      unidad_extraida %in% c("ml", "mililitro", "mililitros") ~ "1000 mililitros",
      unidad_extraida %in% c("litro", "litros") ~ "1000 mililitros",
      TRUE ~ NA_character_
    ),
    
    distancia_objetivo = case_when(
      unidad_extraida %in% c("g", "gr", "gramo", "gramos") ~ abs(cantidad_extraida - 500),
      unidad_extraida %in% c("ml", "mililitro", "mililitros") ~ abs(cantidad_extraida - 1000),
      unidad_extraida %in% c("litro", "litros") ~ abs(cantidad_extraida - 1),
      TRUE ~ NA_real_
    )
  )  %>% group_by(sipsa_name, city, sku_code, exito_name) %>%
  summarise(
    n_fechas = n_distinct(fecha),
    cantidad_extraida = first(cantidad_extraida),
    objetivo = first(objetivo),
    distancia_objetivo = first(distancia_objetivo),
    .groups = "drop"
  ) %>%
  group_by(sipsa_name, city) %>%
  arrange(
    is.na(distancia_objetivo),
    distancia_objetivo,
    desc(n_fechas)
  ) %>%
  ungroup()

# ============================================================
# Panel final con sku representativo
# ============================================================

panel_final <- panel_final_temp %>%
  inner_join(
    lista_representativa %>% select(sipsa_name, city, sku_code),
    by = c("sipsa_name", "city", "sku_code")
  ) %>%
  group_by(sipsa_name, city, fecha) %>%
  summarise(
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
  lista_representativa,
  file = file.path(output_lista_dir, "lista_alimentos.xlsx"),
  overwrite = TRUE
)

saveRDS(panel_final, file.path(output_panel_dir, "panel_v1.rds"))

cat("\nListo. panel_v1.rds y lista_alimentos.xlsx guardados en", output_dir, "\n")
