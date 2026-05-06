# ============================================================
# Librerías
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(openxlsx)


# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"

ruta_base <- "C:/Users/danie/OneDrive/Documentos/input panel"

ruta_salida <- file.path(base_dir, "interno/output/paneles")
ruta_lista  <- file.path(base_dir, "interno/output/lista alimentos")

ruta_julio      <- file.path(ruta_base, "julio")
ruta_agosto     <- file.path(ruta_base, "agosto")
ruta_septiembre <- file.path(ruta_base, "septiembre")


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
# Fecha desde nombre archivo
# ============================================================

extraer_fecha_archivo <- function(archivo) {
  basename(archivo) %>%
    str_extract("\\d{2}-\\d{2}-\\d{4}") %>%
    dmy()
}


# ============================================================
# Construcción panel mensual
# ============================================================

construir_panel_mes <- function(ruta_mes, orden_mes) {
  
  archivos_csv <- list.files(
    path = ruta_mes,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  map_dfr(archivos_csv, function(archivo) {
    
    fecha_archivo <- extraer_fecha_archivo(archivo)
    
    read_csv(
      file = archivo,
      locale = locale(encoding = "UTF-8"),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) %>%
      clean_names() %>%
      select(any_of(variables_panel)) %>%
      mutate(
        sipsa_name = as.character(sipsa_name),
        city = case_when(
          str_to_lower(str_squish(city)) %in% c("bogotá", "bogotá, d.c.", "bogota", "bogota, d.c.") ~ "Bogotá",
          TRUE ~ str_squish(city)
        ),
        sku_code = as.character(sku_code),
        exito_name = as.character(exito_name),
        price = parse_number(price),
        unit_price = as.character(unit_price),
        measurement_unit = as.character(measurement_unit),
        tcac_code = str_remove(as.character(tcac_code), ",.*$"),
        fecha = fecha_archivo,
        dia = day(fecha),
        mes = month(fecha),
        orden_mes = orden_mes
      ) %>%
      filter(
        !is.na(sipsa_name),
        !is.na(city),
        !is.na(sku_code),
        str_detect(sku_code, "^[0-9]+$"),
        !str_detect(city, "^http"),
        !str_detect(unit_price, "T|\\+00:00"),
        price > 0
      )
  }) %>%
    arrange(fecha, sipsa_name, city, sku_code)
}


# ============================================================
# Paneles mensuales
# ============================================================

panel_julio <- construir_panel_mes(ruta_julio, 1)
panel_agosto <- construir_panel_mes(ruta_agosto, 2)
panel_septiembre <- construir_panel_mes(ruta_septiembre, 3)


# ============================================================
# Panel temporal con exito_name
# ============================================================

panel_final_temp <- bind_rows(
  panel_julio,
  panel_agosto,
  panel_septiembre
) %>%
  arrange(orden_mes, fecha, sipsa_name, city, sku_code)


# ============================================================
# Lista representativa por alimento y ciudad
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
  ) %>%
  group_by(sipsa_name, city, sku_code, exito_name) %>%
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
  slice(1) %>%
  ungroup()


# ============================================================
# Panel final con SKU representativo
# ============================================================

panel_final <- panel_final_temp %>%
  inner_join(
    lista_representativa %>%
      select(sipsa_name, city, sku_code),
    by = c("sipsa_name", "city", "sku_code")
  ) %>%
  arrange(orden_mes, fecha, sipsa_name, city, sku_code) %>%
  select(
    sipsa_name,
    city,
    sku_code,
    price,
    unit_price,
    measurement_unit,
    tcac_code,
    fecha,
    dia,
    mes
  )

# ============================================================
# Guardar lista representativa
# ============================================================

write.xlsx(
  lista_representativa,
  file = file.path(ruta_lista, "lista_alimentos.xlsx"),
  overwrite = TRUE
)

# ============================================================
# Guardar paneles
# ============================================================

saveRDS(panel_julio, file.path(ruta_salida, "panel_julio.rds"))
saveRDS(panel_agosto, file.path(ruta_salida, "panel_agosto.rds"))
saveRDS(panel_septiembre, file.path(ruta_salida, "panel_septiembre.rds"))
saveRDS(panel_final, file.path(ruta_salida, "panel_final.rds"))
