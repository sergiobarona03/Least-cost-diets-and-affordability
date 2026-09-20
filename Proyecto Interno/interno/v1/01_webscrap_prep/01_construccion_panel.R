########################################################
## SCRIPT v1/01_webscrap_prep/01_construccion_panel.R
##
## VERSION 1 (metodo original). Construye el panel de precios 
## a partir de los rds mensuales generados por 
## 00_csv_a_rds.R: limpia, tipifica, elige el/los sku 
## representativo(s) por alimento/ciudad. El sku MAS CERCANO a
## 500g/1000ml y estandariza el precio a 500g/1000ml.
##
## El pipeline en produccion (01_webscrap_prep/01_construccion_
## panel.R, fuera de v1/) usa el sku de MENOR precio por gramo
## bajo un umbral. Toda esta carpeta interno/v1/ es un archivo
## autocontenido de versiones anteriores del proyecto
##
## Reads:  panel_dir/data/*.rds  (los rds mensuales SI son los
##         originales de 01_webscrap_prep/data)
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

base_dir  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
panel_dir <- file.path(base_dir, "01_webscrap_prep")
v1_output <- file.path(base_dir, "v1/output")

raw_mensual_dir <- file.path(panel_dir, "data")
dataprep_dir    <- file.path(base_dir, "02_dataprep")

output_panel_dir <- file.path(v1_output, "paneles/raw_mensual")
output_lista_dir <- file.path(v1_output, "lista_alimentos")

ruta_gramos_unidad <- file.path(dataprep_dir, "unidades/lista_unidades gramos.xlsx")

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
# (calcula qué tan cerca está cada sku de 500g o 1000ml,
# solo ordena -- no filtra ni reduce nada todavía)
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
  ungroup()

# ============================================================
# Umbral: quedarse con el/los sku que tengan la MENOR distancia
# al objetivo, por alimento-ciudad. na.rm = TRUE es clave: si no,
# un solo sku sin medida (distancia NA) revienta el minimo de
# todo el grupo y se te cae el alimento completo de lista_final.
# Si NINGUN sku del alimento-ciudad tiene medida (se vende por
# unidad, ej. huevo/aguacate), se queda con TODOS -- entra igual
# al panel y se resuelve mas abajo con la tabla de gramos.
# ============================================================

lista_min <- lista_representativa %>%
  group_by(sipsa_name, city) %>%
  summarise(
    min_distancia = min(distancia_objetivo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(min_distancia = ifelse(is.infinite(min_distancia), NA_real_, min_distancia))

lista_final <- lista_representativa %>%
  left_join(lista_min, by = c("sipsa_name", "city")) %>%
  mutate(
    incluir = case_when(
      !is.na(min_distancia) & !is.na(distancia_objetivo) &
        distancia_objetivo == min_distancia ~ TRUE,
      is.na(min_distancia) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(incluir) %>%
  select(-min_distancia, -incluir)

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
# (para que sepas cuáles te falta completar en esa tabla)
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
# Productos con objetivo_texto: formula (P / cantidad_extraida) * objetivo
# Productos vendidos por kg o g sin empaque: (price / 1000) * 500
# Huevo por unidad: (price / (cantidad_gramos * unidades_paquete)) * 500
# Resto por unidad: (price / cantidad_gramos) * 500
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
# Panel final: mediana de precio_500g (y de price) por
# alimento-ciudad-FECHA (nivel diario). La agregación a mensual
# se hace después, en 01_panel_mensual_tcac.R 
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

cat("\nListo. panel_v1.rds y lista_alimentos.xlsx guardados en", v1_output, "\n")