########################################################
## SCRIPT 01_webscrap_prep/03_descriptivas_scraping.R
## Cifras descriptivas del scraping de precios de julio a septiembre de
## 2025: archivos, filas, productos, filas sin precio y cobertura por
## ciudad y por mes. Alimentan la seccion de datos del working paper.
##
## Reads:  panel_dir/data/julio.rds, agosto.rds, septiembre.rds
##         output_dir/paneles/raw_mensual/panel_v1.rds
##
## Writes: output_dir/descriptivas/scraping.rds
##         output_dir/descriptivas/scraping.xlsx
########################################################

library(tidyverse)
library(writexl)

# ============================================================
# Rutas
# ============================================================

base_dir   <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"
panel_dir  <- file.path(base_dir, "01_webscrap_prep")
output_dir <- file.path(base_dir, "output")
desc_dir   <- file.path(output_dir, "descriptivas")

dir.create(desc_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Filas crudas del scraper
# ============================================================

crudo <- map_dfr(c(julio = "julio", agosto = "agosto", septiembre = "septiembre"), function(m) {
  readRDS(file.path(panel_dir, "data", paste0(m, ".rds"))) %>%
    transmute(mes = m, archivo = archivo_origen, sipsa_name, sku_code,
              ciudad = str_squish(as.character(city)),
              precio = parse_number(as.character(price)))
}) %>%
  mutate(
    ciudad = case_when(
      str_detect(str_to_lower(ciudad), "^bogot") ~ "Bogotá",
      str_detect(str_to_lower(ciudad), "^cartagena") ~ "Cartagena",
      TRUE ~ ciudad
    )
  )

total <- tibble(
  archivos        = n_distinct(crudo$archivo),
  filas           = nrow(crudo),
  productos       = n_distinct(crudo$sku_code),
  alimentos_sipsa = n_distinct(crudo$sipsa_name),
  filas_precio_cero = sum(crudo$precio == 0, na.rm = TRUE),
  pct_precio_cero   = 100 * mean(crudo$precio == 0, na.rm = TRUE)
)

por_mes <- crudo %>%
  group_by(mes) %>%
  summarise(archivos = n_distinct(archivo), filas = n(),
            pct_precio_cero = 100 * mean(precio == 0, na.rm = TRUE), .groups = "drop")

# ============================================================
# Panel limpio: cobertura por ciudad
# ============================================================

panel_v1 <- readRDS(file.path(output_dir, "paneles/raw_mensual/panel_v1.rds"))

por_ciudad <- panel_v1 %>%
  group_by(ciudad = city) %>%
  summarise(fechas = n_distinct(fecha), alimentos_con_precio = n_distinct(sipsa_name),
            observaciones = n(), .groups = "drop") %>%
  left_join(
    readRDS(file.path(output_dir, "lista_alimentos/skus_todos.rds")) %>%
      group_by(ciudad = city) %>%
      summarise(productos = n_distinct(sku_code), .groups = "drop"),
    by = "ciudad"
  ) %>%
  arrange(desc(observaciones))

por_mes_panel <- panel_v1 %>%
  group_by(mes = month(fecha)) %>%
  summarise(fechas = n_distinct(fecha), observaciones = n(), .groups = "drop")

salida <- list(total = total, por_mes = por_mes, por_ciudad = por_ciudad,
               por_mes_panel = por_mes_panel)

saveRDS(salida, file.path(desc_dir, "scraping.rds"))
write_xlsx(salida, file.path(desc_dir, "scraping.xlsx"))

message("Done.")
