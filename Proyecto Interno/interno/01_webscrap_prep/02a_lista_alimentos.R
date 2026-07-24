########################################################
## SCRIPT 01_webscrap_prep/02a_lista_alimentos.R
## Excluye alimentos no deseados del panel_v1 y calcula, por
## ciudad, qué alimentos cumplen el umbral relativo de fechas
## disponibles (85%). Deja todo listo para que 02b arme el
## panel balanceado.
##
## Reads:  output_panel_dir/panel_v1.rds
## Writes: output_panel_dir/vpanel_v2.rds
##         output_panel_dir/lista_por_ciudad.rds
##         output_lista_dir/conteo_alimentos.xlsx
##         output_lista_dir/lista_total_alimentos.xlsx
##         output_lista_dir/alimentos_por_ciudad_detalle.xlsx
########################################################

library(tidyverse)
library(dplyr)
library(openxlsx)
library(stringi)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/"

output_dir       <- file.path(base_dir, "output")
output_panel_dir <- file.path(output_dir, "paneles/raw_mensual")
output_lista_dir <- file.path(output_dir, "lista_alimentos")

# ============================================================
# Cargar panel final (output de 01_construccion_panel.R)
# ============================================================

panel_final <- readRDS(file.path(output_panel_dir, "panel_v1.rds")) %>%
  mutate(
    city       = str_squish(as.character(city)),
    city       = case_when(
      str_detect(str_to_lower(city), "^cartagena") ~ "Cartagena",
      TRUE ~ city
    ),
    sku_code   = as.character(sku_code),
    sipsa_name = str_squish(as.character(sipsa_name)),
    fecha      = as.Date(fecha),
    mes        = format(fecha, "%Y-%m")
  )

# ============================================================
# Alimentos a excluir
# ============================================================

alimentos_excluir <- c(
  "Color (bolsita)",
  "Mayonesa doy pack", "Mostaza doy pack", "Salsa de tomate doy pack",
  "Jugo instantáneo (sobre)", "Galletas saladas", "Gelatina", "Margarina",
  "Chocolate instantáneo", "Chocolate amargo", "Chocolate dulce",
  "Vinagre", "Leche en Polvo"
)

alimentos_excluir_norm <- alimentos_excluir %>%
  str_to_upper() %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  str_squish()

# ============================================================
# Filtrar alimentos excluidos
# ============================================================

panel_filtrado <- panel_final %>%
  mutate(
    sipsa_name_norm = sipsa_name %>%
      str_to_upper() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_squish()
  ) %>%
  filter(
    !sipsa_name_norm %in% alimentos_excluir_norm,
    !is.na(city),
    !is.na(sku_code),
    !is.na(sipsa_name),
    !is.na(fecha)
  ) %>%
  select(-sipsa_name_norm)

# ============================================================
# Umbral relativo: 85% de las fechas únicas disponibles por ciudad
# ============================================================

umbral_por_ciudad <- panel_filtrado %>%
  group_by(city) %>%
  summarise(
    fechas_disponibles = n_distinct(fecha),
    umbral             = floor(0.85 * fechas_disponibles),
    .groups = "drop"
  )

cat("\n====== Umbral por ciudad ======\n")
print(umbral_por_ciudad %>% arrange(umbral))

# ============================================================
# Lista de alimentos que cumplen el umbral relativo POR CIUDAD
# ============================================================

lista_por_ciudad <- panel_filtrado %>%
  group_by(city, sku_code, sipsa_name) %>%
  summarise(n_fechas = n_distinct(fecha), .groups = "drop") %>%
  left_join(umbral_por_ciudad, by = "city") %>%
  filter(n_fechas >= umbral) %>%
  select(city, sku_code, sipsa_name, n_fechas, fechas_disponibles, umbral)

# ============================================================
# Total de ciudades y lista de ciudades
# ============================================================

todas_las_ciudades <- panel_filtrado %>%
  semi_join(lista_por_ciudad, by = c("city", "sku_code", "sipsa_name")) %>%
  distinct(city) %>%
  pull(city) %>%
  sort()

total_ciudades <- length(todas_las_ciudades)

# ============================================================
# Lista total: unión de todas las ciudades sin duplicados
# ============================================================

lista_total <- lista_por_ciudad %>%
  distinct(sku_code, sipsa_name) %>%
  arrange(sipsa_name)

# ============================================================
# Chequeos finales
# ============================================================

alimentos_por_ciudad <- lista_por_ciudad %>%
  count(city, name = "n_alimentos") %>%
  arrange(desc(n_alimentos))

cat("\nTotal ciudades:          ", total_ciudades, "\n")
cat("Alimentos únicos totales:", nrow(lista_total), "\n\n")

print(alimentos_por_ciudad)

# ============================================================
# Guardar outputs
# ============================================================

write.xlsx(alimentos_por_ciudad,
           file.path(output_lista_dir, "conteo_alimentos.xlsx"),
           overwrite = TRUE)

write.xlsx(lista_total,
           file.path(output_lista_dir, "lista_total_alimentos.xlsx"),
           overwrite = TRUE)

wb <- createWorkbook()

for (cd in todas_las_ciudades) {
  df_ciudad <- lista_por_ciudad %>%
    filter(city == cd) %>%
    select(sku_code, sipsa_name, n_fechas, umbral) %>%
    arrange(sipsa_name)
  addWorksheet(wb, sheetName = cd)
  writeData(wb, sheet = cd, df_ciudad)
}

saveWorkbook(wb,
             file.path(output_lista_dir, "alimentos_por_ciudad_detalle.xlsx"),
             overwrite = TRUE)

# Insumos para 02b_panel_balanceado.R
saveRDS(panel_filtrado, file.path(output_panel_dir, "panel_v2.rds"))
saveRDS(lista_por_ciudad, file.path(output_panel_dir, "lista_por_ciudad.rds"))

cat("\nListo. panel_filtrado.rds y lista_por_ciudad.rds guardados para 02b.\n")