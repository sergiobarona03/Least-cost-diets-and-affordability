########################################################
## SCRIPT 01_webscrap_prep/02a_lista_alimentos.R
## Excluye alimentos no deseados del panel_v1 y calcula, por
## ciudad, qué alimentos cumplen el umbral relativo de fechas
## (85% de los días en que su grupo GABA tiene datos en esa
## ciudad, ver aux-functions/umbral_fechas_grupo.R). Deja todo
## listo para que 02b arme el panel balanceado.
##
## Reads:  output_panel_dir/panel_v1.rds
##         proyecto_dir/composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx
## Writes: output_panel_dir/panel_v2.rds
##         output_panel_dir/lista_por_ciudad.rds
##         output_lista_dir/conteo_alimentos.xlsx
##         output_lista_dir/lista_total_alimentos.xlsx
##         output_lista_dir/alimentos_por_ciudad_detalle.xlsx
########################################################

library(tidyverse)
library(dplyr)
library(openxlsx)
library(stringi)
library(janitor)

# ============================================================
# Rutas
# ============================================================

proyecto_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
base_dir     <- file.path(proyecto_dir, "interno/")

output_dir       <- file.path(base_dir, "output")
output_panel_dir <- file.path(output_dir, "paneles/raw_mensual")
output_lista_dir <- file.path(output_dir, "lista_alimentos")
ruta_tcac        <- file.path(proyecto_dir, "composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx")

source(file.path(base_dir, "01_webscrap_prep/aux-functions/mapeo_tcac.R"),          encoding = "UTF-8")
source(file.path(base_dir, "01_webscrap_prep/aux-functions/umbral_fechas_grupo.R"), encoding = "UTF-8")

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
  "Vinagre", "Bocadillo veleño"
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
    !is.na(sipsa_name),
    !is.na(fecha)
  ) %>%
  select(-sipsa_name_norm)

# ============================================================
# Grupo GABA de cada alimento (frutas y verduras por separado);
# sin grupo: SIN CATEGORIA o sin mapeo
# ============================================================

grupos_alimentos <- unir_mapeo_tcac(
  panel_filtrado %>% distinct(sipsa_name),
  leer_mapeo_tcac(ruta_tcac)
) %>%
  clean_names() %>%
  transmute(
    sipsa_name,
    grupo = case_when(
      subgrupos_gabas %in% c("FRUTAS", "VERDURAS") ~ subgrupos_gabas,
      grupos_gabas == "SIN CATEGORIA"              ~ NA_character_,
      TRUE                                         ~ grupos_gabas
    )
  )

# ============================================================
# Lista de alimentos que cumplen el umbral relativo de fechas
# (85% de los días del grupo en cada ciudad)
# ============================================================

lista_por_ciudad <- lista_por_umbral_grupo(panel_filtrado, grupos_alimentos)

cat("\n====== Grupos con umbral distinto al de su ciudad ======\n")
lista_por_ciudad %>%
  filter(!is.na(grupo)) %>%
  distinct(city, grupo, fechas_disponibles, dias_grupo, umbral) %>%
  filter(umbral != floor(0.85 * fechas_disponibles)) %>%
  arrange(city, grupo) %>%
  print(n = Inf)

# ============================================================
# Total de ciudades y lista de ciudades
# ============================================================

todas_las_ciudades <- panel_filtrado %>%
  semi_join(lista_por_ciudad, by = c("city", "sipsa_name")) %>%
  distinct(city) %>%
  pull(city) %>%
  sort()

total_ciudades <- length(todas_las_ciudades)

# ============================================================
# Lista total: unión de todas las ciudades sin duplicados
# ============================================================

lista_total <- lista_por_ciudad %>%
  distinct(sipsa_name) %>%
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
    select(sipsa_name, grupo, n_fechas, umbral) %>%
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