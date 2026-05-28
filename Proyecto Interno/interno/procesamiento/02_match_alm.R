# ============================================================
# 02. Lista alimentos y PANEL BALANCEADO
# ============================================================

library(tidyverse)
library(dplyr)
library(openxlsx)
library(stringi)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"

ruta_lista <- file.path(base_dir, "interno/output/lista alimentos")
ruta_panel <- file.path(base_dir, "interno/output/paneles")

# ============================================================
# Cargar panel final
# ============================================================

panel_final <- readRDS(file.path(ruta_panel, "panel.rds")) %>%
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
  "AREPAS RELLENAS CON ALGO", "BOCADILLOS",
  "CEREAL ALIMENTO PARA BEBÉ", "CEREAL PARA DESAYUNO", "CHOCOLATE INSTANTANEO",
  "CHORIZO", "GALLETAS DE SAL", "GALLETAS DULCES", "GALLETAS INTEGRALES",
  "GASEOSAS", "GELATINA O FLAN", "HARINA PARA TORTAS", "HELADOS DE CREMA",
  "JAMÓN", "JUGOS INSTANTANEOS O EN POLVO", "JUGOS PROCESADOS",
  "MALTAS", "MAYONESA", "MERMELADA", "MORTADELA",
  "PIZZA", "SALCHICHAS", "SALCHICHÓN", "SALSA DE TOMATE",
  "SOPAS", "YOGOURT", "CREMA DE LECHE", "PAPAS FRITAS",
  "CILANTRO", "COLOR", "COMINOS", "LAUREL", "MOSTAZA",
  "PIMIENTA", "TOMILLO", "REVUELTO VERDE",
  "ALMUERZO CORRIENTE O EJECUTIVO", "ALMUERZO ESPECIAL O A LA CARTA",
  "CHOCOLATE EN PASTA", "CAFÉ INSTANTÁNEO", "CAFÉ MOLIDO", "COMBOS",
  "CREMAS", "TINTO",
  "HAMBURGUESA", "KUMIS", "JUGOS NATURALES", "SUERO",
  "BOCADILLO VELEÑO", "Color (bolsita)",
  "Mayonesa doy pack", "Mostaza doy pack", "Salsa de tomate doy pack",
  "Jugo instantáneo (sobre)", "Galletas saladas", "Gelatina", "Margarina",
  "Chocolate instantáneo", "Chocolate amargo", "Chocolate dulce",
  "Vinagre"
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
# Lista de alimentos con mínimo 70 fechas POR CIUDAD
# ============================================================

lista_por_ciudad <- panel_filtrado %>%
  group_by(city, sku_code, sipsa_name) %>%
  summarise(n_fechas = n_distinct(fecha), .groups = "drop") %>%
  filter(n_fechas >= 70)

# ============================================================
# Panel balanceado: cada combinación ciudad-alimento cumple el criterio
# ============================================================

panel_balanceado <- panel_filtrado %>%
  semi_join(lista_por_ciudad, by = c("city", "sku_code", "sipsa_name"))

# ============================================================
# Total de ciudades y lista de ciudades
# ============================================================

total_ciudades <- panel_balanceado %>%
  summarise(n = n_distinct(city)) %>%
  pull(n)

todas_las_ciudades <- panel_balanceado %>%
  distinct(city) %>%
  pull(city) %>%
  sort()

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
print(lista_total, n = Inf)

# ============================================================
# Guardar outputs
# ============================================================

# 1. Conteo de alimentos por ciudad
write.xlsx(alimentos_por_ciudad,
           file.path(ruta_lista, "conteo_alimentos.xlsx"),
           overwrite = TRUE)

# 2. Lista total de alimentos únicos (unión de todas las ciudades)
write.xlsx(lista_total,
           file.path(ruta_lista, "lista_total_alimentos.xlsx"),
           overwrite = TRUE)

# 3. Excel con una hoja por ciudad
wb <- createWorkbook()

for (cd in todas_las_ciudades) {
  df_ciudad <- lista_por_ciudad %>%
    filter(city == cd) %>%
    select(sku_code, sipsa_name, n_fechas) %>%
    arrange(sipsa_name)
  addWorksheet(wb, sheetName = cd)
  writeData(wb, sheet = cd, df_ciudad)
}

saveWorkbook(wb,
             file.path(ruta_lista, "alimentos_por_ciudad_detalle.xlsx"),
             overwrite = TRUE)

# 4. Panel balanceado
saveRDS(panel_balanceado,
        file.path(ruta_panel, "panel_balanceado.rds"))