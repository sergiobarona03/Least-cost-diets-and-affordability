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
    city      = str_squish(as.character(city)),
    city      = case_when(
      str_detect(str_to_lower(city), "^cartagena") ~ "Cartagena",
      TRUE ~ city
    ),
    sku_code  = as.character(sku_code),
    sipsa_name = str_squish(as.character(sipsa_name)),
    fecha     = as.Date(fecha),
    mes       = format(fecha, "%Y-%m")   
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
# Corregir SKUs duplicados con nombres distintos
# (mismo SKU, mismo precio → mismo producto, nombre erróneo)
# ============================================================

panel_filtrado <- panel_filtrado %>%
  mutate(
    sipsa_name = case_when(
      sku_code == "1165839" & sipsa_name == "Maíz amarillo trillado" ~ "Maíz amarillo cáscara",
      sku_code == "154893"  & sipsa_name == "Carne de res, murillo"  ~ "Carne de res, morrillo",
      TRUE ~ sipsa_name
    )
  )

# ============================================================
# Verificar que no queden más SKUs duplicados
# (corre esto y el resultado debe estar vacío)
# ============================================================

skus_duplicados <- panel_filtrado %>%
  distinct(sku_code, sipsa_name) %>%
  group_by(sku_code) %>%
  filter(n() > 1) %>%
  arrange(sku_code)

if (nrow(skus_duplicados) > 0) {
  cat("⚠️  Aún hay SKUs duplicados:\n")
  print(skus_duplicados)
} else {
  cat("✅ Sin SKUs duplicados. Puedes continuar.\n")
}

# ============================================================
# Lista de alimentos con mínimo 70 fechas en total
# ============================================================

lista_balanceada <- panel_filtrado %>%
  group_by(sku_code, sipsa_name) %>%
  summarise(n_fechas = n_distinct(fecha), .groups = "drop") %>%
  filter(n_fechas >= 70)

# ============================================================
# Panel balanceado preliminar
# ============================================================

panel_balanceado <- panel_filtrado %>%
  inner_join(lista_balanceada, by = c("sku_code", "sipsa_name"))

# ============================================================
# Total de ciudades
# ============================================================

total_ciudades <- panel_balanceado %>%
  summarise(n = n_distinct(city)) %>%
  pull(n)

# ============================================================
# Alimentos presentes en TODAS las ciudades
# ============================================================

alimentos_totales <- panel_balanceado %>%
  distinct(city, sku_code, sipsa_name) %>%
  group_by(sku_code, sipsa_name) %>%
  summarise(n_ciudades = n_distinct(city), .groups = "drop") %>%
  filter(n_ciudades == total_ciudades) %>%
  arrange(sipsa_name, sku_code)

# ============================================================
# Panel balanceado final
# ============================================================

panel_balanceado <- panel_balanceado %>%
  semi_join(alimentos_totales, by = c("sku_code", "sipsa_name"))

# ============================================================
# Chequeos finales
# ============================================================

alimentos_por_ciudad <- panel_balanceado %>%
  distinct(city, sku_code, sipsa_name) %>%
  count(city, name = "n_alimentos") %>%
  arrange(desc(n_alimentos))

cat("\nTotal ciudades:  ", total_ciudades, "\n")
cat("Alimentos finales:", nrow(alimentos_totales), "\n\n")

print(alimentos_por_ciudad)
print(alimentos_totales, n = Inf)

# ============================================================
# Guardar outputs
# ============================================================

write.xlsx(alimentos_por_ciudad,
           file.path(ruta_lista, "conteo_alimentos.xlsx"),
           overwrite = TRUE)

write.xlsx(alimentos_totales,
           file.path(ruta_lista, "alimentos_total_ciudad.xlsx"),
           overwrite = TRUE)

saveRDS(panel_balanceado,
        file.path(ruta_panel, "panel_balanceado.rds"))