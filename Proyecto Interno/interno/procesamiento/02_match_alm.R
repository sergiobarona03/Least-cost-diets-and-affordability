# ============================================================
# 02. Lista alimentos y PANEL BALANCEADO (final)
# ============================================================

# Librerías
library(tidyverse)
library(dplyr)
library(openxlsx)
library(stringi)


# ============================================================
# Base dir
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"


# ============================================================
# Rutas
# ============================================================

ruta_lista <- file.path(base_dir, "interno/output/lista alimentos")
ruta_panel <- file.path(base_dir, "interno/output/paneles")


# ============================================================
# Cargar panel final
# ============================================================

panel_final <- readRDS(file.path(ruta_panel, "panel.rds")) %>%
  mutate(
    city = str_squish(as.character(city)),
    city = case_when(
      str_detect(str_to_lower(city), "^cartagena") ~ "Cartagena",
      TRUE ~ city
    )
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


# ============================================================
# Normalizar vector de exclusión
# ============================================================

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
    !sipsa_name_norm %in% alimentos_excluir_norm
  ) %>%
  select(-sipsa_name_norm)


# ============================================================
# Lista de alimentos (>=70 fechas)
# ============================================================

lista_balanceada <- panel_filtrado %>%
  group_by(sku_code, sipsa_name) %>%
  summarise(n_fechas = n_distinct(fecha), .groups = "drop") %>%
  filter(n_fechas >= 70)


# ============================================================
# Panel balanceado
# ============================================================

panel_balanceado <- panel_filtrado %>%
  inner_join(lista_balanceada, by = c("sku_code", "sipsa_name"))


# ============================================================
# 1. Conteo de alimentos por ciudad
# ============================================================

alimentos_por_ciudad <- panel_balanceado %>%
  distinct(city, sipsa_name) %>%
  count(city, name = "n_alimentos") %>%
  arrange(desc(n_alimentos))


# ============================================================
# 2. Alimentos presentes en TODAS las ciudades
# ============================================================

total_ciudades <- panel_balanceado %>%
  summarise(n = n_distinct(city)) %>%
  pull(n)

alimentos_totales <- panel_balanceado %>%
  distinct(city, sipsa_name) %>%
  group_by(sipsa_name) %>%
  summarise(n_ciudades = n_distinct(city), .groups = "drop") %>%
  filter(n_ciudades == total_ciudades) %>%
  arrange(sipsa_name)


# ============================================================
# Chequeos
# ============================================================

alimentos_por_ciudad

nrow(alimentos_totales)

alimentos_totales


# ============================================================
# Guardar outputs
# ============================================================

write.xlsx(
  alimentos_por_ciudad,
  file = file.path(ruta_lista, "conteo_alimentos.xlsx"),
  overwrite = TRUE
)

write.xlsx(
  alimentos_totales,
  file = file.path(ruta_lista, "alimentos_total_ciudad.xlsx"),
  overwrite = TRUE
)

saveRDS(panel_balanceado, file.path(ruta_panel, "panel_balanceado.rds"))
