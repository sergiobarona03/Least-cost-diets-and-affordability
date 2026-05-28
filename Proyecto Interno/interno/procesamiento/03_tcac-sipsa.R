# ============================================================
# 03. Comparar lista de alimentos con TCAC y agregar info nutricional
# ============================================================

library(tidyverse)
library(openxlsx)
library(stringi)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"

ruta_lista  <- file.path(base_dir, "interno/output/lista alimentos")
ruta_tcac   <- file.path(base_dir, "composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx")
ruta_output <- ruta_lista

# ============================================================
# Cargar lista total de alimentos
# ============================================================

lista_total <- read.xlsx(file.path(ruta_lista, "lista_total_alimentos.xlsx"))

# ============================================================
# Cargar TCAC
# ============================================================

tcac <- read.xlsx(ruta_tcac, sheet = "Imputada") %>%
  rename(sipsa_name = `Alimento.(Nombre.sipsa)`) %>%
  mutate(sipsa_name = str_squish(as.character(sipsa_name)))

# ============================================================
# Correcciones manuales en lista_total antes del join
# ============================================================

lista_total <- lista_total %>%
  mutate(
    sipsa_name_join = case_when(
      sku_code == "1519" & sipsa_name == "Ajo importado" ~ "Ajo",
      TRUE ~ sipsa_name
    )
  )

# ============================================================
# Normalizar nombres para el join
# ============================================================

normalizar <- function(x) {
  x %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_squish()
}

lista_total <- lista_total %>%
  mutate(sipsa_name_norm = normalizar(sipsa_name_join))

tcac <- tcac %>%
  mutate(sipsa_name_norm = normalizar(sipsa_name))

# ============================================================
# Join por nombre normalizado
# ============================================================

tcac_unico <- tcac %>%
  distinct(sipsa_name_norm, .keep_all = TRUE)

lista_con_nut <- lista_total %>%
  left_join(
    tcac_unico %>% select(-sipsa_name),
    by = "sipsa_name_norm"
  ) %>%
  select(-sipsa_name_norm, -sipsa_name_join) %>%
  relocate(sku_code, sipsa_name)

# ============================================================
# Diagnóstico: alimentos sin match
# ============================================================

sin_match <- lista_con_nut %>%
  filter(is.na(Codigo_TCAC)) %>%
  select(sku_code, sipsa_name)

cat("\nAlimentos con info nutricional:", nrow(lista_con_nut) - nrow(sin_match), "\n")
cat("Alimentos SIN match en TCAC:   ", nrow(sin_match), "\n\n")

# ============================================================
# Guardar outputs
# ============================================================

write.xlsx(lista_con_nut,
           file.path(ruta_output, "composicion_270526.xlsx"),
           overwrite = TRUE)
