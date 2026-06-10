# ============================================================
# 03. Comparar lista de alimentos con TCAC y agregar info nutricional
# ============================================================

library(tidyverse)
library(openxlsx)
library(stringi)
library(janitor)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"

ruta_lista  <- file.path(base_dir, "interno/output/lista alimentos")
ruta_tcac   <- file.path(base_dir, "composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx")
ruta_output <- file.path(base_dir, "interno/output/tcac")

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
      # Corrección preexistente
      sku_code == "1519"    & sipsa_name == "Ajo importado"                  ~ "Ajo",
      
      # Correcciones nuevas — mapeadas por sku_code al nombre exacto en TCAC
      sku_code == "869594"  & sipsa_name == "Almejas con concha"             ~ "Almejas",
      sku_code == "871595"  & sipsa_name == "Bagre rayado en postas congelado" ~ "Bagre rayado",
      sku_code == "855053"  & sipsa_name == "Carne de cerdo, lomo sin hueso" ~ "Carne de cerdo, lomo",
      sku_code == "1523894" & sipsa_name == "Carne de cerdo, pernil sin hueso" ~ "Carne de cerdo, lomo",
      sku_code == "723282"  & sipsa_name == "Trucha en corte mariposa"       ~ "Trucha",
      sku_code == "1101"    & sipsa_name == "Uva roja"                       ~ "Uva comun",
      sku_code == "1601993" & sipsa_name == "Yuca ICA"                       ~ "Yuca",
      
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

# Limpiar nombres de columnas
lista_con_nut_clean <- lista_con_nut %>%
  clean_names()

# Excel 
write.xlsx(lista_con_nut_clean,
           file.path(ruta_output, "composicion_310526.xlsx"),
           overwrite = TRUE)

# RDS 
saveRDS(lista_con_nut_clean,
        file.path(ruta_output, "composicion_310526.rds"))
