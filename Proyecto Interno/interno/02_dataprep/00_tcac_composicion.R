########################################################
## SCRIPT 02_dataprep/00_tcac_composicion.R
## Cruza la lista total de alimentos (output de 01_webscrap_prep)
## con la tabla TCAC de composición nutricional y guarda la
## composición nutricional por alimento (sku_code + sipsa_name).
##
## Reads:  output_dir/lista alimentos/lista_total_alimentos.xlsx
##         proyecto_dir/composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx
## Writes: output_dir/tcac/composicion_310526.xlsx
##         output_dir/tcac/composicion_310526.rds
########################################################

library(tidyverse)
library(openxlsx)
library(stringi)
library(janitor)

# ============================================================
# Rutas
# ============================================================

proyecto_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
interno_dir  <- file.path(proyecto_dir, "interno")
output_dir   <- file.path(interno_dir, "output")

ruta_lista  <- file.path(output_dir, "lista_alimentos")
ruta_tcac   <- file.path(proyecto_dir, "composicion-nut/1823_mapeo_sipsa_tcac v1.0_2025.xlsx")
ruta_output <- file.path(output_dir, "tcac")

dir.create(ruta_output, recursive = TRUE, showWarnings = FALSE)

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
      # Correcciones mapeadas por sku_code al nombre exacto en TCAC
      sku_code == "1519"    & sipsa_name == "Ajo importado"                  ~ "Ajo",
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
# Guardar outputs
# ============================================================

lista_con_nut_clean <- lista_con_nut %>%
  clean_names()

write.xlsx(lista_con_nut_clean,
           file.path(ruta_output, "composicion_270726.xlsx"),
           overwrite = TRUE)

saveRDS(lista_con_nut_clean,
        file.path(ruta_output, "composicion_270726.rds"))

cat("\nListo. composicion_270726.rds guardado en", ruta_output, "\n")