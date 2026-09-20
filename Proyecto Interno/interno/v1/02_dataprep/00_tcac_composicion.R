########################################################
## SCRIPT v1/02_dataprep/00_tcac_composicion.R
## Version v1: mismo script que el pipeline en produccion,
## leyendo y escribiendo dentro de v1/output/ en vez de output/.
## La tabla TCAC en si (mapeo nutricional) no depende del metodo
## de precio, asi que se lee del proyecto real, sin duplicar.
##
## Cruza la lista total de alimentos (output de v1/01_webscrap_prep)
## con la tabla TCAC de composición nutricional y guarda la
## composición nutricional por alimento (sipsa_name).
##
## Reads:  v1_output_dir/lista_alimentos/lista_total_alimentos.xlsx
##         proyecto_dir/composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx
## Writes: v1_output_dir/tcac/composicion_270726.xlsx
##         v1_output_dir/tcac/composicion_270726.rds
########################################################

library(tidyverse)
library(openxlsx)
library(stringi)
library(janitor)

# ============================================================
# Rutas
# ============================================================

proyecto_dir  <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
interno_dir   <- file.path(proyecto_dir, "interno")
v1_output_dir <- file.path(interno_dir, "v1/output")

ruta_lista  <- file.path(v1_output_dir, "lista_alimentos")
ruta_tcac   <- file.path(proyecto_dir, "composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx")
ruta_output <- file.path(v1_output_dir, "tcac")

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
      sipsa_name == "Ajo importado"                    ~ "Ajo",
      sipsa_name == "Almejas con concha"                ~ "Almejas",
      sipsa_name == "Bagre rayado en postas congelado"  ~ "Bagre rayado",
      sipsa_name == "Carne de cerdo, lomo sin hueso"    ~ "Carne de cerdo, lomo",
      sipsa_name == "Carne de cerdo, pernil sin hueso"  ~ "Carne de cerdo, lomo",
      sipsa_name == "Trucha en corte mariposa"          ~ "Trucha",
      sipsa_name == "Uva roja"                          ~ "Uva comun",
      sipsa_name == "Yuca ICA"                          ~ "Yuca",

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
  relocate(sipsa_name)

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
