########################################################
## SCRIPT 05_sensibilidad_umbral/02_correr_regla.R
## Corre panel mensual y los cuatro modelos con los alimentos que
## pasan una regla de cobertura de 01_cobertura_alimentos.R.
##
## Uso: Rscript 02_correr_regla.R <regla>
##   regla: actual, base, fruta75, fruta80, semanal o quincenal
##
## Usa los scripts de 02_dataprep y 03_models tal como están, solo
## cambia en memoria de dónde leen el panel y la composición y
## dónde escriben, así que no toca los resultados del proyecto.
##
## Reads:  sens_dir/cobertura.rds
##         panel_dir/panel_v2.rds
## Writes: sens_dir/<regla>/ (resultados de cada modelo)
########################################################

regla_sens <- commandArgs(trailingOnly = TRUE)[1]
reglas_sens <- c("actual", "base", "fruta75", "fruta80", "semanal", "quincenal")
if (is.na(regla_sens) || !regla_sens %in% reglas_sens) {
  stop("regla debe ser una de: ", paste(reglas_sens, collapse = ", "))
}

library(tidyverse)
library(openxlsx)
library(stringi)
library(janitor)

dir_proyecto <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
dir_interno  <- file.path(dir_proyecto, "interno/")

dir_panel <- file.path(dir_interno, "output/paneles/raw_mensual")
dir_sens  <- file.path(dir_interno, "output/sensibilidad_umbral")
ruta_tcac_mapeo <- file.path(dir_proyecto, "composicion-nut/Mapeo Sipsa TCAC _28.07.26.xlsx")

source(file.path(dir_interno, "01_webscrap_prep/aux-functions/mapeo_tcac.R"), encoding = "UTF-8")

# ============================================================
# Panel y composición de los alimentos que pasan la regla
# ============================================================

dir_salida <- file.path(dir_sens, regla_sens)
dir.create(dir_salida, showWarnings = FALSE)

alimentos_regla <- readRDS(file.path(dir_sens, "cobertura.rds"))$cobertura %>%
  filter(.data[[paste0("pasa_", regla_sens)]]) %>%
  select(city, sipsa_name)

ruta_panel_bal   <- file.path(tempdir(), "panel_balanceado.rds")
ruta_composicion <- file.path(tempdir(), "composicion.rds")
ruta_panel_mens  <- file.path(tempdir(), "panel_mensual.rds")

readRDS(file.path(dir_panel, "panel_v2.rds")) %>%
  semi_join(alimentos_regla, by = c("city", "sipsa_name")) %>%
  saveRDS(ruta_panel_bal)

unir_mapeo_tcac(
  alimentos_regla %>% distinct(sipsa_name) %>% arrange(sipsa_name),
  leer_mapeo_tcac(ruta_tcac_mapeo)
) %>%
  clean_names() %>%
  saveRDS(ruta_composicion)

message(sprintf("%s: %d pares alimento-ciudad, %d alimentos distintos",
                regla_sens, nrow(alimentos_regla), n_distinct(alimentos_regla$sipsa_name)))

# ============================================================
# Scripts del proyecto con las rutas cambiadas
# Una clave que empieza con ^ reemplaza la línea completa; las
# demás, solo el texto que coincide.
# ============================================================

correr <- function(script, cambios) {
  lineas <- readLines(file.path(dir_interno, script), encoding = "UTF-8", warn = FALSE)
  for (clave in names(cambios)) {
    stopifnot(any(grepl(clave, lineas)))
    if (startsWith(clave, "^")) {
      lineas <- sub(paste0(clave, ".*$"), cambios[[clave]], lineas)
    } else {
      lineas <- sub(clave, cambios[[clave]], lineas)
    }
  }
  message("\n== ", script, " | ", regla_sens)
  eval(parse(text = lineas), envir = new.env(parent = globalenv()))
}

en_tcac  <- 'file.path\\(output_dir, "tcac/composicion_270726.rds"\\)'
en_panel <- 'file.path\\(output_dir, "paneles/panel_mensual_cities_tcac.rds"\\)'

correr("02_dataprep/01_panel_mensual_tcac.R", setNames(
  list("ruta_panel_v2 <- ruta_panel_bal", "ruta_composicion", "ruta_panel_mens"),
  c("^ruta_panel_v2 <- ", en_tcac, en_panel)))
correr("03_models/01_coca.R", setNames(
  list("ruta_panel_mens", "coca_dir <- dir_salida"), c(en_panel, "^coca_dir\\s*<-")))
correr("03_models/02_cona.R", setNames(
  list("ruta_panel_mens", "cona_dir <- dir_salida"), c(en_panel, "^cona_dir\\s*<-")))
correr("03_models/03_cord.R", setNames(
  list("ruta_panel_mens", "ruta_composicion", "cord_dir <- dir_salida"), c(en_panel, en_tcac, "^cord_dir\\s*<-")))
correr("03_models/031_coahd.R", setNames(
  list("ruta_panel_mens", "ruta_composicion", "coahd_dir <- dir_salida"), c(en_panel, en_tcac, "^coahd_dir\\s*<-")))
