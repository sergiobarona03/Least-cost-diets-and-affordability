########################################################
## SCRIPT 06_sensibilidad_umbral/04_sin_umbral_tamano.R
## Corre el proyecto completo, desde el panel de precios hasta las
## cinco dietas, sin el umbral de tamano (500 g en los solidos y
## 1.000 ml en los liquidos) en la seleccion del sku representativo
## (umbral_gramos = NULL y umbral_ml = NULL).
##
## Usa los scripts de 01_webscrap_prep, 02_dataprep y 03_models tal
## como están; solo cambia en memoria el parametro y las rutas de
## salida, así que no toca los resultados del proyecto.
##
## Uso: Rscript 04_sin_umbral_tamano.R
##
## Reads:  panel_dir/data/*.rds (precios crudos)
##         output/gabas (intercambios de la guia, se copian)
## Writes: sens_dir/sin_umbral_tamano/ (resultados de cada modelo)
##         sens_dir/sin_umbral_tamano/insumos/ (paneles y composicion)
########################################################

dir_proyecto <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
dir_interno  <- file.path(dir_proyecto, "interno/")

source(file.path(dir_interno, "06_sensibilidad_umbral/aux-functions/correr_con_cambios.R"))

dir_salida  <- file.path(dir_interno, "output/sensibilidad_umbral/sin_umbral_tamano")
dir_insumos <- file.path(dir_salida, "insumos")

dir.create(dir_insumos, recursive = TRUE, showWarnings = FALSE)
file.copy(file.path(dir_interno, "output/gabas"), dir_insumos, recursive = TRUE)
file.copy(file.path(dir_interno, "output/ipc"), dir_insumos, recursive = TRUE)

en_insumos <- list("^output_dir\\s*<-" = "output_dir <- dir_insumos")
etiqueta   <- "sin_umbral_tamano"

correr_con_cambios("01_webscrap_prep/01_construccion_panel.R",
  c(en_insumos, list("^umbral_gramos\\s*<-" = "umbral_gramos <- NULL",
                     "^umbral_ml\\s*<-"     = "umbral_ml <- NULL")), etiqueta)
correr_con_cambios("01_webscrap_prep/02a_lista_alimentos.R", en_insumos, etiqueta)
correr_con_cambios("01_webscrap_prep/02b_panel_balanceado.R", en_insumos, etiqueta)
correr_con_cambios("02_dataprep/00_tcac_composicion.R", en_insumos, etiqueta)
correr_con_cambios("02_dataprep/01_panel_mensual_tcac.R", en_insumos, etiqueta)

for (m in c("coca", "cona", "ccona", "cord", "coahd")) {
  script <- c(coca = "01_coca", cona = "02_cona", ccona = "04_cc_cona", cord = "03_cord", coahd = "031_coahd")[[m]]
  cambios <- c(en_insumos, setNames(list(paste0(m, "_dir <- dir_salida")), paste0("^", m, "_dir\\s*<-")))
  correr_con_cambios(paste0("03_models/", script, ".R"), cambios, etiqueta)
}
