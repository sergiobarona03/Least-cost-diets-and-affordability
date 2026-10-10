########################################################
## SCRIPT 05_figures/09_tab_asequibilidad.R
## Tabla resumen de asequibilidad por ciudad y modelo, trimestre
## jul-sep 2025: porcentaje de la poblacion que no puede pagar la dieta
## (promedio simple de los diez deciles, de modo que cada decil pesa
## igual) en el ejercicio base, con el costo segun el tamano del hogar
## y en el contrafactual.
##
## Falta el insumo de ingreso (microdatos de pobreza monetaria 2025 en 04_income/input).
##
## Reads:  afford_dir/asequibilidad.rds
## Writes: afford_dir/tabla_asequibilidad.xlsx
##         afford_dir/tabla_asequibilidad.rds
########################################################

source("C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/interno/05_figures/00_fig_config.R")
library(tidyverse)
library(writexl)

indicadores <- readRDS(file.path(afford_dir, "asequibilidad.rds"))$indicadores %>%
  filter(agrupar == "deciles") %>%
  recode_city(col = "ciudad")

# Promedio de los diez deciles en cada mes, y luego de los tres meses
tabla <- indicadores %>%
  group_by(caso, ciudad_lbl, model, fecha) %>%
  summarise(rate = mean(rate), gap = mean(gap), .groups = "drop") %>%
  group_by(caso, ciudad_lbl, model) %>%
  summarise(rate = mean(rate), gap = mean(gap), .groups = "drop") %>%
  pivot_wider(names_from = c(caso, model), values_from = c(rate, gap), names_sep = "_")

por_decil <- indicadores %>%
  filter(caso == "base") %>%
  group_by(model, grupo) %>%
  summarise(rate = mean(rate), gap = mean(gap), .groups = "drop")

saveRDS(list(ciudad = tabla, decil = por_decil), file.path(afford_dir, "tabla_asequibilidad.rds"))
write_xlsx(list(ciudad_modelo = tabla, decil_modelo = por_decil),
           file.path(afford_dir, "tabla_asequibilidad.xlsx"))

message("Tabla de asequibilidad guardada en: ", afford_dir)
