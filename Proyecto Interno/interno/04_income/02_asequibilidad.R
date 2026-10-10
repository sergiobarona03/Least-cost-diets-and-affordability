########################################################
## SCRIPT 04_income/02_asequibilidad.R
## Asequibilidad de las cinco dietas por decil de ingreso, ciudad y mes
## del trimestre jul-sep 2025, con tres ejercicios:
##   base:          el costo per capita del hogar de tres personas se
##                  compara con el gasto en alimentos per capita
##   por_tamano:    el costo per capita depende del tamano del hogar (3, 4
##                  o 5 personas, hogares de 02_eer_*members.R)
##   contrafactual: el hogar dedica todo su ingreso a alimentos
## Cada ejercicio se calcula por decil de ingreso y, para los dos primeros,
## tambien segun el tamano del hogar (3 o menos, 4, 5 o mas).
##
## Reads:  income_dir/deciles_2025.rds
##         hcost_dir/hcost_full.rds (CoCA, CoNA, CC-CoNA, CoRD y CoAHD; 3, 4 y 5 miembros)
##         aux_dir/Afford_Exp.R
##
## Writes: afford_dir/asequibilidad.rds
##         afford_dir/asequibilidad.xlsx
########################################################

library(tidyverse)
library(lubridate)
library(writexl)

# ============================================================
# Rutas
# ============================================================

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

income_dir  <- file.path(base_dir, "interno/04_income")
aux_dir     <- file.path(income_dir, "aux-functions")
afford_dir  <- file.path(income_dir, "asequibilidad")
hcost_dir   <- file.path(base_dir, "interno/03_models/hcost")

dir.create(afford_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(aux_dir, "Afford_Exp.R"))

MODELOS <- c("CoCA", "CoNA", "CC-CoNA", "CoRD", "CoAHD")

# -----------------------------------------------------------------------
# Insumos
# -----------------------------------------------------------------------

message("Loading inputs...")

ingreso <- readRDS(file.path(income_dir, "deciles_2025.rds")) %>%
  transmute(ciudad = dominio, fecha = as.Date(fecha), year, mes,
            deciles = paste0("Decil ", deciles),
            ung = nug, income, per_capita_income,
            food_exp_per_capita = food_exp_pc_month,
            fex_c18 = fex_c)

hcost_todos <- readRDS(file.path(hcost_dir, "hcost_full.rds")) %>%
  mutate(fecha = as.Date(fecha), model = as.character(model))

# Costo per capita del hogar de tres personas
hcost <- hcost_todos %>%
  filter(hogar_miembros == 3) %>%
  distinct(model, ciudad, fecha, per_capita)

# Costo per capita segun el tamano del hogar (3, 4 o 5 personas)
costo_tamano <- hcost_todos %>%
  distinct(model, ciudad, fecha, tamano = hogar_miembros, per_capita)

# -----------------------------------------------------------------------
# Loop: ciudad x mes
# -----------------------------------------------------------------------

combinaciones <- ingreso %>% distinct(ciudad, fecha)

indicadores <- list(); medias <- list()

for (k in seq_len(nrow(combinaciones))) {
  i <- combinaciones$ciudad[k]
  t <- combinaciones$fecha[k]

  h_aux <- ingreso %>% filter(ciudad == i, fecha == t)
  u_base <- hcost %>% filter(ciudad == i, fecha == t, model %in% MODELOS)
  u_tam  <- costo_tamano %>% filter(ciudad == i, fecha == t, model %in% MODELOS)

  if (n_distinct(u_base$model) < length(MODELOS) || n_distinct(u_tam$model) < length(MODELOS)) {
    warning("Sin costos de las cinco dietas: ", i, " | ", t)
    next
  }

  # caso: umbral y gasto; agrupar: por decil o por tamano del hogar
  casos <- list(
    base                  = list(Afford_Exp(h_aux, u_base, "alimentos", "deciles"), "deciles"),
    por_tamano            = list(Afford_Exp(h_aux, u_tam,  "alimentos", "deciles"), "deciles"),
    contrafactual         = list(Afford_Exp(h_aux, u_base, "ingreso",   "deciles"), "deciles"),
    base_segun_hogar      = list(Afford_Exp(h_aux, u_base, "alimentos", "tamano"), "tamano"),
    por_tamano_segun_hogar = list(Afford_Exp(h_aux, u_tam,  "alimentos", "tamano"), "tamano")
  )

  for (nombre in names(casos)) {
    indicadores[[length(indicadores) + 1]] <- casos[[nombre]][[1]]$indicadores %>%
      mutate(caso = nombre, agrupar = casos[[nombre]][[2]], ciudad = i, fecha = t)
  }
  medias[[length(medias) + 1]] <- casos$base[[1]]$medias %>% mutate(ciudad = i, fecha = t)
}

indicadores <- bind_rows(indicadores) %>%
  mutate(model = factor(model, levels = MODELOS),
         year = year(fecha), mes = month(fecha)) %>%
  arrange(caso, ciudad, fecha, model, grupo)

medias <- bind_rows(medias)

# -----------------------------------------------------------------------
# Participacion de la poblacion por tamano del hogar
# -----------------------------------------------------------------------

tamanos <- ingreso %>%
  mutate(tamano = pmin(pmax(as.numeric(ung), 3), 5)) %>%
  group_by(ciudad, fecha, tamano) %>%
  summarise(personas = sum(fex_c18), .groups = "drop") %>%
  group_by(ciudad, fecha) %>%
  mutate(participacion = personas / sum(personas)) %>%
  ungroup()

message(sprintf("  %d filas de indicadores | %d ciudades | %d meses",
                nrow(indicadores), n_distinct(indicadores$ciudad), n_distinct(indicadores$fecha)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

salida <- list(indicadores = indicadores, medias = medias, tamanos = tamanos,
               costos_base = hcost, costos_tamano = costo_tamano)
saveRDS(salida, file.path(afford_dir, "asequibilidad.rds"))
write_xlsx(list(indicadores = indicadores, medias = medias, tamanos = tamanos),
           file.path(afford_dir, "asequibilidad.xlsx"))

message("Done.")
