########################################################
## SCRIPT 03_models/05_models_hcost.R
## Costo del hogar para las cinco dietas (CoCA, CoNA, CC-CoNA, CoRD y
## CoAHD) en los hogares de 3, 4 y 5 miembros:
##   - costo total del hogar por día
##   - costo per cápita por día, mes (30 días) y año (365 días)
##
## El CC-CoNA entra con el alpha de referencia (aux-functions/alfa_referencia.R).
##
## Reads:  coca_dir/coca_results<sufijo>.rds
##         cona_dir/cona_results<sufijo>.rds
##         ccona_dir/ccona_results<sufijo>.rds
##         cord_dir/cord_results<sufijo>.rds
##         coahd_dir/coahd_results<sufijo>.rds
##         (sufijo = "" para 3 miembros, "_4m" y "_5m")
##
## Writes: hcost_dir/hcost_full.rds
##         hcost_dir/hcost_full.xlsx
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

coca_dir  <- file.path(base_dir, "interno/03_models/coca")
cona_dir  <- file.path(base_dir, "interno/03_models/cona")
ccona_dir <- file.path(base_dir, "interno/03_models/ccona")
cord_dir  <- file.path(base_dir, "interno/03_models/cord")
coahd_dir <- file.path(base_dir, "interno/03_models/coahd")
hcost_dir <- file.path(base_dir, "interno/03_models/hcost")

dir.create(hcost_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(base_dir, "interno/03_models/aux-functions/alfa_referencia.R"))

MODELOS <- c("CoCA", "CoNA", "CC-CoNA", "CoRD", "CoAHD")

# -----------------------------------------------------------------------
# Load model results: un costo diario por miembro, ciudad y mes
# -----------------------------------------------------------------------

message("Loading model results...")

leer_hogar <- function(n_miembros) {
  sufijo <- if (n_miembros == 3L) "" else paste0("_", n_miembros, "m")
  bind_rows(
    CoCA = readRDS(file.path(coca_dir, paste0("coca_results", sufijo, ".rds"))) %>%
      distinct(ciudad, fecha, Demo_Group, Sex, cost_day),
    CoNA = readRDS(file.path(cona_dir, paste0("cona_results", sufijo, ".rds")))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, cost_day = cona_cost),
    "CC-CoNA" = readRDS(file.path(ccona_dir, paste0("ccona_results", sufijo, ".rds")))$cost %>%
      filter(alpha_val == ALFA_REFERENCIA) %>%
      select(ciudad, fecha, Demo_Group, Sex, cost_day),
    CoRD = readRDS(file.path(cord_dir, paste0("cord_results", sufijo, ".rds")))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, cost_day),
    CoAHD = readRDS(file.path(coahd_dir, paste0("coahd_results", sufijo, ".rds")))$cost %>%
      select(ciudad, fecha, Demo_Group, Sex, cost_day),
    .id = "model"
  ) %>%
    mutate(fecha = as.Date(fecha), hogar_miembros = n_miembros)
}

costos <- bind_rows(lapply(3:5, leer_hogar))

# Cada dieta debe tener un costo por miembro, ciudad y mes en cada hogar
esperado <- costos %>% count(hogar_miembros, model) %>% mutate(esperado = hogar_miembros * 13 * 3)
stopifnot(all(esperado$n == esperado$esperado))

message(sprintf("  %d filas | hogares de %s miembros",
                nrow(costos), paste(sort(unique(costos$hogar_miembros)), collapse = ", ")))

# -----------------------------------------------------------------------
# Costo del hogar por hogar, dieta, ciudad y fecha
# -----------------------------------------------------------------------

hcost_full <- costos %>%
  group_by(hogar_miembros, model, ciudad, fecha) %>%
  mutate(
    n_members        = n(),
    total_household  = sum(cost_day),
    per_capita       = total_household / n_members,
    per_capita_month = per_capita * 30,
    per_capita_year  = per_capita * 365
  ) %>%
  ungroup() %>%
  mutate(year = year(fecha), mes = month(fecha),
         model = factor(model, levels = MODELOS)) %>%
  select(hogar_miembros, model, ciudad, fecha, year, mes, Demo_Group, Sex, cost_day,
         n_members, total_household, per_capita, per_capita_month,
         per_capita_year) %>%
  arrange(hogar_miembros, model, ciudad, fecha, Demo_Group, Sex)

message(sprintf("  Done. %d rows | models: %s",
                nrow(hcost_full),
                paste(levels(hcost_full$model), collapse = ", ")))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

saveRDS(hcost_full, file.path(hcost_dir, "hcost_full.rds"))

write_xlsx(
  c(list(full = hcost_full),
    lapply(split(hcost_full, hcost_full$model), as.data.frame)),
  file.path(hcost_dir, "hcost_full.xlsx"))

message("Done.")
