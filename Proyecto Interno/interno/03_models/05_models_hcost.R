########################################################
## SCRIPT 03_models/05_models_hcost.R
## Costo del hogar representativo (hombre adulto 31-50, mujer
## adulta 31-50 y niña 9-13) para CoCA, CoNA, CoRD y CoAHD:
##   - costo total del hogar por día
##   - costo per cápita por día, mes (30 días) y año (365 días)
##
## Reads:  coca_dir/coca_results.rds
##         cona_dir/cona_results.rds
##         cord_dir/cord_results.rds
##         coahd_dir/coahd_results.rds
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
cord_dir  <- file.path(base_dir, "interno/03_models/cord")
coahd_dir <- file.path(base_dir, "interno/03_models/coahd")
hcost_dir <- file.path(base_dir, "interno/03_models/hcost")

dir.create(hcost_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------
# Load model results: un costo diario por miembro, ciudad y mes
# -----------------------------------------------------------------------

message("Loading model results...")

costos <- bind_rows(
  CoCA = readRDS(file.path(coca_dir, "coca_results.rds")) %>%
    distinct(ciudad, fecha, Demo_Group, Sex, cost_day),
  CoNA = readRDS(file.path(cona_dir, "cona_results.rds"))$cost %>%
    select(ciudad, fecha, Demo_Group, Sex, cost_day = cona_cost),
  CoRD = readRDS(file.path(cord_dir, "cord_results.rds"))$cost %>%
    select(ciudad, fecha, Demo_Group, Sex, cost_day),
  CoAHD = readRDS(file.path(coahd_dir, "coahd_results.rds"))$cost %>%
    select(ciudad, fecha, Demo_Group, Sex, cost_day),
  .id = "model"
) %>%
  mutate(fecha = as.Date(fecha))

message(sprintf("  %s",
                paste(names(table(costos$model)), table(costos$model),
                      sep = ": ", collapse = " | ")))

# -----------------------------------------------------------------------
# Costo del hogar por modelo, ciudad y fecha
# -----------------------------------------------------------------------

hcost_full <- costos %>%
  group_by(model, ciudad, fecha) %>%
  mutate(
    n_members        = n(),
    total_household  = sum(cost_day),
    per_capita       = total_household / n_members,
    per_capita_month = per_capita * 30,
    per_capita_year  = per_capita * 365
  ) %>%
  ungroup() %>%
  mutate(year = year(fecha), mes = month(fecha),
         model = factor(model, levels = c("CoCA", "CoNA", "CoRD", "CoAHD"))) %>%
  select(model, ciudad, fecha, year, mes, Demo_Group, Sex, cost_day,
         n_members, total_household, per_capita, per_capita_month,
         per_capita_year) %>%
  arrange(model, ciudad, fecha, Demo_Group, Sex)

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
