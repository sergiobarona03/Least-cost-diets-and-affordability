########################################################
## SCRIPT 02_models/05_hcost.R
## Household diet cost — representative household
## Aggregates CoCA, CoNA, CoRD member costs to:
##   - total household cost per day
##   - per capita cost per day / month / year
##
## Representative household:
##   Adult male   31–51
##   Adult female 31–51
##   Female child 10–14
##
## Reads:  COCA_DIR/coca_results.rds
##         CONA_DIR/cona_results.rds
##         CORD_DIR/cord_results.rds
##
## Writes: HCOST_DIR/hcost_full.rds
##         HCOST_DIR/hcost_full.xlsx
########################################################

source("00_config.R")
library(tidyverse)
library(lubridate)
library(writexl)

# -----------------------------------------------------------------------
# Load model results
# -----------------------------------------------------------------------
message("Loading model results...")

df.coca <- readRDS(file.path(COCA_DIR, "coca_results.rds")) %>%
  mutate(fecha = as.Date(fecha))

# cona_results.rds is now a list with cost, comp, limit, spe
cona_full <- readRDS(file.path(CONA_DIR, "cona_results.rds"))
df.cona <- cona_full$cost %>%
  mutate(fecha = as.Date(fecha)) %>%
  dplyr::rename(cost_day = cona_cost)

df.cord <- readRDS(file.path(CORD_DIR, "cord_results.rds"))$cost %>%
  mutate(fecha = as.Date(fecha))

message(sprintf(
  "  CoCA: %d rows | CoNA: %d rows | CoRD: %d rows",
  nrow(df.coca), nrow(df.cona), nrow(df.cord)))

# -----------------------------------------------------------------------
# Helper: compute household cost metrics for one city × date × model
# -----------------------------------------------------------------------
household_metrics <- function(df, dominio, fecha_sel, model_name) {
  
  members <- df %>%
    filter(ciudad == dominio, fecha == fecha_sel)
  
  if (nrow(members) == 0) return(NULL)
  
  n_members <- nrow(members)
  
  members %>%
    mutate(
      model            = model_name,
      n_members        = n_members,
      total_household  = sum(as.numeric(cost_day), na.rm = TRUE),
      per_capita       = total_household / n_members,
      per_capita_month = per_capita * 30,
      per_capita_year  = per_capita * 365
    ) %>%
    select(model, ciudad, fecha,
           Demo_Group, Sex,
           cost_day,
           n_members,
           total_household,
           per_capita,
           per_capita_month,
           per_capita_year)
}

# -----------------------------------------------------------------------
# Full panel: all cities × dates
# -----------------------------------------------------------------------
dominios <- sort(unique(df.coca$ciudad))
fechas   <- sort(unique(df.coca$fecha))

message(sprintf("Building hcost: %d cities × %d dates...",
                length(dominios), length(fechas)))

hcost_full <- map_dfr(dominios, function(i) {
  map_dfr(fechas, function(t) {
    
    coca_i <- tryCatch(
      household_metrics(df.coca, i, t, "CoCA"),
      error = function(e) NULL)
    
    cona_i <- tryCatch(
      household_metrics(df.cona, i, t, "CoNA"),
      error = function(e) NULL)
    
    cord_i <- tryCatch(
      household_metrics(df.cord, i, t, "CoRD"),
      error = function(e) NULL)
    
    bind_rows(coca_i, cona_i, cord_i)
  })
})

hcost_full <- hcost_full %>%
  mutate(year = year(fecha), mes = month(fecha)) %>%
  select(model, ciudad, fecha, year, mes,
         Demo_Group, Sex,
         cost_day,
         n_members,
         total_household,
         per_capita,
         per_capita_month,
         per_capita_year) %>%
  arrange(model, ciudad, fecha, Demo_Group, Sex)

message(sprintf("  Done. %d rows | models: %s",
                nrow(hcost_full),
                paste(unique(hcost_full$model), collapse = ", ")))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
saveRDS(hcost_full, file.path(HCOST_DIR, "hcost_full.rds"))

write_xlsx(
  list(
    full = hcost_full,
    CoCA = hcost_full %>% filter(model == "CoCA"),
    CoNA = hcost_full %>% filter(model == "CoNA"),
    CoRD = hcost_full %>% filter(model == "CoRD")
  ),
  file.path(HCOST_DIR, "hcost_full.xlsx"))

message("Done. Proceed to 03_income/ scripts.")