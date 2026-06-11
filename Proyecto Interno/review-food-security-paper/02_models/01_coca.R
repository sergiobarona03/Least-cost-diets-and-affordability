########################################################
## SCRIPT 02_models/01_coca.R
## Cost of Caloric Adequacy (CoCA)
## Loop over cities × dates for paper period
##
## Reads:  PREP_DIR/panel_food_paper.rds
##         PREP_DIR/household_eer.rds
##         IN_AUX_DIR/CoCA_paper.R
##
## Writes: COCA_DIR/coca_results.rds
##         COCA_DIR/coca_results.xlsx
########################################################

source("00_config.R")
library(tidyverse)
library(readxl)
library(writexl)

source(file.path(IN_AUX_DIR, "CoCA_paper.R"))

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

data_paper <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds")) %>%
  filter(grupos_gabas == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS")

household_eer <- readRDS(file.path(PREP_DIR, "household_eer.rds"))

message(sprintf("  data_paper: %d rows | %d cities | %d dates",
                nrow(data_paper),
                n_distinct(data_paper$ciudad),
                n_distinct(data_paper$fecha)))

# -----------------------------------------------------------------------
# Loop: CoCA for each city × date
# household_eer already has Age, Sex, Energy columns (from 05_eer.R)
# -----------------------------------------------------------------------
dominios <- sort(unique(household_eer$ciudad))
fechas   <- sort(unique(data_paper$fecha))

message(sprintf("Estimating CoCA: %d cities × %d dates...",
                length(dominios), length(fechas)))

resultados <- list()

for (i in dominios) {
  
  eer.aux <- household_eer %>%
    filter(ciudad == i) %>%
    select(-ciudad) %>%
    rename(Age = rango, Sex = sex, Energy = eer) %>%
    mutate(Sex = if_else(Sex == "Masculino", 0L, 1L)) %>%
    as.data.frame()
  
  for (t in fechas) {
    
    data.aux <- data_paper %>%
      filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
      filter(articulo != "ARROZ PARA SOPA") %>%
      rename(Price_100g = precio_100g,
             Food       = articulo,
             Energy     = energia_kcal) %>%
      as.data.frame()
    
    if (nrow(data.aux) == 0) next
    
    result <- tryCatch(
      CoCA_paper(data = data.aux, EER = eer.aux),
      error = function(e) {
        warning("Error — ", i, " | ", t, " | ", conditionMessage(e))
        NULL
      })
    
    if (!is.null(result) && !is.null(result$cost)) {
      resultados[[length(resultados) + 1]] <- result$cost %>%
        mutate(ciudad = i, fecha = t)
    }
  }
}

df.coca <- bind_rows(resultados) %>%
  mutate(fecha = as.Date(fecha),
         year  = year(fecha),
         mes   = month(fecha))

message(sprintf("  Done. %d rows | %d city-month cells",
                nrow(df.coca),
                n_distinct(paste(df.coca$ciudad, df.coca$fecha))))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
saveRDS(df.coca, file.path(COCA_DIR, "coca_results.rds"))
write_xlsx(df.coca, file.path(COCA_DIR, "coca_results.xlsx"))

message("Done. Run 02_cona.R next.")