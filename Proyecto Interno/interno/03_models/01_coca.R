########################################################
## SCRIPT 03_models/01_coca.R
## Cost of Caloric Adequacy (CoCA)
## Loop over cities x dates for the 13-city trimester panel.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         household_dir/household_eer.rds
##         aux_dir/CoCA_paper.R
##
## Writes: coca_dir/coca_results.rds
##         coca_dir/coca_results.xlsx
########################################################

library(tidyverse)
library(readxl)
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

output_dir    <- file.path(base_dir, "interno/output")
aux_dir       <- file.path(base_dir, "interno/03_models/aux-functions")
household_dir <- file.path(base_dir, "interno/02_dataprep/household eer")
coca_dir      <- file.path(base_dir, "interno/03_models/coca")

dir.create(coca_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(aux_dir, "CoCA_paper.R"))

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------

message("Loading inputs...")

data_paper <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  filter(grupos_gabas == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS") %>%
  # household_eer usa ciudad en mayúscula sin tilde (ej. "BOGOTA");
  # se agrega una columna normalizada solo para el cruce, sin tocar
  # la columna "ciudad" original que usa el resto del panel.
  mutate(ciudad_norm = toupper(iconv(ciudad, from = "", to = "ASCII//TRANSLIT")))

household_eer <- readRDS(file.path(household_dir, "household_eer.rds"))

message(sprintf("  data_paper: %d rows | %d cities | %d dates",
                nrow(data_paper),
                n_distinct(data_paper$ciudad),
                n_distinct(data_paper$fecha)))

# -----------------------------------------------------------------------
# Loop: CoCA for each city x date
# household_eer already has Age, Sex, Energy columns (from 03_eer_13cities.R)
# -----------------------------------------------------------------------

dominios <- sort(unique(household_eer$ciudad))
fechas   <- sort(unique(data_paper$fecha))

message(sprintf("Estimating CoCA: %d cities x %d dates...",
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
      filter(ciudad_norm == i, fecha == t, !is.na(precio_100g)) %>%
      filter(articulo != "ARROZ PARA SOPA") %>%
      rename(Price_100g = precio_100g,
             Food       = articulo,
             Energy     = energia_kcal) %>%
      as.data.frame()
    
    if (nrow(data.aux) == 0) next
    
    result <- tryCatch(
      CoCA_paper(data = data.aux, EER = eer.aux),
      error = function(e) {
        warning("Error - ", i, " | ", t, " | ", conditionMessage(e))
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

saveRDS(df.coca, file.path(coca_dir, "coca_results.rds"))
write_xlsx(df.coca, file.path(coca_dir, "coca_results.xlsx"))

message("Done. Run 02_cona.R next.")