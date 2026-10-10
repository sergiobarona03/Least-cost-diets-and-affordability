########################################################
## SCRIPT 03_models/02_cona.R
## Cost of Nutritional Adequacy (CoNA)
## Loop over cities x dates for the 13-city trimester panel.
##
## Hogar de 3 miembros por defecto; con N_MIEMBROS = 4 o 5 usa el hogar de
## 02_eer_4members.R o 02_eer_5members.R y los archivos llevan el sufijo _4m o _5m.
##
## Reads:  output_dir/paneles/panel_mensual_cities_tcac.rds
##         household_dir/household_eer_ll.rds
##         household_dir/household_ul.rds
##         aux_dir/CoNA_paper.R
##
## Writes: cona_dir/cona_results.rds
##         cona_dir/cona_results.xlsx
########################################################

library(tidyverse)
library(writexl)
library(FoodpriceR)

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
cona_dir      <- file.path(base_dir, "interno/03_models/cona")

dir.create(cona_dir, recursive = TRUE, showWarnings = FALSE)

# Tamano del hogar: 3 (base), 4 o 5 miembros. La variable de entorno N_MIEMBROS lo
# cambia; los archivos de 4 y 5 miembros llevan el sufijo _4m o _5m.
n_miembros   <- as.integer(Sys.getenv("N_MIEMBROS", unset = "3"))
stopifnot(n_miembros %in% 3:5)
# VARIANTE_HOGAR pruebas de sensibilidad de los requerimientos; vacia es la version base:
#   "t3"  hogar de 4 con el incremento de energia del tercer trimestre de la gestacion
#   "esc" minimos y maximos de macronutrientes escalados a la energia de cada miembro
variante_hogar <- Sys.getenv("VARIANTE_HOGAR", unset = "")
stopifnot(variante_hogar %in% c("", "t3", "esc"), !(variante_hogar == "t3" && n_miembros != 4L))
sufijo_hogar <- paste0(if (n_miembros == 3L) "" else paste0("_", n_miembros, "m"),
                       if (nzchar(variante_hogar)) paste0("_", variante_hogar))

source(file.path(aux_dir, "CoNA_paper.R"))

# Helper: normalise EER column names regardless of source version
normalise_eer <- function(df) {
  if ("rango" %in% names(df)) names(df)[names(df) == "rango"] <- "Age"
  if ("sex"   %in% names(df)) names(df)[names(df) == "sex"]   <- "Sex"
  if ("eer"   %in% names(df)) names(df)[names(df) == "eer"]   <- "Energy"
  if ("Sex"   %in% names(df) && is.character(df$Sex))
    df$Sex <- if_else(df$Sex == "Masculino", 0L, 1L)
  df
}

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------

message("Loading inputs...")

data_paper <- readRDS(file.path(output_dir, "paneles/panel_mensual_cities_tcac.rds")) %>%
  # household_eer_ll / household_ul usan ciudad en mayúscula sin tilde
  # (ej. "BOGOTA"); se agrega una columna normalizada solo para el
  # cruce, sin tocar la columna "ciudad" original del panel.
  mutate(ciudad_norm = toupper(iconv(ciudad, from = "", to = "ASCII//TRANSLIT")))

household_eer_ll <- readRDS(file.path(household_dir, paste0("household_eer_ll", sufijo_hogar, ".rds")))
household_ul     <- readRDS(file.path(household_dir, paste0("household_ul", sufijo_hogar, ".rds")))

message(sprintf("  data_paper: %d rows | %d cities | %d dates",
                nrow(data_paper),
                n_distinct(data_paper$ciudad),
                n_distinct(data_paper$fecha)))

# -----------------------------------------------------------------------
# Loop: CoNA for each city x date
# -----------------------------------------------------------------------

dominios <- sort(unique(household_eer_ll$ciudad))
fechas   <- sort(unique(data_paper$fecha))

message(sprintf("Estimating CoNA: %d cities x %d dates...",
                length(dominios), length(fechas)))

out_cost  <- list()
out_comp  <- list()
out_limit <- list()
out_spe   <- list()
n_ok <- 0L; n_fail <- 0L

for (i in dominios) {
  
  eer_ll.aux <- household_eer_ll %>%
    filter(ciudad == i) %>%
    select(-ciudad, -any_of("cod_mun")) %>%
    as.data.frame() %>%
    normalise_eer()
  ul.aux <- household_ul %>%
    filter(ciudad == i) %>%
    select(-ciudad, -any_of("cod_mun")) %>%
    as.data.frame() %>%
    normalise_eer()
  
  for (t in fechas) {
    
    data.aux <- data_paper %>%
      filter(ciudad_norm == i, fecha == t, !is.na(precio_100g)) %>%
      filter(articulo != "ARROZ PARA SOPA") %>%
      dplyr::rename(
        Food          = articulo,
        Price_100g    = precio_100g,
        Energy        = energia_kcal,
        Protein       = proteina_g,
        Lipids        = lipidos_g,
        Carbohydrates = carbohidratos_totales_g,
        VitaminC      = vitamina_c_mg,
        Folate        = folatos_mcg,
        VitaminA      = vitamina_a_er,
        Thiamine      = tiamina_mg,
        Riboflavin    = riboflavina_mg,
        Niacin        = niacina_mg,
        VitaminB12    = vitamina_b12_mcg,
        Magnesium     = magnesio_mg,
        Phosphorus    = fosforo_mg,
        Sodium        = sodio_mg,
        Calcium       = calcio_mg,
        Iron          = hierro_mg,
        Zinc          = zinc_mg
      ) %>%
      as.data.frame()
    
    if (nrow(data.aux) == 0) next
    
    result <- tryCatch(
      CoNA_paper(data   = data.aux,
                 EER_LL = eer_ll.aux,
                 UL     = ul.aux),
      error = function(e) {
        warning("Error - ", i, " | ", t, " | ", conditionMessage(e))
        NULL
      })
    
    if (!is.null(result) && !is.null(result$cost)) {
      out_cost[[length(out_cost)   + 1]] <- result$cost  %>%
        mutate(ciudad = i, fecha = t)
      out_comp[[length(out_comp)   + 1]] <- result$comp  %>%
        mutate(ciudad = i, fecha = t)
      out_limit[[length(out_limit) + 1]] <- result$limit %>%
        mutate(ciudad = i, fecha = t)
      out_spe[[length(out_spe)     + 1]] <- result$spe   %>%
        mutate(ciudad = i, fecha = t)
      n_ok <- n_ok + 1L
    } else {
      n_fail <- n_fail + 1L
    }
  }
}

# Consolidate
df.cost  <- bind_rows(out_cost) %>%
  mutate(fecha = as.Date(fecha),
         year  = year(fecha),
         mes   = month(fecha)) %>%
  dplyr::rename(cona_cost = cost_day)

df.comp  <- bind_rows(out_comp)  %>% mutate(fecha = as.Date(fecha))
df.limit <- bind_rows(out_limit) %>% mutate(fecha = as.Date(fecha))
df.spe   <- bind_rows(out_spe)   %>% mutate(fecha = as.Date(fecha))

message(sprintf("  Done. %d OK | %d failed | cost: %d rows | spe: %d rows",
                n_ok, n_fail, nrow(df.cost), nrow(df.spe)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

saveRDS(list(cost  = df.cost,
             comp  = df.comp,
             limit = df.limit,
             spe   = df.spe),
        file.path(cona_dir, paste0("cona_results", sufijo_hogar, ".rds")))

write_xlsx(list(cost  = df.cost,
                comp  = df.comp,
                limit = df.limit,
                spe   = df.spe),
           file.path(cona_dir, paste0("cona_results", sufijo_hogar, ".xlsx")))

message("Done. Run 03_cord.R next.")
