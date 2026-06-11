########################################################
## SCRIPT 02_models/02_cona.R
## Cost of Nutritional Adequacy (CoNA)
## Loop over cities × dates for paper period
##
## Reads:  PREP_DIR/panel_food_paper.rds
##         PREP_DIR/household_eer_ll.rds
##         PREP_DIR/household_ul.rds
##         IN_AUX_DIR/CoNA_paper.R
##
## Writes: CONA_DIR/cona_results.rds
##         CONA_DIR/cona_results.xlsx
########################################################

source("00_config.R")
library(tidyverse)
library(writexl)
library(FoodpriceR)

source(file.path(IN_AUX_DIR, "CoNA_paper.R"))

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

data_paper    <- readRDS(file.path(PREP_DIR, "panel_food_paper.rds"))
household_eer_ll <- readRDS(file.path(PREP_DIR, "household_eer_ll.rds"))
household_ul     <- readRDS(file.path(PREP_DIR, "household_ul.rds"))

message(sprintf("  data_paper: %d rows | %d cities | %d dates",
                nrow(data_paper),
                n_distinct(data_paper$ciudad),
                n_distinct(data_paper$fecha)))

# -----------------------------------------------------------------------
# Loop: CoNA for each city × date
# -----------------------------------------------------------------------
dominios <- sort(unique(household_eer_ll$ciudad))
fechas   <- sort(unique(data_paper$fecha))

message(sprintf("Estimating CoNA: %d cities × %d dates...",
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
      filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
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
        warning("Error — ", i, " | ", t, " | ", conditionMessage(e))
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
        file.path(CONA_DIR, "cona_results.rds"))

write_xlsx(list(cost  = df.cost,
                comp  = df.comp,
                limit = df.limit,
                spe   = df.spe),
           file.path(CONA_DIR, "cona_results.xlsx"))

message("Done. Run 03_cord.R next.")

