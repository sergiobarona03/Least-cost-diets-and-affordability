########################################################
## SCRIPT 03_income/02_affordability.R
## Calcula indicadores de asequibilidad mensual
## Combina costos de dieta (hcost) con ingresos GEIH
##
## Reads:  INCOME_DIR/deciles_final.rds
##         HCOST_DIR/hcost_full.rds
##         IN_AFFORD_AUX (Afford_Expansion.R)
##
## Writes: AFFORD_DIR/afford_poverty.rds
##         AFFORD_DIR/afford_poverty.csv
##         AFFORD_DIR/afford_food.rds
##         AFFORD_DIR/afford_food.csv
##         AFFORD_DIR/afford_results.xlsx
########################################################

source("00_config.R")
library(tidyverse)
library(lubridate)
library(janitor)
library(Hmisc)
library(writexl)

source(IN_AFFORD_AUX)

# -----------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------
message("Loading inputs...")

deciles_final <- readRDS(file.path(INCOME_DIR, "deciles_final.rds"))
hcost_full    <- readRDS(file.path(HCOST_DIR,  "hcost_full.rds"))

message(sprintf(
  "  deciles_final: %d rows | hcost_full: %d rows",
  nrow(deciles_final), nrow(hcost_full)))

# -----------------------------------------------------------------------
# Prepare income data for Afford_Exp()
# Recode city names to match hcost_full (MEDELLÍN, BOGOTÁ D.C., CALI)
# -----------------------------------------------------------------------
income_df <- deciles_final %>%
  mutate(
    fecha  = ymd(paste(year, mes, "01", sep = "-")),
    ciudad = case_when(
      dominio == "MEDELLIN" ~ "MEDELLÍN",
      dominio == "BOGOTA"   ~ "BOGOTÁ D.C.",
      TRUE                  ~ dominio),
    # Afford_Exp() expected column names
    ung                      = nug,
    deciles                  = paste0("Decil ", deciles),
    food_exp_per_capita      = food_exp_pc_month,
    food_exp_per_capita_year = food_exp_pc_year,
    food_income              = food_exp,
    fex_c18                  = fex_c
  ) %>%
  select(ciudad, dominio, year, mes, fecha,
         id_hogar, ung, income, per_capita_income,
         deciles, share,
         food_income,
         food_exp_per_capita,
         food_exp_per_capita_year,
         fex_c18)

# -----------------------------------------------------------------------
# Prepare diet cost data
# Recode city names to match income_df
# -----------------------------------------------------------------------
recode_ciudad <- function(df) {
  df %>% mutate(ciudad = case_when(
    ciudad == "MEDELLIN" ~ "MEDELLÍN",
    ciudad == "BOGOTA"   ~ "BOGOTÁ D.C.",
    TRUE                 ~ ciudad))
}

coca_df <- hcost_full %>%
  filter(model == "CoCA") %>%
  recode_ciudad() %>%
  mutate(year = year(fecha), mes = month(fecha))

cona_df <- hcost_full %>%
  filter(model == "CoNA") %>%
  recode_ciudad() %>%
  mutate(year = year(fecha), mes = month(fecha))

cord_df <- hcost_full %>%
  filter(model == "CoRD") %>%
  recode_ciudad() %>%
  mutate(year = year(fecha), mes = month(fecha))

# -----------------------------------------------------------------------
# Loop: Afford_Exp() for each city × month
# -----------------------------------------------------------------------
city_vector <- sort(unique(income_df$ciudad))
date_vector <- income_df %>%
  distinct(year, mes) %>%
  arrange(year, mes)

message(sprintf(
  "Running Afford: %d cities × %d months...",
  length(city_vector), nrow(date_vector)))

res_afford <- list()
k <- 1

for (city.x in city_vector) {
  
  
  for (row.x in seq_len(nrow(date_vector))) {
    
    year.x <- date_vector$year[row.x]
    mes.x  <- date_vector$mes[row.x]
    
    # Filter income
    inc_aux <- income_df %>%
      filter(ciudad == city.x,
             year   == year.x,
             mes    == mes.x)
    if (nrow(inc_aux) == 0) next
    
    # Filter diet costs
    coca_aux <- coca_df %>%
      filter(ciudad == city.x, year == year.x, mes == as.numeric(mes.x))
    cona_aux <- cona_df %>%
      filter(ciudad == city.x, year == year.x, mes == as.numeric(mes.x))
    cord_aux <- cord_df %>%
      filter(ciudad == city.x, year == year.x, mes == as.numeric(mes.x))
    
    if (nrow(coca_aux) == 0 |
        nrow(cona_aux) == 0 |
        nrow(cord_aux) == 0) next
    
    out <- tryCatch(
      Afford_Exp(
        Hexpense   = inc_aux,
        Model_CoCA = coca_aux,
        Model_CoNA = cona_aux,
        Model_CoRD = cord_aux),
      error = function(e) {
        warning("Error — ", city.x, " | ",
                year.x, "-", sprintf("%02d", mes.x),
                " | ", conditionMessage(e))
        NULL
      })
    
    if (is.null(out)) next
    
    fecha.x <- ymd(paste(year.x, mes.x, "01", sep = "-"))
    
    po <- out$Poverty_outcome %>%
      mutate(ciudad = city.x, fecha = fecha.x,
             year = year.x, mes = mes.x)
    
    mf <- out$Mean_income_food %>%
      mutate(ciudad = city.x, fecha = fecha.x,
             year = year.x, mes = mes.x)
    
    res_afford[[k]] <- list(poverty = po, mean_food = mf)
    k <- k + 1
  }
  message(sprintf("  %s: done", city.x))
}

# -----------------------------------------------------------------------
# Consolidate
# -----------------------------------------------------------------------
afford_poverty <- bind_rows(lapply(res_afford, `[[`, "poverty")) %>%
  mutate(fecha = as.Date(fecha)) %>%
  arrange(ciudad, fecha, model, deciles)

afford_food <- bind_rows(lapply(res_afford, `[[`, "mean_food")) %>%
  mutate(fecha = as.Date(fecha)) %>%
  arrange(ciudad, fecha)

message(sprintf(
  "  afford_poverty: %d rows | afford_food: %d rows",
  nrow(afford_poverty), nrow(afford_food)))

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------
saveRDS(afford_poverty,
        file.path(AFFORD_DIR, "afford_poverty.rds"))
write_csv(afford_poverty,
          file.path(AFFORD_DIR, "afford_poverty.csv"))

saveRDS(afford_food,
        file.path(AFFORD_DIR, "afford_food.rds"))
write_csv(afford_food,
          file.path(AFFORD_DIR, "afford_food.csv"))

write_xlsx(
  list(poverty   = afford_poverty,
       mean_food = afford_food),
  file.path(AFFORD_DIR, "afford_results.xlsx"))

message("Done. All 03_income scripts complete.")
message("Proceed to 04_validation/ scripts.")