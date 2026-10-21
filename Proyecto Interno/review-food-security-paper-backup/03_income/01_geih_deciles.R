########################################################
## SCRIPT 03_income/01_geih_deciles.R
## Procesa bases GEIH de pobreza (personas + hogares)
## Calcula deciles de ingreso per cápita ponderados
## por ciudad × mes para el período del paper
##
## Reads:  GEIH_PERSONAS_DIR/personas_pobreza_{k}.rds
##         GEIH_HOGARES_DIR/hogares_pobreza_{k}.rds
##
## Writes: INCOME_DIR/IncomeCol_{k}.rds    ← un archivo por año
##         INCOME_DIR/deciles_final.rds
##         INCOME_DIR/deciles_final.csv
##         INCOME_DIR/poverty_rates.rds
##         INCOME_DIR/poverty_rates.csv
########################################################

source("00_config.R")
library(tidyverse)
library(lubridate)
library(janitor)
library(Hmisc)

# -----------------------------------------------------------------------
# Accumulators
# -----------------------------------------------------------------------
poverty_all <- list()
deciles_all <- list()

# -----------------------------------------------------------------------
# Loop over years
# -----------------------------------------------------------------------
for (k in GEIH_YEARS) {
  
  message(sprintf("Processing year: %d", k))
  
  # --- Load personas ---
  personas.aux <- readRDS(
    file.path(GEIH_PERSONAS_DIR,
              sprintf("personas_pobreza_%d.rds", k))) %>%
    clean_names()
  
  if ("dpto" %in% names(personas.aux) &
      !("depto" %in% names(personas.aux)))
    names(personas.aux)[names(personas.aux) == "dpto"] <- "depto"
  
  # --- Load hogares ---
  hogares.aux <- readRDS(
    file.path(GEIH_HOGARES_DIR,
              sprintf("hogares_pobreza_%d.rds", k))) %>%
    clean_names()
  
  
  if ("dpto" %in% names(hogares.aux) &
      !("depto" %in% names(hogares.aux)))
    names(hogares.aux)[names(hogares.aux) == "dpto"] <- "depto"
  
  if ("ingtotarr" %in% names(hogares.aux) &
      !("ingtotugarr" %in% names(hogares.aux)))
    names(hogares.aux)[names(hogares.aux) == "ingtotarr"] <- "ingtotugarr"
  
  # --- Prepare hogares ---
  hogares.aux2 <- hogares.aux %>%
    mutate(id_hogar = paste0(directorio, "-", secuencia_p),
           nper = as.numeric(nper),
           npersug = as.numeric(npersug)) %>%
    filter(clase == 1) %>%
    select(year, mes, dominio, id_hogar,
           nper, npersug,
           ingtotug, ingtotugarr,
           ingpcug, li, lp,
           pobre, indigente)
  
  # --- Prepare personas ---
  personas.aux2 <- personas.aux %>%
    mutate(
      id_hogar = paste0(directorio, "-", secuencia_p),
      id       = paste0(directorio, "-", secuencia_p, "-", orden)) %>%
    filter(clase == 1) %>%
    select(year, mes, id, id_hogar,
           clase, dominio,
           depto, ingtot, fex_c)
  
  # --- Merge ---
  dataset.k <- merge(personas.aux2, hogares.aux2,
                     by = c("year", "mes", "dominio", "id_hogar")) %>%
    filter(dominio %in% c("CALI", "MEDELLIN", "BOGOTA"))
  
  # --- Numeric conversion ---
  dataset.k <- dataset.k %>%
    mutate(
      fex_c      = as.numeric(gsub(",", ".", fex_c)),
      ingpcug    = as.numeric(gsub(",", ".", ingpcug)),
      ingtotugarr= as.numeric(gsub(",", ".", ingtotugarr)),
      lp         = as.numeric(gsub(",", ".", lp)),
      li         = as.numeric(gsub(",", ".", li)),
      pobre      = as.numeric(pobre),
      indigente  = as.numeric(indigente))
  
  # -----------------------------------------------------------------------
  # Deciles: weighted quantiles by city × month
  # -----------------------------------------------------------------------
  income_monthly <- dataset.k %>%
    mutate(fecha = ymd(paste(year, mes, "01", sep = "-"))) %>%
    group_by(dominio, year, mes) %>%
    mutate(
      decile_breaks = list(
        wtd.quantile(
          x       = ingpcug,
          weights = fex_c,
          probs   = seq(0, 1, by = 0.1),
          na.rm   = TRUE)),
      deciles = as.integer(
        cut(ingpcug,
            breaks         = decile_breaks[[1]],
            labels         = 1:10,
            include.lowest = TRUE,
            right          = TRUE))
    ) %>%
    select(-decile_breaks) %>%
    ungroup() %>%
    mutate(
      share = case_when(
        deciles %in% c(1, 2)  ~ 0.39,
        deciles %in% c(3, 4)  ~ 0.36,
        deciles %in% c(5, 6)  ~ 0.35,
        deciles %in% c(7, 8)  ~ 0.32,
        deciles %in% c(9, 10) ~ 0.26),
      food_exp             = share * ingtotugarr,
      food_exp_pc          = food_exp / npersug,
      food_exp_pc_month    = food_exp_pc,
      food_exp_pc_year     = food_exp_pc * 12,
      # rename for Afford() compatibility
      per_capita_income    = ingpcug,
      income               = ingtotugarr,
      nug                  = as.numeric(npersug),
      deciles_label        = paste0("Decil ", deciles)
    ) %>%
    select(year, mes, fecha, dominio,
           id_hogar, nug,
           income, per_capita_income,
           deciles, deciles_label,
           share,
           food_exp,
           food_exp_pc,
           food_exp_pc_month,
           food_exp_pc_year,
           fex_c)
  
  deciles_all[[as.character(k)]] <- income_monthly
  
  # Save annual file
  saveRDS(income_monthly,
          file.path(INCOME_DIR,
                    sprintf("IncomeCol_%d.rds", k)))
  
  # -----------------------------------------------------------------------
  # Poverty rates
  # -----------------------------------------------------------------------
  poverty.rates <- dataset.k %>%
    mutate(
      dummy.pm  = if_else(ingpcug < lp, 1, 0),
      dummy.pme = if_else(ingpcug < li, 1, 0),
      fecha     = ymd(paste(year, mes, "01", sep = "-"))) %>%
    group_by(fecha, year, mes, dominio) %>%
    summarise(
      pm  = sum(dummy.pm  * fex_c, na.rm=TRUE) /
        sum(fex_c, na.rm=TRUE) * 100,
      pme = sum(dummy.pme * fex_c, na.rm=TRUE) /
        sum(fex_c, na.rm=TRUE) * 100,
      .groups = "drop")
  
  poverty_all[[as.character(k)]] <- poverty.rates
  
  message(sprintf("  %d: %d households | deciles OK | poverty OK",
                  k, n_distinct(income_monthly$id_hogar)))
}

# -----------------------------------------------------------------------
# Consolidate and save
# -----------------------------------------------------------------------
deciles_final <- bind_rows(deciles_all) %>%
  arrange(dominio, year, mes)

poverty_final <- bind_rows(poverty_all) %>%
  arrange(dominio, fecha)

saveRDS(deciles_final,
        file.path(INCOME_DIR, "deciles_final.rds"))
write_csv(deciles_final,
          file.path(INCOME_DIR, "deciles_final.csv"))

saveRDS(poverty_final,
        file.path(INCOME_DIR, "poverty_rates.rds"))
write_csv(poverty_final,
          file.path(INCOME_DIR, "poverty_rates.csv"))

message(sprintf(
  "Done. deciles_final: %d rows | poverty_final: %d rows",
  nrow(deciles_final), nrow(poverty_final)))
message("Proceed to 02_affordability.R")