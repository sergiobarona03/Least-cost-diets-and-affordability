#######################################################################
## Cálculo de pobreza + Deciles + Gasto en alimentos (TRIMESTRAL)
#######################################################################

#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(devtools)
library(tidyverse)
library(FoodpriceR)
library(lubridate)
library(reshape2)
library(janitor)

#----------------------------------------------------------------------
# Directorio base
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

dir.create("working-papers/working-paper-ipc/output/incomecol",
           recursive = TRUE, showWarnings = FALSE)

dir.create("working-papers/working-paper-ipc/output/poverty_rates",
           recursive = TRUE, showWarnings = FALSE)

dir.create("working-papers/working-paper-ipc/output/deciles_income",
           recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Inicializar objetos acumuladores
#----------------------------------------------------------------------
poverty_all  <- list()
deciles_all  <- list()

#----------------------------------------------------------------------
# Loop años
#----------------------------------------------------------------------
for(k in 2018:2024){
  
  cat("Procesando año:", k, "\n")
  
  #-------------------------
  # Cargar bases
  #-------------------------
  personas.aux <- readRDS(paste0("Pobreza/personas/personas_pobreza_", k, ".rds")) %>%
    clean_names()
  
  if("dpto" %in% names(personas.aux) & !("depto" %in% names(personas.aux))){
    names(personas.aux)[names(personas.aux) == "dpto"] <- "depto"
  }
  
  hogares.aux <- readRDS(paste0("Pobreza/hogares/hogares_pobreza_", k, ".rds")) %>%
    clean_names()
  
  if("dpto" %in% names(hogares.aux) & !("depto" %in% names(hogares.aux))){
    names(hogares.aux)[names(hogares.aux) == "dpto"] <- "depto"
  }
  
  if("ingtotarr" %in% names(hogares.aux) &
     !("ingtotugarr" %in% names(hogares.aux))){
    
    names(hogares.aux)[names(hogares.aux) == "ingtotarr"] <- "ingtotugarr"
  }
  
  #-------------------------
  # Hogares
  #-------------------------
  hogares.aux2 <- hogares.aux %>%
    mutate(id_hogar = paste0(directorio,"-",secuencia_p)) %>%
    filter(clase == 1) %>%
    select(year, mes, dominio, id_hogar,
           nper, npersug,
           ingtotug, ingtotugarr,
           ingpcug, li, lp,
           pobre, indigente)
  
  #-------------------------
  # Personas
  #-------------------------
  personas.aux2 <- personas.aux %>%
    mutate(id_hogar = paste0(directorio,"-",secuencia_p),
           id = paste0(directorio,"-", secuencia_p, "-", orden)) %>%
    filter(clase == 1) %>%
    select(year, mes, id, id_hogar,
           clase, dominio,
           depto, ingtot, fex_c)  # fex_c queda leído pero NO se usa
  
  #-------------------------
  # Merge
  #-------------------------
  dataset.k <- merge(personas.aux2, hogares.aux2,
                     by = c("year", "mes", "dominio", "id_hogar"))
  
  #-------------------------
  # Filtrar ciudades
  #-------------------------
  dataset.k <- dataset.k %>%
    filter(dominio %in% c("CALI", "MEDELLIN", "BOGOTA"))
  
  #-------------------------
  # Conversión numérica (sin usar fex en cálculos)
  #-------------------------
  dataset.k$fex_c        <- as.numeric(gsub(",", ".", dataset.k$fex_c))
  dataset.k$ingpcug      <- as.numeric(gsub(",", ".", dataset.k$ingpcug))
  dataset.k$ingtotugarr  <- as.numeric(gsub(",", ".", dataset.k$ingtotugarr))
  dataset.k$lp           <- as.numeric(gsub(",", ".", dataset.k$lp))
  dataset.k$li           <- as.numeric(gsub(",", ".", dataset.k$li))
  dataset.k$pobre        <- as.numeric(dataset.k$pobre)
  dataset.k$indigente    <- as.numeric(dataset.k$indigente)
  
  #====================================================================
  #           DECILES + SHARE + FOOD EXP (TRIMESTRAL, MEDIANA)
  #====================================================================
  
  dataset_def_deciles <- dataset.k %>%
    mutate(
      fecha = ymd(paste(year, mes, "01", sep = "-")),
      q = quarter(fecha),
      trimestre = paste0(year, "Q", q)
    ) %>%
    # 1) Consolidar a nivel hogar dentro del trimestre (medianas)
    group_by(trimestre, year, q, dominio, id_hogar) %>%
    dplyr::summarize(
      nug = as.numeric(median(npersug, na.rm = TRUE)),
      income = median(ingtotugarr, na.rm = TRUE),
      per_capita_income = median(ingpcug, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # 2) Recalcular deciles trimestrales con el ingreso per cápita trimestral
    group_by(trimestre, year, q, dominio) %>%
    mutate(
      deciles = ntile(per_capita_income, 10)
    ) %>%
    ungroup() %>%
    # 3) Asignar share por decil
    mutate(
      share = case_when(
        deciles %in% c(1,2)  ~ 0.39,
        deciles %in% c(3,4)  ~ 0.36,
        deciles %in% c(5,6)  ~ 0.35,
        deciles %in% c(7,8)  ~ 0.32,
        deciles %in% c(9,10) ~ 0.26
      )
    ) %>%
    # 4) Calcular gasto alimentos (a partir de la mediana del ingreso mensual del trimestre)
    mutate(
      food_exp = share * income,
      food_exp_per_capita = food_exp / nug,
      food_exp_per_capita_year = food_exp_per_capita * 12
    ) %>%
    select(
      year, q, trimestre,
      dominio, id_hogar,
      nug, income, per_capita_income,
      deciles, share,
      food_exp, food_exp_per_capita, food_exp_per_capita_year
    )
  
  deciles_all[[as.character(k)]] <- dataset_def_deciles
  
  #----------------------------------------------------------
  #  GUARDAR BASE ANUAL (AHORA TRIMESTRAL DENTRO DEL AÑO)
  #----------------------------------------------------------
  saveRDS(dataset_def_deciles,
          paste0("working-papers/working-paper-ipc/output/incomecol/",
                 "IncomeCol_quartiles_", k, ".rds"))
  
  #====================================================================
  #                 CÁLCULO POBREZA (TRIMESTRAL, MEDIANA)
  #====================================================================
  
  poverty.rates <- dataset.k %>%
    mutate(
      fecha = ymd(paste(year, mes, "01", sep = "-")),
      q = quarter(fecha),
      trimestre = paste0(year, "Q", q),
      dummy.pm  = ifelse(ingpcug < lp, 1, 0),
      dummy.pme = ifelse(ingpcug < li, 1, 0)
    ) %>%
    # tasa mensual sin ponderar (promedio simple)
    group_by(trimestre, year, q, dominio, fecha) %>%
    dplyr::summarize(
      pm_m  = mean(dummy.pm, na.rm = TRUE) * 100,
      pme_m = mean(dummy.pme, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    # agregación trimestral por mediana de las tasas mensuales
    group_by(trimestre, year, q, dominio) %>%
    dplyr::summarize(
      pm  = median(pm_m, na.rm = TRUE),
      pme = median(pme_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  poverty_all[[as.character(k)]] <- poverty.rates
}

#====================================================================
# UNIR RESULTADOS
#====================================================================

poverty_final <- bind_rows(poverty_all)
deciles_final <- bind_rows(deciles_all)

#----------------------------------------------------------------------
# Guardar (marcado como cuartiles/quarter)
#----------------------------------------------------------------------
saveRDS(deciles_final,
        "working-papers/working-paper-ipc/output/deciles_income/deciles_food_income_cuartiles.rds")

write.csv(deciles_final,
          "working-papers/working-paper-ipc/output/deciles_income/deciles_food_income_cuartiles.csv",
          row.names = FALSE)

saveRDS(poverty_final,
        "working-papers/working-paper-ipc/output/poverty_rates/poverty_rates_city_cuartiles.rds")

write.csv(poverty_final,
          "working-papers/working-paper-ipc/output/poverty_rates/poverty_rates_city_cuartiles.csv",
          row.names = FALSE)
