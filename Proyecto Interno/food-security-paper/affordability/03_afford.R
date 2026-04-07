########################################################
## Cálculo de indicadores de asequibilidad — mensual
## Deciles, costos e indicadores: todo mensual
########################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(Hmisc)

##----------------------------------------------------------
## Directorios
##----------------------------------------------------------

base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

source("food-security-paper/affordability/aux-functions/Afford_Expansion.R")

out_hcost  <- file.path(base_dir, "food-security-paper", "output", "hcost")
out_dir    <- file.path(base_dir, "food-security-paper", "output")
income_dir <- file.path(out_dir,  "income_col")
afford_dir <- file.path(out_dir,  "affordability_metrics")

dir.create(afford_dir, recursive = TRUE, showWarnings = FALSE)

##----------------------------------------------------------
## 1. Cargar base de ingresos mensual
##----------------------------------------------------------

income_data <- read.csv(file.path(income_dir, "deciles_food_income.csv")) %>%
  select(year, mes, dominio, id_hogar, nug,
         income, per_capita_income, fex_c) %>%
  distinct()

##----------------------------------------------------------
## 2. Calcular deciles ponderados mensuales
##
## Deciles calculados dentro de cada ciudad × mes usando
## wtd.quantile() con fex_c como ponderador
##----------------------------------------------------------

income_monthly <- income_data %>%
  mutate(fecha = ymd(paste(year, mes, "01", sep = "-"))) %>%
  group_by(dominio, year, mes) %>%
  mutate(
    decile_breaks = list(
      wtd.quantile(
        x       = per_capita_income,
        weights = fex_c,
        probs   = seq(0, 1, by = 0.1),
        na.rm   = TRUE
      )
    ),
    deciles = as.integer(
      cut(per_capita_income,
          breaks         = decile_breaks[[1]],
          labels         = 1:10,
          include.lowest = TRUE,
          right          = TRUE)
    )
  ) %>%
  select(-decile_breaks) %>%
  ungroup() %>%
  mutate(
    share = case_when(
      deciles %in% c(1, 2)  ~ 0.39,
      deciles %in% c(3, 4)  ~ 0.36,
      deciles %in% c(5, 6)  ~ 0.35,
      deciles %in% c(7, 8)  ~ 0.32,
      deciles %in% c(9, 10) ~ 0.26
    ),
    food_exp          = share * income,
    food_exp_pc       = food_exp / nug,
    food_exp_pc_month = food_exp_pc,
    food_exp_pc_year  = food_exp_pc * 12
  )

##----------------------------------------------------------
## 3. Renombrar columnas para compatibilidad con Afford()
##
## Afford() espera:
##   deciles                  → "Decil 1", ..., "Decil 10"
##   ung                      → tamaño del hogar
##   income                   → ingreso total del hogar
##   per_capita_income        → ingreso per cápita
##   food_exp_per_capita      → gasto alimentos per cápita mensual
##   food_exp_per_capita_year → gasto alimentos per cápita anual
##   food_income              → gasto total en alimentos del hogar
##   fex_c18                  → factor de expansión
##   share                    → participación gasto alimentario
##----------------------------------------------------------

income_df <- income_monthly %>%
  mutate(
    deciles                  = paste0("Decil ", deciles),
    ung                      = nug,
    food_exp_per_capita      = food_exp_pc_month,
    food_exp_per_capita_year = food_exp_pc_year,
    food_income              = food_exp,
    fex_c18                  = fex_c,
    ciudad = case_when(
      dominio == "MEDELLIN" ~ "MEDELLÍN",
      dominio == "BOGOTA"   ~ "BOGOTÁ D.C.",
      TRUE                  ~ dominio
    )
  ) %>%
  select(ciudad, dominio, year, mes, fecha,
         id_hogar, ung, income, per_capita_income,
         deciles, share,
         food_income, food_exp_per_capita,
         food_exp_per_capita_year, fex_c18)

##----------------------------------------------------------
## 4. Cargar costos de dieta y alinear nombres de ciudad
##----------------------------------------------------------

recode_ciudad <- function(df) {
  df %>%
    mutate(ciudad = case_when(
      ciudad == "MEDELLIN" ~ "MEDELLÍN",
      ciudad == "BOGOTA"   ~ "BOGOTÁ D.C.",
      TRUE                 ~ ciudad
    ))
}

hcost_diets <- readxl::read_excel(
  file.path(out_hcost, "230326_hcost_full.xlsx"), sheet = 1
) %>%
  mutate(
    fecha = as.Date(fecha),
    year  = year(fecha),
    mes   = month(fecha)
  )

coca_df <- hcost_diets %>% filter(model == "CoCA") %>% recode_ciudad()
cona_df <- hcost_diets %>% filter(model == "CoNA") %>% recode_ciudad()
cord_df <- hcost_diets %>% filter(model == "CoRD") %>% recode_ciudad()

##----------------------------------------------------------
## 5. Loop: calcular Afford por ciudad × mes
##----------------------------------------------------------

city_vector <- sort(unique(income_df$ciudad))
date_vector <- income_df %>%
  distinct(year, mes) %>%
  arrange(year, mes)

res_afford <- list()
k <- 1

for (city.x in city_vector) {
  for (row.x in seq_len(nrow(date_vector))) {
    
    year.x <- date_vector$year[row.x]
    mes.x  <- date_vector$mes[row.x]
    
    message("Procesando: ", city.x, " | ", year.x, "-", sprintf("%02d", mes.x))
    
    # Filtrar ingreso — hogares del mes
    inc_aux <- income_df %>%
      filter(ciudad == city.x, year == year.x, mes == mes.x)
    
    if (nrow(inc_aux) == 0) next
    
    # Filtrar costo mensual
    coca_aux <- coca_df %>% filter(ciudad == city.x, year == year.x, mes == mes.x)
    cona_aux <- cona_df %>% filter(ciudad == city.x, year == year.x, mes == mes.x)
    cord_aux <- cord_df %>% filter(ciudad == city.x, year == year.x, mes == mes.x)
    
    if (nrow(coca_aux) == 0 | nrow(cona_aux) == 0 | nrow(cord_aux) == 0) next
    
    # Ejecutar Afford
    out <- tryCatch(
      Afford(
        Hexpense   = inc_aux,
        Model_CoCA = coca_aux,
        Model_CoNA = cona_aux,
        Model_CoRD = cord_aux
      ),
      error = function(e) {
        warning("Error — ciudad: ", city.x, " | ",
                year.x, "-", sprintf("%02d", mes.x),
                " | ", conditionMessage(e))
        NULL
      }
    )
    
    if (is.null(out)) next
    
    fecha.x <- ymd(paste(year.x, mes.x, "01", sep = "-"))
    
    po <- out$Poverty_outcome %>%
      mutate(ciudad = city.x,
             fecha  = fecha.x,
             year   = year.x,
             mes    = mes.x)
    
    mf <- out$Mean_income_food %>%
      mutate(ciudad = city.x,
             fecha  = fecha.x,
             year   = year.x,
             mes    = mes.x)
    
    res_afford[[k]] <- list(poverty = po, mean_food = mf)
    k <- k + 1
  }
}

##----------------------------------------------------------
## 6. Consolidar
##----------------------------------------------------------

afford_poverty <- bind_rows(lapply(res_afford, `[[`, "poverty")) %>%
  mutate(fecha = as.Date(fecha)) %>%
  arrange(ciudad, fecha, model, deciles)

afford_food <- bind_rows(lapply(res_afford, `[[`, "mean_food")) %>%
  mutate(fecha = as.Date(fecha)) %>%
  arrange(ciudad, fecha, decile_groups)

##----------------------------------------------------------
## 7. Guardar
##----------------------------------------------------------

saveRDS(list(poverty   = afford_poverty,
             mean_food = afford_food),
        file.path(afford_dir, "Afford_results_monthly.rds"))

writexl::write_xlsx(
  list(poverty   = afford_poverty,
       mean_food = afford_food),
  file.path(afford_dir, "Afford_results_monthly.xlsx")
)

