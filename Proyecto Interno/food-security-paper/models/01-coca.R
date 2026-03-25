########################################################
## Estimaciones del modelo CoCA
## Loop sobre ciudades y fechas
########################################################

library(tidyverse)
library(readxl)

##----------------------------------------------------------
## Directorios
##----------------------------------------------------------

base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

aux_dir  <- file.path(base_dir, "food-security-paper", "models", "aux-functions")
out_coca <- file.path(base_dir, "food-security-paper", "output", "coca")
out_eer  <- file.path(base_dir, "food-security-paper", "output", "eer")
input1_dir <- file.path(base_dir, "food-security-paper", "output", "tcac_food_table")

# Cargar función CoCA
source(file.path(aux_dir, "CoCA_paper.R"))

##----------------------------------------------------------
## Input 1: Food table
##----------------------------------------------------------

data_paper <- readRDS(file.path(input1_dir, "panel_city_month_food_1999_2025.rds")) %>%
  select(ciudad, fecha, ano, mes_num, articulo, precio_100g,
         grupos_gabas, subgrupos_gabas,
         gramos_g_1_intercambio_1_intercambio,
         energia_kcal, proteina_g, lipidos_g, carbohidratos_totales_g,
         vitamina_c_mg, folatos_mcg, vitamina_a_er, tiamina_mg,
         riboflavina_mg, niacina_mg, vitamina_b12_mcg,
         magnesio_mg, fosforo_mg, sodio_mg, calcio_mg, hierro_mg, zinc_mg) %>%
  distinct() %>%
  filter(fecha >= "2019-01-01", fecha < "2025-01-01",
         grupos_gabas == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS")

##----------------------------------------------------------
## Input 2: EER del hogar representativo
## Hombres 31-51, Mujeres 31-51, Niña 10-14
##----------------------------------------------------------

agg_eer <- read_excel(file.path(out_eer, "220326_agg_eer.xlsx"))

household_eer <- agg_eer %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[10, 14)")
  ) %>%
  mutate(ciudad = case_when(
    cod_mun == "05001"    ~ "MEDELLIN",
    cod_mun == "11001"    ~ "BOGOTA",
    cod_mun == "76001"    ~ "CALI",
    cod_mun == "Nacional" ~ "COLOMBIA",
    TRUE ~ cod_mun
  )) %>%
  filter(ciudad != "COLOMBIA")

##----------------------------------------------------------
## Función auxiliar: preparar EER para CoCA
##----------------------------------------------------------

preparar_eer <- function(eer_ciudad) {
  eer_ciudad %>%
    rename(Age = rango, Sex = sex, Energy = eer) %>%
    mutate(Sex = if_else(Sex == "Masculino", 0L, 1L)) %>%
    as.data.frame()
}

##----------------------------------------------------------
## Loop: estimar CoCA para cada ciudad × fecha
##----------------------------------------------------------

dominios <- levels(as.factor(household_eer$ciudad))
fechas   <- levels(as.factor(data_paper$fecha))

resultados <- vector("list", length(dominios) * length(fechas))
idx <- 1

for (i in dominios) {
  
  eer.aux <- preparar_eer(household_eer %>% filter(ciudad == i))
  
  for (t in fechas) {
    print(paste0("Estimando ", i , " en ", t))
    data.aux <- data_paper %>%
      filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
      filter(articulo != "ARROZ PARA SOPA") %>%
      rename(Price_100g = precio_100g,
             Food       = articulo,
             Energy     = energia_kcal) %>%
      as.data.frame()
    
    coca.aux <- CoCA_paper(data = data.aux, EER = eer.aux)
    
    if (!is.null(coca.aux)) {
      resultados[[idx]] <- coca.aux$cost %>%
        mutate(ciudad = i, fecha = t)
      idx <- idx + 1
    }
  }
}

##----------------------------------------------------------
## Consolidar y guardar
##----------------------------------------------------------

df.coca <- bind_rows(resultados)

writexl::write_xlsx(df.coca, file.path(out_coca, "230326_coca_results.xlsx"))
saveRDS(df.coca,  file.path(out_coca, "230326_coca_results.rds"))


