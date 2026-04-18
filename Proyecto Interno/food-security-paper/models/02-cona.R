########################################################
## Cost of Nutritional Adequacy (CoNA) — Estimation
## Loop over cities and dates — full output
########################################################

library(tidyverse)
library(readxl)
library(FoodpriceR)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

aux_dir    <- file.path(base_dir, "food-security-paper", "models", "aux-functions")
out_cona   <- file.path(base_dir, "food-security-paper", "output", "cona")
out_eer    <- file.path(base_dir, "food-security-paper", "output", "eer")
input1_dir <- file.path(base_dir, "food-security-paper", "output", "tcac_food_table")

source(file.path(aux_dir, "CoNA_paper.R"))

##----------------------------------------------------------
## Input 1: Food table
##----------------------------------------------------------

data_paper <- readRDS(file.path(input1_dir, "panel_city_month_food_1999_2025.rds")) %>%
  dplyr::select(ciudad, fecha, ano, mes_num, articulo, precio_100g,
         grupos_gabas, subgrupos_gabas,
         gramos_g_1_intercambio_1_intercambio,
         energia_kcal, proteina_g, lipidos_g, carbohidratos_totales_g,
         vitamina_c_mg, folatos_mcg, vitamina_a_er, tiamina_mg,
         riboflavina_mg, niacina_mg, vitamina_b12_mcg,
         magnesio_mg, fosforo_mg, sodio_mg, calcio_mg, hierro_mg, zinc_mg) %>%
  distinct() %>%
  filter(fecha >= "2019-01-01", fecha < "2025-01-01") %>%
  dplyr::rename(
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
  )

##----------------------------------------------------------
## Input 2: EER — representative household
## Adult male (31–51), adult female (31–51), female child (10–14)
##----------------------------------------------------------

agg_eer <- read_excel(file.path(out_eer, "220326_agg_eer.xlsx"))

household_eer <- agg_eer %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[10, 14)")
  ) %>%
  mutate(ciudad = case_when(
    cod_mun == "05001" ~ "MEDELLIN",
    cod_mun == "11001" ~ "BOGOTA",
    cod_mun == "76001" ~ "CALI",
    TRUE ~ cod_mun
  )) %>%
  filter(ciudad %in% c("BOGOTA", "MEDELLIN", "CALI")) %>%
  dplyr::rename(Age = rango, Sex = sex, Energy = eer) %>%
  mutate(Sex = if_else(Sex == "Masculino", 0L, 1L)) %>%
  as.data.frame()

##----------------------------------------------------------
## Input 3: Nutrient lower limits (EER_LL) and upper limits (UL)
##----------------------------------------------------------

eer_ll_base <- FoodpriceR::EER_LL %>%
  mutate(Age = recode(Age,
                      "31 a 50 años" = "[31,51)",
                      "9 a 13 años"  = "[10, 14)"))

ul_base <- FoodpriceR::UL %>%
  mutate(Age = recode(Age,
                      "31 a 50 años" = "[31,51)",
                      "9 a 13 años"  = "[10, 14)"))

household_eer_ll <- merge(household_eer,
                          eer_ll_base %>% select(-Energy),
                          by = c("Sex", "Age"))

household_ul <- merge(household_eer,
                      ul_base %>% select(-Energy),
                      by = c("Sex", "Age")) %>%
  select(-Energy) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 9999999)))

##----------------------------------------------------------
## Loop: estimate CoNA for each city × date
##----------------------------------------------------------

dominios <- levels(as.factor(household_eer$ciudad))
fechas   <- levels(as.factor(data_paper$fecha))

out_cost  <- vector("list", length(dominios) * length(fechas))
out_comp  <- vector("list", length(dominios) * length(fechas))
out_limit <- vector("list", length(dominios) * length(fechas))
out_spe   <- vector("list", length(dominios) * length(fechas))

idx <- 1

for (i in dominios) {
  
  eer_ll.aux <- household_eer_ll %>% filter(ciudad == i) %>% select(-ciudad)
  ul.aux     <- household_ul     %>% filter(ciudad == i) %>% select(-ciudad)
  
  for (t in fechas) {
    
    message("Estimating: ", i, " | ", t)
    
    data.aux <- data_paper %>%
      filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
      filter(articulo != "ARROZ PARA SOPA") %>%
      dplyr::rename(Price_100g = precio_100g, Food = articulo) %>%
      as.data.frame()
    
    if (nrow(data.aux) == 0) next
    
    result <- tryCatch(
      CoNA_paper(data = data.aux, EER_LL = eer_ll.aux, UL = ul.aux),
      error = function(e) {
        warning("Error — ciudad: ", i, " | fecha: ", t, " | ", conditionMessage(e))
        NULL
      }
    )
    
    if (!is.null(result)) {
      out_cost[[idx]]  <- result$cost  %>% mutate(ciudad = i, fecha = t)
      out_comp[[idx]]  <- result$comp  %>% mutate(ciudad = i, fecha = t)
      out_limit[[idx]] <- result$limit %>% mutate(ciudad = i, fecha = t)
      out_spe[[idx]]   <- result$spe   %>% mutate(ciudad = i, fecha = t)
      idx <- idx + 1
    }
  }
}

##----------------------------------------------------------
## Consolidate and save
##----------------------------------------------------------

df.cost  <- bind_rows(out_cost)
df.comp  <- bind_rows(out_comp)
df.limit <- bind_rows(out_limit)
df.spe   <- bind_rows(out_spe)

saveRDS(list(cost  = df.cost,
             comp  = df.comp,
             limit = df.limit,
             spe   = df.spe),
        file.path(out_cona, "230326_cona_full.rds"))

writexl::write_xlsx(
  list(cost  = df.cost,
       comp  = df.comp,
       limit = df.limit,
       spe   = df.spe),
  file.path(out_cona, "230326_cona_full.xlsx")
)
