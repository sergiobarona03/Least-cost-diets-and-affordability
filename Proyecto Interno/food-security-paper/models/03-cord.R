########################################################
## Cost of Recommended Diet (CoRD)
## Full script: data loading, harmonization, estimation
########################################################

library(tidyverse)
library(readxl)
library(FoodpriceR)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir   <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

aux_dir    <- file.path(base_dir, "food-security-paper", "models",  "aux-functions")
out_gaba   <- file.path(base_dir, "food-security-paper", "output",  "gaba")
out_cord   <- file.path(base_dir, "food-security-paper", "output",  "cord")
input1_dir <- file.path(base_dir, "food-security-paper", "output",  "tcac_food_table")

source(file.path(aux_dir, "CoRD_Herforth.R"))

##----------------------------------------------------------
## Canonical group vocabulary
##----------------------------------------------------------

GROUP_CANON <- c(
  "Cereales, raíces, tubérculos y plátanos",
  "Frutas",
  "Verduras",
  "Leche y productos lácteos",
  "Carnes, huevos, leguminosas, frutos secos y semillas",
  "Grasas",
  "Azúcares"
)

##----------------------------------------------------------
## Input 1: Food table
## Required columns by CoRD_Herforth(): Food, Serving_g, Price_serving, Group
##----------------------------------------------------------

map_group_paper <- c(
  "AZUCARES"                                                    = "Azúcares",
  "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS"  = "Carnes, huevos, leguminosas, frutos secos y semillas",
  "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"                     = "Cereales, raíces, tubérculos y plátanos",
  "FRUTAS"                                                       = "Frutas",
  "GRASAS"                                                       = "Grasas",
  "LECHE Y PRODUCTOS LACTEOS"                                    = "Leche y productos lácteos",
  "VERDURAS"                                                     = "Verduras"
)

data_paper <- readRDS(
  file.path(input1_dir, "panel_city_month_food_1999_2025.rds")
) %>%
  select(ciudad, fecha, ano, mes_num, articulo, precio_100g,
         grupos_gabas, subgrupos_gabas,
         gramos_g_1_intercambio_1_intercambio,
         energia_kcal, proteina_g, lipidos_g, carbohidratos_totales_g,
         vitamina_c_mg, folatos_mcg, vitamina_a_er, tiamina_mg,
         riboflavina_mg, niacina_mg, vitamina_b12_mcg,
         magnesio_mg, fosforo_mg, sodio_mg, calcio_mg, hierro_mg, zinc_mg) %>%
  distinct() %>%
  filter(fecha >= "2019-01-01", fecha < "2025-01-01") %>%
  rename(
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
    Zinc          = zinc_mg,
    Serving_g     = gramos_g_1_intercambio_1_intercambio,
    Group         = grupos_gabas
  ) %>%
  # Price_serving required by CoRD_Herforth() = price per 100g × serving_g / 100
  mutate(Price_serving = precio_100g * Serving_g / 100) %>%
  # Separate fruits and vegetables before recoding
  mutate(
    Group = if_else(subgrupos_gabas == "FRUTAS",   "FRUTAS",   Group),
    Group = if_else(subgrupos_gabas == "VERDURAS", "VERDURAS", Group)
  ) %>%
  mutate(Group = recode(Group, !!!map_group_paper)) %>%
  filter(Group %in% GROUP_CANON)

##----------------------------------------------------------
## Input 2: Adjusted servings (GABAS)
## Required columns by CoRD_Herforth(): Age, Serving, Group
## Optional: Sex
##----------------------------------------------------------

map_group_serv <- c(
  "AZÚCARES"                                                     = "Azúcares",
  "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS"         = "Carnes, huevos, leguminosas, frutos secos y semillas",
  "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"                      = "Cereales, raíces, tubérculos y plátanos",
  "FRUTAS"                                                        = "Frutas",
  "GRASAS"                                                        = "Grasas",
  "LECHE Y PRODUCTOS LÁCTEOS"                                     = "Leche y productos lácteos",
  "VERDURAS"                                                      = "Verduras"
)

serv_adj <- read_excel(
  file.path(out_gaba, "230326_ajd_gabas_exchanges.xlsx")
) %>%
  filter(cod_mun != "Nacional") %>%
  select(-c("e_kcal", "factor_ajuste", "n_exchanges", "e_kcal_adj")) %>%
  rename(
    Age     = rango,
    Sex     = sex,
    Group   = grupo_principal,
    Serving = n_exchanges_adj
  ) %>%
  filter(
    (Sex == "Masculino" & Age == "[31,51)") |
      (Sex == "Femenino"  & Age == "[31,51)") |
      (Sex == "Femenino"  & Age == "[10, 14)")
  ) %>%
  mutate(
    Group  = recode(Group, !!!map_group_serv),
    Sex    = if_else(Sex == "Masculino", 0L, 1L),
    ciudad = recode(cod_mun,
                    "05001" = "MEDELLIN",
                    "11001" = "BOGOTA",
                    "76001" = "CALI")
  ) %>%
  select(-cod_mun) %>%
  filter(Group %in% GROUP_CANON)

##----------------------------------------------------------
## Input 3: Diversity requirements
## Required columns by CoRD_Herforth(): Group (or Subgroup), Number
##----------------------------------------------------------

div_cord_harm <- tribble(
  ~Group,                                                          ~Number,
  "Cereales, raíces, tubérculos y plátanos",                        3,
  "Frutas",                                                         2,
  "Verduras",                                                       2,
  "Leche y productos lácteos",                                      1,
  "Carnes, huevos, leguminosas, frutos secos y semillas",           2,
  "Grasas",                                                         1,
  "Azúcares",                                                       1
)



##----------------------------------------------------------
## Loop: estimate CoRD for each city × date
##----------------------------------------------------------

dominios <- levels(as.factor(serv_adj$ciudad))
fechas   <- levels(as.factor(data_paper$fecha))

out_cost <- vector("list", length(dominios) * length(fechas))
out_comp <- vector("list", length(dominios) * length(fechas))
idx      <- 1

for (i in dominios) {
  
  # serv filtered by city, drop ciudad before passing to function
  serv.aux <- serv_adj %>% filter(ciudad == i) %>% select(-ciudad)
  
  for (t in fechas) {
    
    message("Estimating: ", i, " | ", t)
    
    data.aux <- data_paper %>%
      filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
      rename(Food = articulo) %>%
      as.data.frame()
    
    if (nrow(data.aux) == 0) next
    
    result <- tryCatch(
      CoRD_Herforth(data    = data.aux,
                    serv    = serv.aux,
                    diverse = div_cord_harm),
      error = function(e) {
        warning("Error — ciudad: ", i, " | fecha: ", t,
                " | ", conditionMessage(e))
        NULL
      }
    )
    
    if (!is.null(result)) {
      out_cost[[idx]] <- result$cost %>% mutate(ciudad = i, fecha = t)
      out_comp[[idx]] <- result$comp %>% mutate(ciudad = i, fecha = t)
      idx <- idx + 1
    }
  }
}

##----------------------------------------------------------
## Consolidate and save
##----------------------------------------------------------

df.cost <- bind_rows(out_cost)
df.comp <- bind_rows(out_comp)

saveRDS(list(cost = df.cost, comp = df.comp),
        file.path(out_cord, "230326_cord_full.rds"))

writexl::write_xlsx(
  list(cost = df.cost, comp = df.comp),
  file.path(out_cord, "230326_cord_full.xlsx")
)

