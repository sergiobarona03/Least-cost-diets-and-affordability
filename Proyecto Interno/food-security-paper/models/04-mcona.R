########################################################
## IPC-Constrained Cost of Nutritional Adequacy (CoNA-IPC)
## Minimum-cost diet with IPC quantity share constraints
## Loop over cities, dates, and alpha values — full output
########################################################

library(tidyverse)
library(readxl)
library(FoodpriceR)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir   <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"

aux_dir    <- file.path(base_dir, "food-security-paper", "models", "aux-functions")
out_ipc    <- file.path(base_dir, "food-security-paper", "output", "cona-ipc")
out_eer    <- file.path(base_dir, "food-security-paper", "output", "eer")
input1_dir <- file.path(base_dir, "food-security-paper", "output", "tcac_food_table")

source(file.path(aux_dir, "CoNA_IPC_paper.R"))

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
    Zinc          = zinc_mg
  ) %>%
  # Separate fruits and vegetables before passing to the model
  mutate(
    Group = grupos_gabas,
    Group = if_else(subgrupos_gabas == "FRUTAS",   "FRUTAS",   Group),
    Group = if_else(subgrupos_gabas == "VERDURAS", "VERDURAS", Group)
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
  rename(Age = rango, Sex = sex, Energy = eer) %>%
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
## Input 4: IPC expenditure shares by food group
## Must match exactly the Group values in data_paper$Group
##----------------------------------------------------------

ipc_shares <- tribble(
  ~Group,                                                                ~share,
  "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS",                           0.30,
  "FRUTAS",                                                             0.10,
  "VERDURAS",                                                           0.10,
  "LECHE Y PRODUCTOS LACTEOS",                                          0.15,
  "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS",         0.25,
  "GRASAS",                                                             0.05,
  "AZUCARES",                                                           0.05
) %>% as.data.frame()

stopifnot(abs(sum(ipc_shares$share) - 1) < 1e-6)

# Verify group name alignment before running
missing_in_data <- setdiff(ipc_shares$Group, unique(data_paper$Group))
if (length(missing_in_data) > 0)
  warning("Groups in ipc_shares not found in data_paper$Group: ",
          paste(missing_in_data, collapse = ", "))

##----------------------------------------------------------
## Alpha grid — sensitivity analysis on quantity share enforcement
## Constraint: sum_{i in g}(x_i) / sum_i(x_i) >= alpha * s_g
##
## alpha = 0    : CoNA estándar (no IPC constraint)
## alpha = 0.50 : each group must represent at least 50% of its IPC quantity share
## alpha = 0.75 : 75% of IPC quantity share
## alpha = 1.00 : full IPC quantity share enforced (most restrictive)
##----------------------------------------------------------

alphas <- c(0, 1.00)

##----------------------------------------------------------
## Loop: estimate CoNA-IPC for each city × date × alpha
##----------------------------------------------------------

dominios <- levels(as.factor(household_eer$ciudad))
fechas   <- levels(as.factor(data_paper$fecha))

out_cost  <- list()
out_comp  <- list()
out_limit <- list()
out_spe   <- list()

for (alp in alphas) {
  
  
  
  message("\n========== alpha = ", alp, " ==========")
  
  for (i in dominios) {
    
    eer_ll.aux <- household_eer_ll %>% filter(ciudad == i) %>% select(-ciudad)
    ul.aux     <- household_ul     %>% filter(ciudad == i) %>% select(-ciudad)
    
    for (t in fechas) {
      
      message("Estimating: ", i, " | ", t, " | alpha = ", alp)
      
      data.aux <- data_paper %>%
        filter(ciudad == i, fecha == t, !is.na(precio_100g)) %>%
        filter(articulo != "ARROZ PARA SOPA") %>%
        rename(Price_100g = precio_100g, Food = articulo) %>%
        as.data.frame()
      
      if (nrow(data.aux) == 0) next
      
      result <- tryCatch(
        CoNA_IPC_paper(
          data       = data.aux,
          EER_LL     = eer_ll.aux,
          UL         = ul.aux,
          IPC_shares = ipc_shares,
          alpha      = alp
        ),
        error = function(e) {
          warning("Error — ciudad: ", i, " | fecha: ", t,
                  " | alpha: ", alp, " | ", conditionMessage(e))
          NULL
        }
      )
      
      if (!is.null(result)) {
        out_cost[[length(out_cost) + 1]]   <- result$cost  %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        out_comp[[length(out_comp) + 1]]   <- result$comp  %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        out_limit[[length(out_limit) + 1]] <- result$limit %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
        out_spe[[length(out_spe) + 1]]     <- result$spe   %>%
          mutate(ciudad = i, fecha = t, alpha_val = alp)
      }
    }
  }
}

##----------------------------------------------------------
## Consolidate
##----------------------------------------------------------

df.cost  <- bind_rows(out_cost)
df.comp  <- bind_rows(out_comp)
df.limit <- bind_rows(out_limit)
df.spe   <- bind_rows(out_spe)

message("Done. Rows in df.cost: ", nrow(df.cost))

##----------------------------------------------------------
## Save — combined file + one file per alpha
##----------------------------------------------------------

# Combined file with alpha column
saveRDS(list(cost  = df.cost,
             comp  = df.comp,
             limit = df.limit,
             spe   = df.spe),
        file.path(out_ipc, "230326_cona_ipc_full.rds"))

writexl::write_xlsx(
  list(cost  = df.cost,
       comp  = df.comp,
       limit = df.limit,
       spe   = df.spe),
  file.path(out_ipc, "230326_cona_ipc_full.xlsx")
)

# Separate file per alpha for convenience
for (alp in alphas) {
  alp_tag <- gsub("\\.", "", as.character(alp))
  saveRDS(
    list(cost  = df.cost  %>% filter(alpha_val == alp),
         comp  = df.comp  %>% filter(alpha_val == alp),
         limit = df.limit %>% filter(alpha_val == alp),
         spe   = df.spe   %>% filter(alpha_val == alp)),
    file.path(out_ipc, paste0("230326_cona_ipc_alpha", alp_tag, ".rds"))
  )
}

message("All outputs saved to: ", out_ipc)