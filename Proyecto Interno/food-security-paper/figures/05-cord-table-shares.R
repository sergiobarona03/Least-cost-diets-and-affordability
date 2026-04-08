########################################################
## Cost of Recommended Diet (CoRD)
## Summary table: Mean food group cost shares by year × city
## Fruits and Vegetables merged into one group
## Period: 2019–2024
########################################################

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

base_dir   <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno"
out_cord   <- file.path(base_dir, "food-security-paper", "output", "cord")
out_tab    <- file.path(base_dir, "food-security-paper", "output", "tables")
input1_dir <- file.path(base_dir, "food-security-paper", "output", "tcac_food_table")

dir.create(out_tab, recursive = TRUE, showWarnings = FALSE)

##----------------------------------------------------------
## Load data
##----------------------------------------------------------

cord <- readRDS(file.path(out_cord, "230326_cord_full.rds"))

df.comp <- cord$comp %>%
  mutate(fecha = as.Date(fecha), year = year(fecha))

data_paper_prices <- readRDS(
  file.path(input1_dir, "panel_city_month_food_1999_2025.rds")
) %>%
  select(ciudad, fecha, articulo,
         grupos_gabas, subgrupos_gabas,
         precio_100g,
         gramos_g_1_intercambio_1_intercambio) %>%
  distinct() %>%
  filter(fecha >= "2019-01-01", fecha < "2025-01-01") %>%
  rename(
    Food      = articulo,
    Serving_g = gramos_g_1_intercambio_1_intercambio,
    Group     = grupos_gabas
  ) %>%
  mutate(
    fecha         = as.Date(fecha),
    Price_serving = precio_100g * Serving_g / 100,
    Group         = if_else(subgrupos_gabas == "FRUTAS",   "FRUTAS",   Group),
    Group         = if_else(subgrupos_gabas == "VERDURAS", "VERDURAS", Group)
  )

##----------------------------------------------------------
## Labels — Fruits and Vegetables merged
##----------------------------------------------------------

city_labels <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

group_labels <- c(
  "Cereales, raíces, tubérculos y plátanos"              = "Cereals, roots & plantains",
  "FRUTAS"                                               = "Fruits & vegetables",
  "Frutas"                                               = "Fruits & vegetables",
  "VERDURAS"                                             = "Fruits & vegetables",
  "Verduras"                                             = "Fruits & vegetables",
  "Leche y productos lácteos"                            = "Dairy",
  "Carnes, huevos, leguminosas, frutos secos y semillas" = "Meat, eggs & legumes",
  "Grasas"                                               = "Fats",
  "Azúcares"                                             = "Sugars"
)

group_order <- c("Dairy", "Meat, eggs & legumes",
                 "Cereals, roots & plantains",
                 "Fruits & vegetables", "Fats", "Sugars")

##----------------------------------------------------------
## Build cost contributions
##----------------------------------------------------------

comp_cost <- df.comp %>%
  left_join(
    data_paper_prices %>% select(ciudad, fecha, Food, Price_serving, Group),
    by = c("ciudad", "fecha", "Food")
  ) %>%
  mutate(
    cost_contrib = Price_serving * Number_Serving,
    ciudad_label = recode(ciudad, !!!city_labels),
    Group_en     = recode(Group.x, !!!group_labels),
    year         = year(fecha)
  )

##----------------------------------------------------------
## Helper function: compute annual cost shares
## Step 1: sum cost by group × city × month
## Step 2: compute monthly share within city × month
## Step 3: average monthly shares within year × city
##----------------------------------------------------------

compute_shares <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(c(group_vars, "fecha", "Group_en")))) %>%
    dplyr::summarize(cost_group = sum(cost_contrib, na.rm = TRUE),
                     .groups = "drop") %>%
    group_by(across(all_of(c(group_vars, "fecha")))) %>%
    mutate(
      total_cost = sum(cost_group),
      share      = cost_group / total_cost * 100
    ) %>%
    ungroup() %>%
    group_by(across(all_of(c(group_vars, "Group_en")))) %>%
    dplyr::summarize(
      mean_share = round(mean(share, na.rm = TRUE), 1),
      .groups    = "drop"
    ) %>%
    mutate(Group_en = factor(Group_en, levels = group_order)) %>%
    arrange(across(all_of(group_vars)), Group_en)
}

##----------------------------------------------------------
## Table 1: By city × year
## Rows: city × food group | Columns: year
##----------------------------------------------------------

shares_city_year <- compute_shares(comp_cost, c("ciudad_label", "year"))

table_city_year <- shares_city_year %>%
  pivot_wider(names_from  = year,
              values_from = mean_share) %>%
  arrange(ciudad_label, Group_en) %>%
  rename(City         = ciudad_label,
         `Food group` = Group_en)

##----------------------------------------------------------
## Table 2: Pooled across cities, by year
## Rows: food group | Columns: year
##----------------------------------------------------------

shares_pooled_year <- compute_shares(comp_cost, "year")

table_pooled_year <- shares_pooled_year %>%
  pivot_wider(names_from  = year,
              values_from = mean_share) %>%
  arrange(Group_en) %>%
  rename(`Food group` = Group_en)

##----------------------------------------------------------
## Table 3: By city, pooled across years (period mean)
## Rows: food group | Columns: city
##----------------------------------------------------------

shares_city_pooled <- comp_cost %>%
  group_by(ciudad_label, fecha, Group_en) %>%
  dplyr::summarize(cost_group = sum(cost_contrib, na.rm = TRUE),
                   .groups = "drop") %>%
  group_by(ciudad_label, fecha) %>%
  mutate(total_cost = sum(cost_group),
         share      = cost_group / total_cost * 100) %>%
  ungroup() %>%
  group_by(ciudad_label, Group_en) %>%
  dplyr::summarize(
    mean_share = round(mean(share, na.rm = TRUE), 1),
    .groups    = "drop"
  ) %>%
  mutate(Group_en = factor(Group_en, levels = group_order)) %>%
  pivot_wider(names_from  = ciudad_label,
              values_from = mean_share) %>%
  arrange(Group_en) %>%
  rename(`Food group` = Group_en) %>%
  select(`Food group`, Bogotá, Cali, Medellín)

##----------------------------------------------------------
## Save
##----------------------------------------------------------

write_xlsx(
  list(
    `By city and year`     = table_city_year,
    `Pooled across cities` = table_pooled_year,
    `By city (period mean)` = shares_city_pooled,
    `Raw values`           = shares_city_year %>%
      rename(City           = ciudad_label,
             `Food group`   = Group_en,
             Year           = year,
             `Mean share (%)` = mean_share)
  ),
  file.path(out_tab, "table_cord_cost_shares_annual.xlsx")
)

##----------------------------------------------------------
## Print for inspection
##----------------------------------------------------------

cat("\n=== CoRD — pooled across cities, by year ===\n\n")
print(table_pooled_year, n = Inf)

cat("\n=== CoRD — by city, period mean ===\n\n")
print(shares_city_pooled, n = Inf)

cat("\n=== CoRD — by city and year ===\n\n")
print(table_city_year, n = Inf)

message("Table saved to: ", out_tab)