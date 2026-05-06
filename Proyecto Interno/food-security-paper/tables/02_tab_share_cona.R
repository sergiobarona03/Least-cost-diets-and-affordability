# =========================================================
# Table: Share of energy and nutrients in CoNA least-cost diets
# by GABAS food group
# =========================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(openxlsx)

# =========================
# 1. PATHS
# =========================

path_cona <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/cona/230326_cona_full.xlsx"

path_tcac <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/tcac_food_table/tcac_master.rds"

out_path <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/cona"

# =========================
# 2. CLEAN FUNCTION
# =========================

clean_text <- function(x){
  x %>%
    as.character() %>%
    str_to_upper() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# =========================
# 3. READ CONA FILE
# =========================

comp <- read_excel(path_cona, sheet = "comp") %>%
  clean_names() %>%
  mutate(
    fecha = as.Date(fecha),
    ciudad = clean_text(ciudad),
    food_clean = clean_text(food)
  )

limit <- read_excel(path_cona, sheet = "limit") %>%
  clean_names() %>%
  mutate(
    fecha = as.Date(fecha),
    ciudad = clean_text(ciudad)
  )

# =========================
# 4. READ TCAC MASTER
# =========================

tcac <- readRDS(path_tcac) %>%
  clean_names() %>%
  mutate(
    articulo_clean = clean_text(articulo),
    grupos_gabas = as.character(grupos_gabas)
  )

# =========================
# 5. STANDARDIZE NUTRIENT COLUMNS
# =========================

tcac_nutrients <- tcac %>%
  transmute(
    food_clean = articulo_clean,
    food_group = case_when(
      grupos_gabas == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS" ~ "Cereals, roots, tubers, and bananas",
      grupos_gabas == "FRUTAS Y VERDURAS" ~ "Fruits and vegetables",
      grupos_gabas == "LECHE Y PRODUCTOS LACTEOS" ~ "Milk and dairy products",
      grupos_gabas == "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS" ~ "Meats, eggs, legumes, nuts, and seeds",
      grupos_gabas == "GRASAS" ~ "Fats",
      grupos_gabas == "AZUCARES" ~ "Sugars",
      grupos_gabas %in% c("0", "SIN CATEGORIA") ~ "Others",
      TRUE ~ "Others"
    ),
    Energy = energia_kcal,
    Protein = proteina_g,
    Carbohydrate = carbohidratos_totales_g,
    Lipids = lipidos_g,
    Calcium = calcio_mg,
    Iron = hierro_mg,
    Magnesium = magnesio_mg,
    Phosphorus = fosforo_mg,
    Zinc = zinc_mg,
    Sodium = sodio_mg,
    Vitamin_C = vitamina_c_mg,
    Folate = folatos_mcg,
    Vitamin_A = vitamina_a_er,
    Thiamine = tiamina_mg,
    Riboflavin = riboflavina_mg,
    Niacin = niacina_mg
  ) %>%
  distinct(food_clean, .keep_all = TRUE)

# =========================
# 6. MERGE CONA COMPOSITION WITH TCAC NUTRIENTS
# =========================

base_nutrients <- comp %>%
  left_join(tcac_nutrients, by = "food_clean")

# =========================
# 7. CHECK UNMATCHED FOODS
# =========================

unmatched_foods <- base_nutrients %>%
  filter(is.na(food_group)) %>%
  distinct(food, food_clean) %>%
  arrange(food)

write.xlsx(
  unmatched_foods,
  file.path(out_path, "unmatched_foods_for_nutrient_share_table.xlsx"),
  overwrite = TRUE
)

# =========================
# 8. COMPUTE NUTRIENT CONTRIBUTIONS
# =========================

nutrient_cols <- c(
  "Energy", "Protein", "Carbohydrate", "Lipids",
  "Calcium", "Iron", "Magnesium", "Phosphorus", "Zinc", "Sodium",
  "Vitamin_C", "Folate", "Vitamin_A", "Thiamine", "Riboflavin", "Niacin"
)

long_contrib <- base_nutrients %>%
  dplyr::filter(!is.na(food_group)) %>%
  mutate(across(all_of(nutrient_cols), ~ quantity * .x / 100)) %>%
  dplyr::select(ciudad, fecha, demo_group, sex, food_group, all_of(nutrient_cols)) %>%
  pivot_longer(
    cols = all_of(nutrient_cols),
    names_to = "nutrient",
    values_to = "nutrient_amount"
  ) %>%
  dplyr::filter(!is.na(nutrient_amount))

# =========================
# 9. SHARE BY FOOD GROUP
# =========================

share_table_long <- long_contrib %>%
  dplyr::group_by(ciudad, fecha, demo_group, sex, nutrient, food_group) %>%
  dplyr::summarise(
    amount_group = sum(nutrient_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(ciudad, fecha, demo_group, sex, nutrient) %>%
  mutate(
    total_nutrient = sum(amount_group, na.rm = TRUE),
    share = 100 * amount_group / total_nutrient
  ) %>%
  ungroup() %>%
  filter(!is.na(share), is.finite(share))

# =========================
# 10. AVERAGE OVER CITY, MONTH, AGE, AND SEX
# =========================

table_share <- share_table_long %>%
  dplyr::group_by(nutrient, food_group) %>%
  dplyr::summarise(
    share = mean(share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(share = round(share, 1)) %>%
  pivot_wider(
    names_from = food_group,
    values_from = share,
    values_fill = 0
  )

# =========================
# 11. ORDER ROWS AND COLUMNS
# =========================

nutrient_order <- c(
  "Energy", "Protein", "Carbohydrate", "Lipids",
  "Calcium", "Iron", "Magnesium", "Phosphorus", "Zinc", "Sodium",
  "Vitamin_C", "Folate", "Vitamin_A", "Thiamine", "Riboflavin", "Niacin"
)

nutrient_labels <- c(
  "Energy" = "Energy",
  "Protein" = "Protein",
  "Carbohydrate" = "Carbohydrate",
  "Lipids" = "Lipids",
  "Calcium" = "Calcium",
  "Iron" = "Iron",
  "Magnesium" = "Magnesium",
  "Phosphorus" = "Phosphorus",
  "Zinc" = "Zinc",
  "Sodium" = "Sodium",
  "Vitamin_C" = "Vitamin C",
  "Folate" = "Folate",
  "Vitamin_A" = "Vitamin A",
  "Thiamine" = "Thiamine",
  "Riboflavin" = "Riboflavin",
  "Niacin" = "Niacin"
)

group_order <- c(
  "Cereals, roots, tubers, and bananas",
  "Fruits and vegetables",
  "Milk and dairy products",
  "Meats, eggs, legumes, nuts, and seeds",
  "Fats",
  "Sugars",
  "Others"
)

table_share <- table_share %>%
  mutate(
    nutrient_raw = nutrient,
    nutrient = factor(nutrient_raw, levels = nutrient_order)
  ) %>%
  arrange(nutrient) %>%
  mutate(
    nutrient = recode(as.character(nutrient_raw), !!!nutrient_labels)
  ) %>%
  select(
    nutrient,
    any_of(group_order)
  )
# =========================
# 12. EXPORT TO EXCEL
# =========================

write.xlsx(
  table_share,
  file.path(out_path, "table_share_energy_nutrients_by_gabas_group.xlsx"),
  overwrite = TRUE
)
