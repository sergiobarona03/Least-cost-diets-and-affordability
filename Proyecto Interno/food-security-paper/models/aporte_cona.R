########################################################
## Aporte nutricional de cada alimento en CoNA
## por grupo demográfico
########################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(writexl)
  library(janitor)
  library(stringr)
  library(FoodpriceR)
  library(tibble)
})

# ------------------------------------------------------------
# 1) Directorios y rutas
# ------------------------------------------------------------
dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]
if (is.na(base_dir)) stop("Ninguno de los directorios existe")

in_cona <- file.path(base_dir, "food-security-paper", "output", "cona", "230326_cona_full.xlsx")
in_tcac <- file.path(base_dir, "food-security-paper", "output", "tcac_food_table", "tcac_master.rds")
out_dir <- file.path(base_dir, "food-security-paper", "output", "cona", "nutrient_contribution")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2) Función mínima
# ------------------------------------------------------------
norm_food <- function(x) {
  x %>%
    as.character() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    toupper() %>%
    trimws() %>%
    str_replace_all("\\s+", " ")
}

# ------------------------------------------------------------
# 3) Nutrientes
# ------------------------------------------------------------
tcac_nutrients <- c(
  "energia_kcal", "proteina_g", "lipidos_g", "carbohidratos_totales_g",
  "vitamina_c_mg", "folatos_mcg", "vitamina_a_er", "tiamina_mg",
  "riboflavina_mg", "niacina_mg", "vitamina_b12_mcg", "magnesio_mg",
  "fosforo_mg", "sodio_mg", "calcio_mg", "hierro_mg", "zinc_mg"
)

req_to_tcac <- c(
  Energy = "energia_kcal",
  Protein = "proteina_g",
  Lipids = "lipidos_g",
  Carbohydrates = "carbohidratos_totales_g",
  VitaminC = "vitamina_c_mg",
  Folate = "folatos_mcg",
  VitaminA = "vitamina_a_er",
  Thiamine = "tiamina_mg",
  Riboflavin = "riboflavina_mg",
  Niacin = "niacina_mg",
  VitaminB12 = "vitamina_b12_mcg",
  Magnesium = "magnesio_mg",
  Phosphorus = "fosforo_mg",
  Sodium = "sodio_mg",
  Calcium = "calcio_mg",
  Iron = "hierro_mg",
  Zinc = "zinc_mg"
)

# ------------------------------------------------------------
# 4) Leer datos
# ------------------------------------------------------------
cona <- read_excel(in_cona, sheet = "comp") %>% clean_names()
tcac <- readRDS(in_tcac) %>% clean_names()

if ("articulo" %in% names(tcac) && !("food" %in% names(tcac))) {
  tcac <- tcac %>% dplyr::rename(food = articulo)
}

# ------------------------------------------------------------
# 5) Preparar bases
# ------------------------------------------------------------
cona <- cona %>%
  mutate(
    food = norm_food(food),
    quantity = as.numeric(quantity),
    fecha = as.Date(fecha)
  )

tcac <- tcac %>%
  mutate(food = norm_food(food))

nutrient_cols <- intersect(tcac_nutrients, names(tcac))
if (length(nutrient_cols) == 0) stop("No se encontraron nutrientes en tcac_master")

tcac_food <- tcac %>%
  dplyr::group_by(food) %>%
  dplyr::summarise(across(all_of(nutrient_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# ------------------------------------------------------------
# 6) Unión y aportes
# ------------------------------------------------------------
cona_eval <- cona %>%
  left_join(tcac_food, by = "food")

for (v in nutrient_cols) {
  cona_eval[[paste0(v, "_aporte")]] <- (cona_eval$quantity / 100) * cona_eval[[v]]
}

aporte_cols <- paste0(nutrient_cols, "_aporte")
group_keys <- c("demo_group", "sex", "ciudad", "fecha")

# ------------------------------------------------------------
# 7) Totales por dieta
# ------------------------------------------------------------
diet_totals <- cona_eval %>%
  dplyr::group_by(across(all_of(group_keys))) %>%
  dplyr::summarise(across(all_of(aporte_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

food_contrib <- cona_eval %>%
  dplyr::select(all_of(group_keys), food, quantity, all_of(aporte_cols)) %>%
  pivot_longer(
    cols = all_of(aporte_cols),
    names_to = "nutriente",
    values_to = "aporte_absoluto"
  ) %>%
  mutate(nutriente = sub("_aporte$", "", nutriente))

diet_totals_long <- diet_totals %>%
  pivot_longer(
    cols = all_of(aporte_cols),
    names_to = "nutriente",
    values_to = "total_nutriente_dieta"
  ) %>%
  mutate(nutriente = sub("_aporte$", "", nutriente))

food_contrib <- food_contrib %>%
  left_join(
    diet_totals_long,
    by = c("demo_group", "sex", "ciudad", "fecha", "nutriente")
  ) %>%
  mutate(
    pct_aporte_dieta = if_else(
      total_nutriente_dieta > 0,
      100 * aporte_absoluto / total_nutriente_dieta,
      NA_real_
    )
  )

# ------------------------------------------------------------
# 8) Cargar EER, EER_LL y UL
# ------------------------------------------------------------
data("EER", package = "FoodpriceR", envir = environment())
data("EER_LL", package = "FoodpriceR", envir = environment())
data("UL", package = "FoodpriceR", envir = environment())

# ------------------------------------------------------------
# 9) Pasar requerimientos a formato largo
# ------------------------------------------------------------
req_eer <- as_tibble(EER)
req_eer_ll <- as_tibble(EER_LL)
req_ul <- as_tibble(UL)

names(req_eer) <- janitor::make_clean_names(names(req_eer))
names(req_eer_ll) <- janitor::make_clean_names(names(req_eer_ll))
names(req_ul) <- janitor::make_clean_names(names(req_ul))

req_eer <- req_eer %>%
  pivot_longer(-c(age, sex), names_to = "req_nutrient", values_to = "eer") %>%
  mutate(
    req_nutrient = case_when(
      req_nutrient == "energy" ~ "Energy",
      req_nutrient == "protein" ~ "Protein",
      req_nutrient == "lipids" ~ "Lipids",
      req_nutrient == "carbohydrates" ~ "Carbohydrates",
      req_nutrient == "vitaminc" ~ "VitaminC",
      req_nutrient == "folate" ~ "Folate",
      req_nutrient == "vitamina" ~ "VitaminA",
      req_nutrient == "thiamine" ~ "Thiamine",
      req_nutrient == "riboflavin" ~ "Riboflavin",
      req_nutrient == "niacin" ~ "Niacin",
      req_nutrient == "vitaminb12" ~ "VitaminB12",
      req_nutrient == "magnesium" ~ "Magnesium",
      req_nutrient == "phosphorus" ~ "Phosphorus",
      req_nutrient == "sodium" ~ "Sodium",
      req_nutrient == "calcium" ~ "Calcium",
      req_nutrient == "iron" ~ "Iron",
      req_nutrient == "zinc" ~ "Zinc",
      TRUE ~ NA_character_
    ),
    nutriente = req_to_tcac[req_nutrient],
    age = as.character(age),
    sex = as.character(sex)
  ) %>%
  filter(!is.na(nutriente)) %>%
  select(age, sex, nutriente, eer)

req_eer_ll <- req_eer_ll %>%
  pivot_longer(-c(age, sex), names_to = "req_nutrient", values_to = "eer_ll") %>%
  mutate(
    req_nutrient = case_when(
      req_nutrient == "energy" ~ "Energy",
      req_nutrient == "protein" ~ "Protein",
      req_nutrient == "lipids" ~ "Lipids",
      req_nutrient == "carbohydrates" ~ "Carbohydrates",
      req_nutrient == "vitaminc" ~ "VitaminC",
      req_nutrient == "folate" ~ "Folate",
      req_nutrient == "vitamina" ~ "VitaminA",
      req_nutrient == "thiamine" ~ "Thiamine",
      req_nutrient == "riboflavin" ~ "Riboflavin",
      req_nutrient == "niacin" ~ "Niacin",
      req_nutrient == "vitaminb12" ~ "VitaminB12",
      req_nutrient == "magnesium" ~ "Magnesium",
      req_nutrient == "phosphorus" ~ "Phosphorus",
      req_nutrient == "sodium" ~ "Sodium",
      req_nutrient == "calcium" ~ "Calcium",
      req_nutrient == "iron" ~ "Iron",
      req_nutrient == "zinc" ~ "Zinc",
      TRUE ~ NA_character_
    ),
    nutriente = req_to_tcac[req_nutrient],
    age = as.character(age),
    sex = as.character(sex)
  ) %>%
  filter(!is.na(nutriente)) %>%
  select(age, sex, nutriente, eer_ll)

req_ul <- req_ul %>%
  pivot_longer(-c(age, sex), names_to = "req_nutrient", values_to = "ul") %>%
  mutate(
    req_nutrient = case_when(
      req_nutrient == "energy" ~ "Energy",
      req_nutrient == "protein" ~ "Protein",
      req_nutrient == "lipids" ~ "Lipids",
      req_nutrient == "carbohydrates" ~ "Carbohydrates",
      req_nutrient == "vitaminc" ~ "VitaminC",
      req_nutrient == "folate" ~ "Folate",
      req_nutrient == "vitamina" ~ "VitaminA",
      req_nutrient == "thiamine" ~ "Thiamine",
      req_nutrient == "riboflavin" ~ "Riboflavin",
      req_nutrient == "niacin" ~ "Niacin",
      req_nutrient == "vitaminb12" ~ "VitaminB12",
      req_nutrient == "magnesium" ~ "Magnesium",
      req_nutrient == "phosphorus" ~ "Phosphorus",
      req_nutrient == "sodium" ~ "Sodium",
      req_nutrient == "calcium" ~ "Calcium",
      req_nutrient == "iron" ~ "Iron",
      req_nutrient == "zinc" ~ "Zinc",
      TRUE ~ NA_character_
    ),
    nutriente = req_to_tcac[req_nutrient],
    age = as.character(age),
    sex = as.character(sex)
  ) %>%
  filter(!is.na(nutriente)) %>%
  select(age, sex, nutriente, ul)

req_bounds <- req_eer %>%
  full_join(req_eer_ll, by = c("age", "sex", "nutriente")) %>%
  full_join(req_ul, by = c("age", "sex", "nutriente"))

# ------------------------------------------------------------
# 10) Unir requerimientos a food_contrib
# ------------------------------------------------------------
food_contrib_validated <- food_contrib %>%
  mutate(
    age = case_when(
      demo_group == "[1,4)" ~ "1 a 3 años",
      demo_group == "[4,9)" ~ "4 a 8 años",
      demo_group %in% c("[9,14)", "[10,14)", "[10, 14)") ~ "9 a 13 años",
      demo_group == "[14,19)" ~ "14 a 18 años",
      demo_group == "[19,31)" ~ "19 a 30 años",
      demo_group == "[31,51)" ~ "31 a 50 años",
      demo_group == "[51,71)" ~ "51 a 70 años",
      demo_group %in% c("[71,Inf)", "[71, Inf)") ~ ">70 años",
      TRUE ~ NA_character_
    ),
    sex = as.character(sex)
  ) %>%
  left_join(
    req_bounds,
    by = c("age", "sex", "nutriente")
  )

# ------------------------------------------------------------
# 11) Resumen
# ------------------------------------------------------------
demo_summary <- food_contrib %>%
  dplyr::group_by(demo_group, sex, nutriente, food) %>%
  dplyr::summarise(
    aporte_promedio = mean(aporte_absoluto, na.rm = TRUE),
    pct_promedio_dieta = mean(pct_aporte_dieta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(demo_group, sex, nutriente, desc(pct_promedio_dieta))

top_foods_demo <- demo_summary %>%
  group_by(demo_group, sex, nutriente) %>%
  slice_max(order_by = pct_promedio_dieta, n = 10, with_ties = FALSE) %>%
  ungroup()

text_summary <- top_foods_demo %>%
  mutate(
    resumen = paste0(food, " aporta ", round(pct_promedio_dieta, 2), "% del total de ", nutriente)
  )

# ------------------------------------------------------------
# 12) Guardar
# ------------------------------------------------------------
saveRDS(food_contrib, file.path(out_dir, "food_contribution.rds"))
saveRDS(food_contrib_validated, file.path(out_dir, "food_contribution_validated.rds"))
saveRDS(top_foods_demo, file.path(out_dir, "top_foods.rds"))

write_xlsx(
  list(
    food_contribution = food_contrib,
    food_contribution_validated = food_contrib_validated,
    top_foods = top_foods_demo,
    resumen = text_summary
  ),
  path = file.path(out_dir, "outputs.xlsx")
)

message("Listo.")