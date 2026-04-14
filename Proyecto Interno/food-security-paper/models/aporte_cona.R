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
# Directorios base
# ------------------------------------------------------------
dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

# ------------------------------------------------------------
# 1) Rutas
# ------------------------------------------------------------
in_cona <- file.path(
  base_dir,
  "food-security-paper", "output", "cona",
  "230326_cona_full.xlsx"
)

in_tcac <- file.path(
  base_dir,
  "food-security-paper", "output", "tcac_food_table",
  "tcac_master.rds"
)

out_dir <- file.path(
  base_dir,
  "food-security-paper", "output", "cona",
  "nutrient_contribution"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2) Funciones auxiliares
# ------------------------------------------------------------
norm_food <- function(x) {
  x %>%
    as.character() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    toupper() %>%
    trimws() %>%
    str_replace_all("\\s+", " ")
}

norm_names <- function(x) {
  x %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

map_demo_group_to_age <- function(x) {
  case_when(
    x == "[1,4)"      ~ "1 a 3 años",
    x == "[4,9)"      ~ "4 a 8 años",
    x == "[9,14)"     ~ "9 a 13 años",
    x == "[10, 14)"   ~ "9 a 13 años",
    x == "[10,14)"    ~ "9 a 13 años",
    x == "[14,19)"    ~ "14 a 18 años",
    x == "[19,31)"    ~ "19 a 30 años",
    x == "[31,51)"    ~ "31 a 50 años",
    x == "[51,71)"    ~ "51 a 70 años",
    x == "[71, Inf)"  ~ ">70 años",
    x == "[71,Inf)"   ~ ">70 años",
    TRUE ~ NA_character_
  )
}

load_foodpricer_obj <- function(obj_name) {
  if (exists(obj_name, envir = .GlobalEnv)) return(invisible(TRUE))
  ok <- tryCatch({
    data(list = obj_name, package = "FoodpriceR", envir = .GlobalEnv)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) warning("No se pudo cargar el objeto de FoodpriceR: ", obj_name)
  invisible(ok)
}

build_req_long <- function(obj, value_name, req_to_tcac) {
  req_tbl <- as_tibble(obj)
  names(req_tbl) <- norm_names(names(req_tbl))
  
  if (!all(c("age", "sex") %in% names(req_tbl))) {
    stop("El objeto de requerimientos debe contener las columnas Age y Sex.")
  }
  
  req_tbl %>%
    pivot_longer(
      cols = -c(age, sex),
      names_to = "req_nutrient",
      values_to = value_name
    ) %>%
    mutate(
      req_nutrient_raw = case_when(
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
      nutriente = unname(req_to_tcac[req_nutrient_raw]),
      age = as.character(age),
      sex = as.character(sex)
    ) %>%
    filter(!is.na(nutriente)) %>%
    select(age, sex, nutriente, all_of(value_name))
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

req_name_map <- c(
  energia_kcal            = "Energy",
  proteina_g              = "Protein",
  lipidos_g               = "Lipids",
  carbohidratos_totales_g = "Carbohydrates",
  vitamina_c_mg           = "VitaminC",
  folatos_mcg             = "Folate",
  vitamina_a_er           = "VitaminA",
  tiamina_mg              = "Thiamine",
  riboflavina_mg          = "Riboflavin",
  niacina_mg              = "Niacin",
  vitamina_b12_mcg        = "VitaminB12",
  magnesio_mg             = "Magnesium",
  fosforo_mg              = "Phosphorus",
  sodio_mg                = "Sodium",
  calcio_mg               = "Calcium",
  hierro_mg               = "Iron",
  zinc_mg                 = "Zinc"
)

req_to_tcac <- setNames(names(req_name_map), req_name_map)

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

if (length(nutrient_cols) == 0) {
  stop("No se encontraron nutrientes en tcac_master")
}

tcac_food <- tcac %>%
  dplyr::group_by(food) %>%
  dplyr::summarise(
    across(all_of(nutrient_cols), safe_mean),
    .groups = "drop"
  )

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
# 7) Totales dieta
# ------------------------------------------------------------
diet_totals <- cona_eval %>%
  dplyr::group_by(across(all_of(group_keys))) %>%
  dplyr::summarise(
    across(all_of(aporte_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 8) Aporte dentro de dieta
# ------------------------------------------------------------
food_contrib <- cona_eval %>%
  dplyr::select(all_of(group_keys), food, quantity, all_of(aporte_cols)) %>%
  pivot_longer(
    cols = all_of(aporte_cols),
    names_to = "nutriente",
    values_to = "aporte_absoluto"
  ) %>%
  mutate(
    nutriente = sub("_aporte$", "", nutriente)
  )

diet_totals_long <- diet_totals %>%
  pivot_longer(
    cols = all_of(aporte_cols),
    names_to = "nutriente",
    values_to = "total_nutriente_dieta"
  ) %>%
  mutate(
    nutriente = sub("_aporte$", "", nutriente)
  )

food_contrib <- food_contrib %>%
  left_join(
    diet_totals_long,
    by = c("demo_group", "sex", "ciudad", "fecha", "nutriente")
  ) %>%
  dplyr::mutate(
    pct_aporte_dieta = if_else(
      total_nutriente_dieta > 0,
      100 * aporte_absoluto / total_nutriente_dieta,
      NA_real_
    )
  )

# ------------------------------------------------------------
# 9) Cargar EER, EER_LL y UL
# ------------------------------------------------------------
invisible(load_foodpricer_obj("EER"))
invisible(load_foodpricer_obj("EER_LL"))
invisible(load_foodpricer_obj("UL"))

req_eer <- build_req_long(
  get("EER", envir = .GlobalEnv),
  "eer",
  req_to_tcac
)

req_eer_ll <- build_req_long(
  get("EER_LL", envir = .GlobalEnv),
  "eer_ll",
  req_to_tcac
)

req_ul <- build_req_long(
  get("UL", envir = .GlobalEnv),
  "ul",
  req_to_tcac
)

req_bounds <- req_eer %>%
  full_join(req_eer_ll, by = c("age", "sex", "nutriente")) %>%
  full_join(req_ul, by = c("age", "sex", "nutriente"))

# ------------------------------------------------------------
# 10) Estandarizar sexo para el cruce
# ------------------------------------------------------------
food_contrib_validated_base <- food_contrib %>%
  dplyr::mutate(
    sex_original = as.character(sex),
    age = map_demo_group_to_age(as.character(demo_group))
  )

sex_req_values <- sort(unique(req_bounds$sex))
print(sex_req_values)

# Intento de homologación básica
food_contrib_validated_base <- food_contrib_validated_base %>%
  mutate(
    sex_req = case_when(
      sex_original %in% c("0", "M", "Male", "Hombre", "Masculino", "masculino") &
        any(sex_req_values %in% c("0", "M", "Male", "Hombre", "Masculino", "masculino")) ~
        sex_req_values[sex_req_values %in% c("0", "M", "Male", "Hombre", "Masculino", "masculino")][1],
      
      sex_original %in% c("1", "F", "Female", "Mujer", "Femenino", "femenino") &
        any(sex_req_values %in% c("1", "F", "Female", "Mujer", "Femenino", "femenino")) ~
        sex_req_values[sex_req_values %in% c("1", "F", "Female", "Mujer", "Femenino", "femenino")][1],
      
      sex_original %in% sex_req_values ~ sex_original,
      TRUE ~ NA_character_
    )
  )

food_contrib_validated <- food_contrib_validated_base %>%
  left_join(
    req_bounds,
    by = c("age", "sex_req" = "sex", "nutriente")
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
  dplyr::group_by(demo_group, sex, nutriente) %>%
  slice_max(order_by = pct_promedio_dieta, n = 10, with_ties = FALSE) %>%
  ungroup()

text_summary <- top_foods_demo %>%
  mutate(
    resumen = paste0(
      food, " aporta ",
      round(pct_promedio_dieta, 2),
      "% del total de ", nutriente
    )
  )

# ------------------------------------------------------------
# 12) Diagnóstico rápido
# ------------------------------------------------------------
diag_match <- food_contrib_validated %>%
  dplyr::summarise(
    filas_totales = n(),
    filas_con_age = sum(!is.na(age)),
    filas_con_eer = sum(!is.na(eer)),
    filas_con_eer_ll = sum(!is.na(eer_ll)),
    filas_con_ul = sum(!is.na(ul))
  )

print(diag_match)

# ------------------------------------------------------------
# 13) Guardar
# ------------------------------------------------------------
saveRDS(food_contrib, file.path(out_dir, "food_contribution.rds"))
saveRDS(food_contrib_validated, file.path(out_dir, "food_contribution_validated.rds"))
saveRDS(top_foods_demo, file.path(out_dir, "top_foods.rds"))

write_xlsx(
  list(
    food_contribution = food_contrib,
    food_contribution_validated = food_contrib_validated,
    top_foods = top_foods_demo,
    resumen = text_summary,
    diagnostico_match = diag_match
  ),
  path = file.path(out_dir, "outputs.xlsx")
)

message("Listo.")