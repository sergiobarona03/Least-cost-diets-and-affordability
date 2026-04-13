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
# 2) Función normalizar nombres
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
  "energia_kcal","proteina_g","lipidos_g","carbohidratos_totales_g",
  "vitamina_c_mg","folatos_mcg","vitamina_a_er","tiamina_mg",
  "riboflavina_mg","niacina_mg","vitamina_b12_mcg","magnesio_mg",
  "fosforo_mg","sodio_mg","calcio_mg","hierro_mg","zinc_mg"
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

if (length(nutrient_cols) == 0) {
  stop("No se encontraron nutrientes en tcac_master")
}

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
# 7) Totales dieta
# ------------------------------------------------------------
diet_totals <- cona_eval %>%
  dplyr::group_by(across(all_of(group_keys))) %>%
  dplyr::summarise(across(all_of(aporte_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::mutate(nutriente = sub("_aporte$", "", nutriente))

diet_totals_long <- diet_totals %>%
  pivot_longer(
    cols = all_of(aporte_cols),
    names_to = "nutriente",
    values_to = "total_nutriente_dieta"
  ) %>%
  dplyr::mutate(nutriente = sub("_aporte$", "", nutriente))

food_contrib <- food_contrib %>%
  left_join(diet_totals_long,
            by = c("demo_group", "sex", "ciudad", "fecha", "nutriente")) %>%
  mutate(
    pct_aporte_dieta = if_else(
      total_nutriente_dieta > 0,
      100 * aporte_absoluto / total_nutriente_dieta,
      NA_real_
    )
  )

# ------------------------------------------------------------
# 9) Resumen
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
# 10) Guardar
# ------------------------------------------------------------
saveRDS(food_contrib, file.path(out_dir, "food_contribution.rds"))
saveRDS(top_foods_demo, file.path(out_dir, "top_foods.rds"))

write_xlsx(
  list(
    food_contribution = food_contrib,
    top_foods = top_foods_demo,
    resumen = text_summary
  ),
  path = file.path(out_dir, "outputs.xlsx")
)

message("Listo.")