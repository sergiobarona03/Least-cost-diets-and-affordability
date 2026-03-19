########################################################
## validate_cord_vs_cona_from_comp.R
## Build nutrient contributions from CoNA and CoRD
## using food composition (TCAC), then compare CoRD
## against CoNA requirements/contributions
########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(janitor)
  library(lubridate)
})

# ------------------------------------------------------------
# 0) Paths
# ------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\working-papers/working-paper-ipc/output/least_cost_metrics/real"

in_cona <- file.path(base_dir, "cona_comp_fullsample.rds")
in_cord <- file.path(base_dir, "cord_comp_fullsample.rds")
in_tcac <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\working-papers/working-paper-ipc/output/least_cost_metrics/real/tmp/tcac_master.rds"

out_dir <- file.path(base_dir, "validation_cord_vs_cona_from_comp")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1) Helpers
# ------------------------------------------------------------
norm_names <- function(x) {
  x %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
}

norm_food <- function(x) {
  x %>%
    as.character() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    toupper() %>%
    trimws() %>%
    stringr::str_replace_all("\\s+", " ")
}

norm_city <- function(x) {
  x %>%
    as.character() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    toupper() %>%
    trimws()
}

safe_sum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

first_non_na <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

# ------------------------------------------------------------
# 2) Load
# ------------------------------------------------------------
cona <- readRDS(in_cona)
cord <- readRDS(in_cord)
tcac <- readRDS(in_tcac)

names(cona) <- norm_names(names(cona))
names(cord) <- norm_names(names(cord))
names(tcac) <- norm_names(names(tcac))

# ------------------------------------------------------------
# 3) Standardize TCAC
# ------------------------------------------------------------
if ("articulo" %in% names(tcac) && !("food" %in% names(tcac))) {
  tcac <- tcac %>% dplyr::rename(food = articulo)
}

if (all(c("grupos_gabas", "subgrupos_gabas") %in% names(tcac)) &&
    !all(c("group", "subgroup") %in% names(tcac))) {
  tcac <- tcac %>%
    dplyr::rename(
      group = grupos_gabas,
      subgroup = subgrupos_gabas
    )
}

tcac <- tcac %>%
  dplyr::mutate(
    food = norm_food(food)
  )

candidate_nutrients <- c(
  "energia_kcal",
  "proteina_g", "protein_g",
  "grasa_g", "fat_g",
  "carbohidratos_g", "carbohydrate_g",
  "hierro_mg", "iron_mg",
  "zinc_mg",
  "calcio_mg", "calcium_mg",
  "vitamina_a_rae_ug", "vitamin_a_ug",
  "folato_ug", "folate_ug",
  "vitamina_b12_ug", "b12_ug",
  "vitamina_c_mg",
  "sodio_mg", "potasio_mg"
)

nutrient_cols <- intersect(candidate_nutrients, names(tcac))

if (!("gramos_g_1_intercambio_1_intercambio" %in% names(tcac))) {
  warning("TCAC does not contain gramos_g_1_intercambio_1_intercambio. CoRD may fail if it uses servings.")
}

if (length(nutrient_cols) == 0) {
  stop("No nutrient columns found in tcac_master. Inspect names(tcac).")
}

message("Nutrient columns found in TCAC:")
print(nutrient_cols)

tcac_food <- tcac %>%
  dplyr::group_by(food) %>%
  dplyr::summarise(
    gramos_g_1_intercambio_1_intercambio =
      if ("gramos_g_1_intercambio_1_intercambio" %in% names(tcac)) safe_mean(gramos_g_1_intercambio_1_intercambio) else NA_real_,
    across(all_of(nutrient_cols), safe_mean),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 4) Standardize CoNA
# ------------------------------------------------------------
if (!all(c("food", "quantity") %in% names(cona))) {
  stop("CoNA must contain at least columns 'Food' and 'quantity' (after name cleaning: food, quantity).")
}

cona_std <- cona %>%
  dplyr::mutate(
    food = norm_food(food),
    quantity = suppressWarnings(as.numeric(quantity))
  )

# ------------------------------------------------------------
# 5) Standardize CoRD
# ------------------------------------------------------------
if (!all(c("food", "number_serving") %in% names(cord))) {
  stop("CoRD must contain at least columns 'Food' and 'Number_Serving' (after name cleaning: food, number_serving).")
}

cord_std <- cord %>%
  dplyr::mutate(
    food = norm_food(food),
    number_serving = suppressWarnings(as.numeric(number_serving))
  )

# ------------------------------------------------------------
# 6) Join TCAC to CoNA and compute nutrient contributions
# ------------------------------------------------------------
cona_eval <- cona_std %>%
  dplyr::left_join(tcac_food, by = "food") %>%
  mutate(
    gramos_consumidos = quantity
  )

for (v in nutrient_cols) {
  cona_eval[[paste0(v, "_contrib")]] <- cona_eval$gramos_consumidos / 100 * cona_eval[[v]]
}

# ------------------------------------------------------------
# 7) Join TCAC to CoRD and compute nutrient contributions
# ------------------------------------------------------------
cord_eval <- cord_std %>%
  dplyr::left_join(tcac_food, by = "food") %>%
  mutate(
    gramos_consumidos = number_serving * gramos_g_1_intercambio_1_intercambio
  )

for (v in nutrient_cols) {
  cord_eval[[paste0(v, "_contrib")]] <- cord_eval$gramos_consumidos / 100 * cord_eval[[v]]
}

# ------------------------------------------------------------
# 8) Diagnostics on merge
# ------------------------------------------------------------
diag_cona_merge <- cona_eval %>%
  dplyr::summarise(
    n = n(),
    pct_food_match = 100 * mean(!is.na(gramos_consumidos) & rowSums(!is.na(across(all_of(nutrient_cols)))) > 0),
    pct_missing_quantity = 100 * mean(is.na(quantity))
  )

diag_cord_merge <- cord_eval %>%
  dplyr::summarise(
    n = n(),
    pct_food_match = 100 * mean(!is.na(gramos_consumidos) & rowSums(!is.na(across(all_of(nutrient_cols)))) > 0),
    pct_missing_servings = 100 * mean(is.na(number_serving)),
    pct_missing_serving_grams = 100 * mean(is.na(gramos_g_1_intercambio_1_intercambio))
  )

message("CoNA food-nutrition match (%): ", round(diag_cona_merge$pct_food_match, 2))
message("CoRD food-nutrition match (%): ", round(diag_cord_merge$pct_food_match, 2))
message("CoRD missing serving grams (%): ", round(diag_cord_merge$pct_missing_serving_grams, 2))

# ------------------------------------------------------------
# 9) Identify grouping columns
# ------------------------------------------------------------
possible_keys <- c("demo_group", "sex", "ciudad", "fecha", "escenario")
keys_cona <- intersect(possible_keys, names(cona_eval))
keys_cord <- intersect(possible_keys, names(cord_eval))
common_keys <- intersect(keys_cona, keys_cord)

if (length(common_keys) == 0) {
  stop("No common grouping keys found between CoNA and CoRD.")
}

message("Common keys detected before adjustment:")
print(common_keys)

# ------------------------------------------------------------
# 9.1) Exclude escenario from join keys
# ------------------------------------------------------------
common_keys <- setdiff(common_keys, "escenario")

if (length(common_keys) == 0) {
  stop("After removing 'escenario', no common grouping keys remain.")
}

message("Common keys used for comparison:")
print(common_keys)

# ------------------------------------------------------------
# 9.2) Harmonize key types and normalize city
# ------------------------------------------------------------
if ("ciudad" %in% names(cona_eval)) cona_eval$ciudad <- norm_city(cona_eval$ciudad)
if ("ciudad" %in% names(cord_eval)) cord_eval$ciudad <- norm_city(cord_eval$ciudad)

for (k in common_keys) {
  if (k == "fecha") {
    cona_eval[[k]] <- as.Date(cona_eval[[k]])
    cord_eval[[k]] <- as.Date(cord_eval[[k]])
  } else {
    cona_eval[[k]] <- as.character(cona_eval[[k]])
    cord_eval[[k]] <- as.character(cord_eval[[k]])
  }
}

cona_contrib_cols <- paste0(nutrient_cols, "_contrib")
cord_contrib_cols <- paste0(nutrient_cols, "_contrib")

# ------------------------------------------------------------
# 10) Aggregate nutrient contributions
# ------------------------------------------------------------
cona_tot <- cona_eval %>%
  dplyr::group_by(across(all_of(common_keys))) %>%
  dplyr::summarise(
    across(all_of(cona_contrib_cols), safe_sum),
    .groups = "drop"
  )

cord_tot <- cord_eval %>%
  dplyr::group_by(across(all_of(common_keys))) %>%
  dplyr::summarise(
    across(all_of(cord_contrib_cols), safe_sum),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 11) Long format and compare
# ------------------------------------------------------------
cona_long <- cona_tot %>%
  pivot_longer(
    cols = all_of(cona_contrib_cols),
    names_to = "nutriente",
    values_to = "aportado_cona"
  ) %>%
  mutate(nutriente = gsub("_contrib$", "", nutriente))

cord_long <- cord_tot %>%
  pivot_longer(
    cols = all_of(cord_contrib_cols),
    names_to = "nutriente",
    values_to = "aportado_cord"
  ) %>%
  mutate(nutriente = gsub("_contrib$", "", nutriente))

validation <- cord_long %>%
  left_join(cona_long, by = c(common_keys, "nutriente")) %>%
  dplyr::mutate(
    ratio_cord_vs_cona = aportado_cord / aportado_cona,
    gap_abs = aportado_cord - aportado_cona,
    cumple = case_when(
      is.na(aportado_cord) | is.na(aportado_cona) ~ NA,
      aportado_cord >= aportado_cona ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  dplyr::arrange(across(all_of(common_keys)), nutriente)

# ------------------------------------------------------------
# 12) Diagnostics after join
# ------------------------------------------------------------
diag_join <- validation %>%
  dplyr::summarise(
    n = n(),
    n_match = sum(!is.na(aportado_cord) & !is.na(aportado_cona)),
    pct_match = 100 * mean(!is.na(aportado_cord) & !is.na(aportado_cona))
  )

message("Validation rows: ", diag_join$n)
message("Rows with CoRD and CoNA matched (%): ", round(diag_join$pct_match, 2))

# ------------------------------------------------------------
# 13) Summaries
# ------------------------------------------------------------
summary_nutrient <- validation %>%
  dplyr::group_by(nutriente) %>%
  dplyr::summarise(
    n = n(),
    n_nonmissing = sum(!is.na(aportado_cord) & !is.na(aportado_cona)),
    pct_nonmissing = 100 * mean(!is.na(aportado_cord) & !is.na(aportado_cona)),
    mean_aportado_cord = safe_mean(aportado_cord),
    mean_aportado_cona = safe_mean(aportado_cona),
    mean_ratio = safe_mean(ratio_cord_vs_cona),
    pct_cumple = 100 * mean(cumple, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(pct_cumple, mean_ratio)

summary_keys_nutrient <- validation %>%
  dplyr::group_by(across(all_of(common_keys)), nutriente) %>%
  dplyr::summarise(
    aportado_cord = safe_mean(aportado_cord),
    aportado_cona = safe_mean(aportado_cona),
    ratio_cord_vs_cona = safe_mean(ratio_cord_vs_cona),
    cumple = ifelse(all(is.na(cumple)), NA, all(cumple, na.rm = TRUE)),
    .groups = "drop"
  )

summary_bundle <- validation %>%
  dplyr::group_by(across(all_of(common_keys))) %>%
  dplyr::summarise(
    n_nutrientes = sum(!is.na(cumple)),
    n_cumple = sum(cumple, na.rm = TRUE),
    pct_cumple = ifelse(n_nutrientes > 0, 100 * n_cumple / n_nutrientes, NA_real_),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 14) Save outputs
# ------------------------------------------------------------
write_csv(cona_eval, file.path(out_dir, "cona_eval_with_nutrients.csv"))
write_csv(cord_eval, file.path(out_dir, "cord_eval_with_nutrients.csv"))
write_csv(validation, file.path(out_dir, "cord_vs_cona_validation_long.csv"))
write_csv(summary_nutrient, file.path(out_dir, "cord_vs_cona_summary_by_nutrient.csv"))
write_csv(summary_keys_nutrient, file.path(out_dir, "cord_vs_cona_summary_by_keys_nutrient.csv"))
write_csv(summary_bundle, file.path(out_dir, "cord_vs_cona_summary_bundle.csv"))

saveRDS(cona_eval, file.path(out_dir, "cona_eval_with_nutrients.rds"))
saveRDS(cord_eval, file.path(out_dir, "cord_eval_with_nutrients.rds"))
saveRDS(validation, file.path(out_dir, "cord_vs_cona_validation_long.rds"))
saveRDS(summary_nutrient, file.path(out_dir, "cord_vs_cona_summary_by_nutrient.rds"))
saveRDS(summary_keys_nutrient, file.path(out_dir, "cord_vs_cona_summary_by_keys_nutrient.rds"))
saveRDS(summary_bundle, file.path(out_dir, "cord_vs_cona_summary_bundle.rds"))

write_xlsx(
  list(
    cona_eval_with_nutrients = cona_eval,
    cord_eval_with_nutrients = cord_eval,
    validation_long = validation,
    summary_nutrient = summary_nutrient,
    summary_keys_nutrient = summary_keys_nutrient,
    summary_bundle = summary_bundle
  ),
  file.path(out_dir, "cord_vs_cona_validation_outputs.xlsx")
)

message("Done. Outputs saved in: ", out_dir)