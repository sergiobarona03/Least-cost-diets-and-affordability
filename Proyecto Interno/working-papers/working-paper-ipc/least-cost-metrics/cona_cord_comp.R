########################################################
## validate_comp_energy_and_nutrients.R
## Build nutrient contributions from CoNA and CoRD
## using food composition (TCAC), then validate:
##   (1) energy by demographic group against EER_LL and UL
##   (2) each nutrient against lower/upper requirement bounds
########################################################


suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(janitor)
  library(lubridate)
  library(FoodpriceR)
})

# ------------------------------------------------------------
# 0) Paths
# ------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\working-papers/working-paper-ipc/output/least_cost_metrics"

in_cona <- file.path(base_dir, "cona_comp_fullsample.rds")
in_cord <- file.path(base_dir, "cord_comp_fullsample.rds")
in_tcac <- file.path(base_dir, "tmp", "tcac_master.rds")

out_dir <- file.path(base_dir, "validation_comp_energy_and_nutrients")
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

load_foodpricer_obj <- function(obj_name) {
  if (exists(obj_name, envir = .GlobalEnv)) return(invisible(TRUE))
  ok <- tryCatch({
    data(list = obj_name, package = "FoodpriceR", envir = .GlobalEnv)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) warning("Could not load FoodpriceR object: ", obj_name)
  invisible(ok)
}

invisible(load_foodpricer_obj("EER"))
invisible(load_foodpricer_obj("EER_LL"))
invisible(load_foodpricer_obj("UL"))

# ------------------------------------------------------------
# 2) Nutrient names
# ------------------------------------------------------------
tcac_nutrients <- c(
  "energia_kcal",
  "proteina_g",
  "lipidos_g",
  "carbohidratos_totales_g",
  "vitamina_c_mg",
  "folatos_mcg",
  "vitamina_a_er",
  "tiamina_mg",
  "riboflavina_mg",
  "niacina_mg",
  "vitamina_b12_mcg",
  "magnesio_mg",
  "fosforo_mg",
  "sodio_mg",
  "calcio_mg",
  "hierro_mg",
  "zinc_mg"
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
# 3) Load data
# ------------------------------------------------------------
cona <- readRDS(in_cona)
cord <- readRDS(in_cord)
tcac <- readRDS(in_tcac)

names(cona) <- norm_names(names(cona))
names(cord) <- norm_names(names(cord))
names(tcac) <- norm_names(names(tcac))

# ------------------------------------------------------------
# 4) Standardize TCAC
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
  dplyr::mutate(food = norm_food(food))

nutrient_cols <- intersect(tcac_nutrients, names(tcac))

if (!("gramos_g_1_intercambio_1_intercambio" %in% names(tcac))) {
  warning("TCAC does not contain gramos_g_1_intercambio_1_intercambio.")
}

if (length(nutrient_cols) == 0) {
  stop("No nutrient columns found in tcac_master.")
}

tcac_food <- tcac %>%
  dplyr::group_by(food) %>%
  dplyr::summarise(
    gramos_g_1_intercambio_1_intercambio =
      if ("gramos_g_1_intercambio_1_intercambio" %in% names(tcac)) safe_mean(gramos_g_1_intercambio_1_intercambio) else NA_real_,
    across(all_of(nutrient_cols), safe_mean),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 5) Standardize CoNA / CoRD
# ------------------------------------------------------------
if (!all(c("food", "quantity") %in% names(cona))) {
  stop("CoNA must contain at least 'food' and 'quantity'.")
}
if (!all(c("food", "number_serving") %in% names(cord))) {
  stop("CoRD must contain at least 'food' and 'number_serving'.")
}

cona_std <- cona %>%
  mutate(
    food = norm_food(food),
    quantity = suppressWarnings(as.numeric(quantity))
  )

cord_std <- cord %>%
  mutate(
    food = norm_food(food),
    number_serving = suppressWarnings(as.numeric(number_serving))
  )

# ------------------------------------------------------------
# 6) Filter 2018+
# ------------------------------------------------------------
cona_std <- cona_std %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= as.Date("2018-01-01"))

cord_std <- cord_std %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(fecha >= as.Date("2018-01-01"))

# ------------------------------------------------------------
# 7) Join TCAC and compute contributions
# ------------------------------------------------------------
cona_eval <- cona_std %>%
  dplyr::left_join(tcac_food, by = "food") %>%
  dplyr::mutate(gramos_consumidos = quantity)

for (v in nutrient_cols) {
  cona_eval[[paste0(v, "_contrib")]] <- cona_eval$gramos_consumidos / 100 * cona_eval[[v]]
}

cord_eval <- cord_std %>%
  dplyr::left_join(tcac_food, by = "food") %>%
  dplyr::mutate(gramos_consumidos = number_serving * gramos_g_1_intercambio_1_intercambio)

for (v in nutrient_cols) {
  cord_eval[[paste0(v, "_contrib")]] <- cord_eval$gramos_consumidos / 100 * cord_eval[[v]]
}

# ------------------------------------------------------------
# 8) Grouping keys
# ------------------------------------------------------------
possible_keys <- c("demo_group", "sex", "age", "ciudad", "fecha", "escenario")
keys_cona <- intersect(possible_keys, names(cona_eval))
keys_cord <- intersect(possible_keys, names(cord_eval))
common_keys <- intersect(keys_cona, keys_cord)
common_keys <- setdiff(common_keys, "escenario")

if (length(common_keys) == 0) {
  stop("No common grouping keys found between CoNA and CoRD.")
}

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
# 9) Aggregate by demographic group
# ------------------------------------------------------------
cona_tot <- cona_eval %>%
  dplyr::group_by(across(all_of(common_keys))) %>%
  dplyr::summarise(across(all_of(cona_contrib_cols), safe_sum), .groups = "drop")

cord_tot <- cord_eval %>%
  dplyr::group_by(across(all_of(common_keys))) %>%
  dplyr::summarise(across(all_of(cord_contrib_cols), safe_sum), .groups = "drop")

# ------------------------------------------------------------
# 10) Separate aportado bases
# ------------------------------------------------------------
aportado_cona <- cona_tot %>%
  pivot_longer(
    cols = all_of(cona_contrib_cols),
    names_to = "nutriente",
    values_to = "aportado_cona"
  ) %>%
  dplyr::mutate(nutriente = gsub("_contrib$", "", nutriente)) %>%
  dplyr::arrange(across(all_of(common_keys)), nutriente)

aportado_cord <- cord_tot %>%
  pivot_longer(
    cols = all_of(cord_contrib_cols),
    names_to = "nutriente",
    values_to = "aportado_cord"
  ) %>%
  dplyr::mutate(nutriente = gsub("_contrib$", "", nutriente)) %>%
  dplyr::arrange(across(all_of(common_keys)), nutriente)

# ------------------------------------------------------------
# 11) Requirements long
# ------------------------------------------------------------
build_req_long <- function(obj, value_name) {
  req_tbl <- as_tibble(obj)
  names(req_tbl) <- norm_names(names(req_tbl))
  
  if (!all(c("age", "sex") %in% names(req_tbl))) {
    stop("Requirement object must contain Age and Sex.")
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
    dplyr::filter(!is.na(nutriente)) %>%
    dplyr::select(age, sex, nutriente, all_of(value_name))
}

req_eer    <- build_req_long(get("EER",    envir = .GlobalEnv), "eer")
req_eer_ll <- build_req_long(get("EER_LL", envir = .GlobalEnv), "eer_ll")
req_ul     <- build_req_long(get("UL",     envir = .GlobalEnv), "ul")

req_bounds <- req_eer %>%
  full_join(req_eer_ll, by = c("age", "sex", "nutriente")) %>%
  full_join(req_ul, by = c("age", "sex", "nutriente"))

# ------------------------------------------------------------
# 12) Validación por nutriente
# ------------------------------------------------------------
join_keys_req <- intersect(c("age", "sex"), common_keys)

validacion_nutriente <- full_join(
  aportado_cona,
  aportado_cord,
  by = c(common_keys, "nutriente")
) %>%
  dplyr::filter(nutriente != "energia_kcal") %>%
  left_join(
    req_bounds %>% filter(nutriente != "energia_kcal"),
    by = c(join_keys_req, "nutriente")
  ) %>%
  dplyr::mutate(
    eer = suppressWarnings(as.numeric(eer)),
    eer_ll = suppressWarnings(as.numeric(eer_ll)),
    ul = suppressWarnings(as.numeric(ul)),
    cumple_cona = case_when(
      is.na(aportado_cona) ~ NA_integer_,
      !is.na(eer_ll) & !is.na(ul) ~ if_else(aportado_cona >= eer_ll & aportado_cona <= ul, 1L, 0L),
      !is.na(eer_ll) &  is.na(ul) ~ if_else(aportado_cona >= eer_ll, 1L, 0L),
      is.na(eer_ll) & !is.na(ul)  ~ if_else(aportado_cona <= ul, 1L, 0L),
      TRUE ~ NA_integer_
    ),
    cumple_cord = case_when(
      is.na(aportado_cord) ~ NA_integer_,
      !is.na(eer_ll) & !is.na(ul) ~ if_else(aportado_cord >= eer_ll & aportado_cord <= ul, 1L, 0L),
      !is.na(eer_ll) &  is.na(ul) ~ if_else(aportado_cord >= eer_ll, 1L, 0L),
      is.na(eer_ll) & !is.na(ul)  ~ if_else(aportado_cord <= ul, 1L, 0L),
      TRUE ~ NA_integer_
    )
  ) %>%
  dplyr::arrange(across(all_of(common_keys)), nutriente)

# ------------------------------------------------------------
# 13) Validación energía por grupo
#     Regla pedida:
#     si el requerimiento (EER) cae dentro de [EER_LL, UL] => 1
# ------------------------------------------------------------
energia_req <- req_bounds %>%
  dplyr::filter(nutriente == "energia_kcal") %>%
  transmute(
    age,
    sex,
    requerimiento_energia = as.numeric(eer),
    eer_ll = as.numeric(eer_ll),
    ul = as.numeric(ul)
  )

validacion_energia_por_grupo <- full_join(
  aportado_cona %>% filter(nutriente == "energia_kcal"),
  aportado_cord %>% filter(nutriente == "energia_kcal"),
  by = c(common_keys, "nutriente")
) %>%
  left_join(
    energia_req,
    by = join_keys_req
  ) %>%
  dplyr::mutate(
    cumple_intervalo = case_when(
      is.na(requerimiento_energia) | is.na(eer_ll) | is.na(ul) ~ NA_integer_,
      requerimiento_energia >= eer_ll & requerimiento_energia <= ul ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  dplyr::arrange(across(all_of(common_keys)))

# ------------------------------------------------------------
# 14) Validación energía total dieta
#     Conteo total de 1 y porcentaje por dieta
# ------------------------------------------------------------
validacion_energia_total_dieta <- validacion_energia_por_grupo %>%
  transmute(
    across(all_of(common_keys)),
    cumple_intervalo
  ) %>%
  dplyr::summarise(
    n_grupos_evaluados = sum(!is.na(cumple_intervalo)),
    n_grupos_cumplen = sum(cumple_intervalo, na.rm = TRUE),
    pct_cumplimiento = if_else(
      n_grupos_evaluados > 0,
      100 * n_grupos_cumplen / n_grupos_evaluados,
      NA_real_
    )
  )

# ------------------------------------------------------------
# 15) Resumen adicional energía por grupo
# ------------------------------------------------------------
validacion_energia_por_grupo_resumen <- validacion_energia_por_grupo %>%
  dplyr::group_by(across(all_of(setdiff(common_keys, "fecha")))) %>%
  dplyr::summarise(
    n_obs = sum(!is.na(cumple_intervalo)),
    n_cumple = sum(cumple_intervalo, na.rm = TRUE),
    pct_cumplimiento = if_else(n_obs > 0, 100 * n_cumple / n_obs, NA_real_),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 16) Save outputs
# ------------------------------------------------------------
saveRDS(aportado_cona, file.path(out_dir, "aportado_cona.rds"))
saveRDS(aportado_cord, file.path(out_dir, "aportado_cord.rds"))
saveRDS(validacion_nutriente, file.path(out_dir, "validacion_nutriente.rds"))
saveRDS(validacion_energia_total_dieta, file.path(out_dir, "validacion_energia_total_dieta.rds"))
saveRDS(validacion_energia_por_grupo, file.path(out_dir, "validacion_energia_por_grupo.rds"))
saveRDS(validacion_energia_por_grupo_resumen, file.path(out_dir, "validacion_energia_por_grupo_resumen.rds"))

# ------------------------------------------------------------
# Save each dataset as its own Excel file
# ------------------------------------------------------------

write_xlsx(aportado_cona,
           file.path(out_dir, "aportado_cona.xlsx"))

write_xlsx(aportado_cord,
           file.path(out_dir, "aportado_cord.xlsx"))

write_csv(
  validacion_nutriente,
  file.path(out_dir, "validacion_nutriente.csv")
)

write_xlsx(validacion_energia_total_dieta,
           file.path(out_dir, "validacion_energia_total_dieta.xlsx"))

write_xlsx(validacion_energia_por_grupo,
           file.path(out_dir, "validacion_energia_por_grupo.xlsx"))

write_xlsx(validacion_energia_por_grupo_resumen,
           file.path(out_dir, "validacion_energia_por_grupo_resumen.xlsx"))

message("Done. Outputs saved in: ", out_dir)
