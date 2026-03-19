########################################################
## 05_compute_CoRD_fullsample.R
## CoRD (cost + comp) for 3 cities, all months
########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(writexl)
  library(FoodpriceR)
  library(rlang)
})

source("working-papers/working-paper-ipc/least-cost-metrics/00_config.R")
source("ajustes_FoodpriceR/CoRD_Herforth.R")

# ------------------------------------------------------------
# 0) Helpers
# ------------------------------------------------------------
norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  toupper(trimws(x))
}

canon_subgroup <- function(x) {
  s <- norm_txt(x)
  case_when(
    is.na(s) | s == "" ~ NA_character_,
    str_detect(s, "AZUCAR|AZUCARES|DULCE|PANELA|MIEL|CONFIT") ~ "AZUCARES",
    str_detect(s, "LACTE|LACTEO|LECHE|QUESO|YOGUR|YOGURT|KUMIS") ~ "LACTEOS",
    str_detect(s, "HUEVO") ~ "HUEVOS",
    str_detect(s, "CARNE|RES|CERDO|POLLO|AVE|PESCAD|ATUN|SARDIN|JAMON|SALCHICH|EMBUT") ~ "CARNES",
    str_detect(s, "LEGUMIN|FRIJOL|LENTEJ|GARBANZ|ARVEJ") ~ "LEGUMINOSAS",
    str_detect(s, "GRASA|ACEIT|MANTEQ|MARGAR|OLEO") ~ "GRASAS",
    str_detect(s, "VERDUR|HORTALIZ|ENSALAD") ~ "VERDURAS",
    str_detect(s, "FRUTA") ~ "FRUTAS",
    str_detect(s, "CEREAL|ARROZ|MAIZ|AVENA|TRIGO|PAN|PASTA|HARIN") ~ "CEREALES",
    str_detect(s, "RAIC|YUCA|N?AME|NAME") ~ "RAICES",
    str_detect(s, "TUBERC|PAPA") ~ "TUBERCULOS",
    str_detect(s, "PLATAN") ~ "PLATANOS",
    s %in% c("CEREALES","RAICES","TUBERCULOS","PLATANOS","FRUTAS","VERDURAS","LACTEOS",
             "CARNES","HUEVOS","LEGUMINOSAS","GRASAS","AZUCARES") ~ s,
    TRUE ~ NA_character_
  )
}

assign_person <- function(demo_group, sex) {
  sex_chr <- case_when(
    sex %in% c(0, "0", "H", "MASC", "Male", "male") ~ "H",
    sex %in% c(1, "1", "M", "FEM", "Female", "female") ~ "M",
    TRUE ~ NA_character_
  )
  
  case_when(
    demo_group == "31 a 50 años" & sex_chr == "H" ~ 1L,
    demo_group == "31 a 50 años" & sex_chr == "M" ~ 2L,
    demo_group == "9 a 13 años"  & sex_chr == "M" ~ 3L,
    TRUE ~ NA_integer_
  )
}

normalize_serv_subgroup <- function(x) {
  case_when(
    norm_txt(x) == "CEREALES"     ~ "Cereales",
    norm_txt(x) == "RAICES"       ~ "Raices",
    norm_txt(x) == "TUBERCULOS"   ~ "Tuberculos",
    norm_txt(x) == "FRUTAS"       ~ "Frutas",
    norm_txt(x) == "VERDURAS"     ~ "Verduras",
    norm_txt(x) == "LACTEOS"      ~ "Lácteos",
    norm_txt(x) == "CARNES"       ~ "Carnes",
    norm_txt(x) == "LEGUMINOSAS"  ~ "Leguminosas",
    norm_txt(x) == "GRASAS"       ~ "Grasas",
    norm_txt(x) == "AZUCARES"     ~ "Azúcares",
    TRUE ~ as.character(x)
  )
}

# ------------------------------------------------------------
# 1) Cargar serv2 / diverse y normalizar
# ------------------------------------------------------------
data("serv2",   package = "FoodpriceR")
data("diverse", package = "FoodpriceR")

serv_tbl <- as_tibble(serv2) %>%
  mutate(Subgroup = normalize_serv_subgroup(Subgroup))

diverse_tbl <- as_tibble(diverse) %>%
  mutate(Subgroup = normalize_serv_subgroup(Subgroup))

stopifnot(all(c("Age","Serving","Subgroup") %in% names(serv_tbl)))
stopifnot(all(c("Subgroup","Number") %in% names(diverse_tbl)))

message("serv_tbl$Subgroup unique (con azúcares):")
print(sort(unique(serv_tbl$Subgroup)))

message("diverse_tbl$Subgroup unique (con azúcares):")
print(sort(unique(diverse_tbl$Subgroup)))

# ------------------------------------------------------------
# 1.1) Mapping canónico -> serv2$Subgroup
# ------------------------------------------------------------
canon2serv <- tribble(
  ~canon,        ~serv2_label,
  "CEREALES",     "Cereales",
  "RAICES",       "Raices",
  "TUBERCULOS",   "Tuberculos",
  "PLATANOS",     "Frutas",
  "FRUTAS",       "Frutas",
  "VERDURAS",     "Verduras",
  "LACTEOS",      "Lácteos",
  "CARNES",       "Carnes",
  "HUEVOS",       "Carnes",
  "LEGUMINOSAS",  "Leguminosas",
  "GRASAS",       "Grasas",
  "AZUCARES",     "Azúcares"
)

needed_labels <- canon2serv %>%
  filter(!is.na(serv2_label)) %>%
  pull(serv2_label) %>%
  unique()

needed_labels_norm <- norm_txt(needed_labels)
serv_labels_norm   <- norm_txt(unique(serv_tbl$Subgroup))

missing_labels <- needed_labels[!(needed_labels_norm %in% serv_labels_norm)]

if (length(missing_labels) > 0) {
  stop(
    "Estos labels del mapping no existen en serv_tbl$Subgroup aun después de normalizar texto: ",
    paste(missing_labels, collapse = ", ")
  )
}

map_to_serv2 <- function(canon_vec) {
  canon_norm <- norm_txt(canon_vec)
  ref_norm   <- norm_txt(canon2serv$canon)
  canon2serv$serv2_label[match(canon_norm, ref_norm)]
}

# ------------------------------------------------------------
# 2) Cargar panel y renombrar nutrientes
# ------------------------------------------------------------
panel <- readRDS(file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))

need_panel <- c(
  "ciudad","fecha","articulo","precio_100g",
  "gramos_g_1_intercambio_1_intercambio"
)
miss_panel <- setdiff(need_panel, names(panel))
if (length(miss_panel) > 0) {
  stop("panel missing required columns: ", paste(miss_panel, collapse = ", "))
}

panel2 <- panel %>%
  mutate(
    ciudad = as.character(ciudad),
    articulo = as.character(articulo)
  )

# FECHA: igual que CoCA y CoNA -> NO reinterpretar
panel2 <- panel2 %>%
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

nutr_cols <- c(
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)

cities_use_std <- c("CALI", "BOGOTA", "MEDELLIN")

panel2 <- panel2 %>%
  filter(ciudad %in% cities_use_std) %>%
  arrange(ciudad, fecha, articulo)

if (!("Subgroup" %in% names(panel2)) && !("Group" %in% names(panel2))) {
  stop("Tu panel no trae Subgroup ni Group. Necesitas un grupo por alimento (articulo -> grupo).")
}

# ------------------------------------------------------------
# 3) Builder: CREA Subgroup + Group + usa gramos de intercambio
# ------------------------------------------------------------
build_cordH_df <- function(df_city_month) {
  
  grp_src <- if ("Subgroup" %in% names(df_city_month)) {
    df_city_month$Subgroup
  } else if ("Group" %in% names(df_city_month)) {
    df_city_month$Group
  } else {
    NA
  }
  
  df_city_month %>%
    mutate(
      Group_raw   = grp_src,
      Group_canon = canon_subgroup(grp_src),
      Subgroup    = map_to_serv2(Group_canon),
      Group       = Subgroup,
      Food        = as.character(articulo),
      Serving_g   = suppressWarnings(as.numeric(gramos_g_1_intercambio_1_intercambio)),
      Price_serving = suppressWarnings(as.numeric(precio_100g)) * Serving_g / 100
    ) %>%
    transmute(
      Food, Serving_g, Price_serving,
      Group, Subgroup, Group_canon, Group_raw,
      across(all_of(nutr_cols), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    filter(
      !is.na(Food), Food != "",
      !is.na(Group),
      !is.na(Subgroup),
      !is.na(Serving_g), is.finite(Serving_g), Serving_g > 0,
      !is.na(Price_serving), is.finite(Price_serving), Price_serving > 0
    ) %>%
    group_by(Food, Group, Subgroup) %>%
    reframe(
      Serving_g     = first(Serving_g),
      Price_serving = mean(Price_serving, na.rm = TRUE),
      across(all_of(nutr_cols), ~ mean(.x, na.rm = TRUE))
    ) %>%
    filter(
      !is.na(Energy), is.finite(Energy), Energy > 0
    )
}

# ------------------------------------------------------------
# 3.5) Smoke test
# ------------------------------------------------------------
cc0 <- sort(unique(panel2$ciudad))[1]
f0  <- sort(unique((panel2 %>% filter(ciudad == cc0))$fecha))[1]

df0 <- panel2 %>% filter(ciudad == cc0, fecha == f0)
dfi <- build_cordH_df(df0)

message("Smoke test: ", cc0, " - ", as.character(f0))
message("Rows df_in: ", nrow(dfi))
print(dplyr::count(dfi, Group, sort = TRUE))

if (nrow(dfi) == 0) stop("Smoke test dejó df_in vacío.")

cordH0 <- tryCatch(
  CoRD_Herforth(data = dfi, serv = serv_tbl, diverse = diverse_tbl, exclude = NULL),
  error = function(e) e
)

if (inherits(cordH0, "error")) {
  stop("Smoke test: CoRD_Herforth falló. Mensaje: ", conditionMessage(cordH0))
} else {
  message("Smoke test OK: CoRD_Herforth corrió.")
}

# ------------------------------------------------------------
# 4) Runner: ciudad x mes
# ------------------------------------------------------------
results_cost <- list()
results_comp <- list()
fail_cordH   <- list()

for (cc in sort(unique(panel2$ciudad))) {
  
  message("== CoRD_Herforth city: ", cc)
  
  panel_c <- panel2 %>% filter(ciudad == cc) %>% arrange(fecha)
  fechas  <- sort(unique(panel_c$fecha))
  
  for (f in fechas) {
    
    df_cmf <- panel_c %>% filter(fecha == f)
    df_in  <- build_cordH_df(df_cmf)
    
    if (nrow(df_in) == 0) {
      fail_cordH[[length(fail_cordH) + 1]] <- tibble(
        ciudad = cc,
        fecha  = f,
        motivo = "No valid foods/groups after cleaning (missing serving_g/price/energy)"
      )
      next
    }
    
    out <- tryCatch({
      
      cordH <- CoRD_Herforth(
        data    = df_in,
        serv    = serv_tbl,
        diverse = diverse_tbl,
        exclude = NULL
      )
      
      cost_df <- as_tibble(cordH$cost) %>%
        dplyr::select(-dplyr::any_of(c("fecha", "trimestre", "year", "q"))) %>%
        mutate(
          ciudad = cc,
          fecha = f,
          escenario = "price_serving_from_precio_100g"
        )
      
      comp_df <- as_tibble(cordH$comp) %>%
        dplyr::select(-dplyr::any_of(c("fecha", "trimestre", "year", "q"))) %>%
        mutate(
          ciudad = cc,
          fecha = f,
          escenario = "price_serving_from_precio_100g"
        )
      
      list(cost = cost_df, comp = comp_df)
      
    }, error = function(e) {
      
      tr <- tryCatch(rlang::trace_back(), error = function(.) NULL)
      
      fail_cordH[[length(fail_cordH) + 1]] <- tibble(
        ciudad = cc,
        fecha  = f,
        motivo = conditionMessage(e),
        call   = paste(deparse(conditionCall(e)), collapse = " "),
        trace  = if (is.null(tr)) NA_character_ else paste(capture.output(print(tr)), collapse = "\n")
      )
      
      NULL
    })
    
    if (!is.null(out)) {
      results_cost[[length(results_cost) + 1]] <- out$cost
      results_comp[[length(results_comp) + 1]] <- out$comp
    }
  }
}

cord_cost <- if (length(results_cost) == 0) tibble() else bind_rows(results_cost)
cord_comp <- if (length(results_comp) == 0) tibble() else bind_rows(results_comp)
cord_fail <- if (length(fail_cordH) == 0) tibble() else bind_rows(fail_cordH)

# blindaje final de fecha: dejarla tal como viene del loop
if (nrow(cord_cost) > 0) {
  cord_cost <- cord_cost %>%
    mutate(fecha = as.Date(fecha))
}

if (nrow(cord_comp) > 0 && "fecha" %in% names(cord_comp)) {
  cord_comp <- cord_comp %>%
    mutate(fecha = as.Date(fecha))
}

# ------------------------------------------------------------
# 4.1) Variables auxiliares para compatibilidad con figuras
# ------------------------------------------------------------
if (nrow(cord_cost) > 0) {
  cord_cost <- cord_cost %>%
    mutate(
      person = assign_person(Demo_Group, Sex),
      per_capita = as.numeric(cost_day)
    )
}

# trimestral desde mensual
if (nrow(cord_cost) > 0) {
  cord_cost_q <- cord_cost %>%
    dplyr::mutate(
      fecha = as.Date(fecha),
      fecha_q = floor_date(fecha, "quarter"),
      trimestre = paste0(year(fecha_q), "Q", quarter(fecha_q))
    ) %>%
    dplyr::group_by(ciudad, fecha = fecha_q, trimestre, Demo_Group, Sex, person) %>%
    dplyr::summarise(
      across(
        any_of(c("cost_day", "Cost_1000kcal", "energy_day", "per_capita")),
        ~ mean(as.numeric(.x), na.rm = TRUE)
      ),
      .groups = "drop"
    )
} else {
  cord_cost_q <- tibble()
}

# ------------------------------------------------------------
# 5) Save outputs
# ------------------------------------------------------------
saveRDS(cord_cost, file.path(out_dir, "cord_cost_fullsample.rds"))
saveRDS(cord_comp, file.path(out_dir, "cord_comp_fullsample.rds"))
write_csv(cord_cost, file.path(out_dir, "cord_cost_fullsample.csv"))
write_csv(cord_comp, file.path(out_dir, "cord_comp_fullsample.csv"))

write_xlsx(
  list(
    cord_cost = cord_cost,
    cord_comp = cord_comp,
    cord_fail = cord_fail,
    canon2serv = canon2serv
  ),
  file.path(out_dir, "cord_fullsample.xlsx")
)

message("Saved corrected CoRD_Herforth outputs in: ", out_dir)