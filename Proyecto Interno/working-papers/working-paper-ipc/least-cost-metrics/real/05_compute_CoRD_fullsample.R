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

# ------------------------------------------------------------
# 1) Cargar serv2 / diverse
# ------------------------------------------------------------
data("serv2",   package = "FoodpriceR")
data("diverse", package = "FoodpriceR")

serv_tbl    <- as_tibble(serv2)
diverse_tbl <- as_tibble(diverse)

stopifnot(all(c("Age","Serving","Subgroup") %in% names(serv_tbl)))
stopifnot(all(c("Subgroup","Number") %in% names(diverse_tbl)))

message("serv2$Subgroup unique:")
print(sort(unique(serv_tbl$Subgroup)))

# ------------------------------------------------------------
# 1.1) Mapping canónico -> serv2$Subgroup + excluir azúcares
# ------------------------------------------------------------
canon2serv <- tribble(
  ~canon,        ~serv2_label,
  "CEREALES",     "Cereales",
  "RAICES",       "Raices",
  "TUBERCULOS",   "Tuberculos",
  "PLATANOS",     "Frutas",        # serv2 no tiene Plátanos separado
  "FRUTAS",       "Frutas",
  "VERDURAS",     "Verduras",
  "LACTEOS",      "Lácteos",
  "CARNES",       "Carnes",
  "HUEVOS",       "Carnes",        # serv2 no tiene Huevos separado
  "LEGUMINOSAS",  "Leguminosas",
  "GRASAS",       "Grasas",
  "AZUCARES",     NA_character_    # EXCLUIR
)

needed_labels <- canon2serv %>% filter(!is.na(serv2_label)) %>% pull(serv2_label) %>% unique()
missing_labels <- setdiff(needed_labels, unique(serv_tbl$Subgroup))
if (length(missing_labels) > 0) {
  stop("Estos labels del mapping no existen en serv2$Subgroup (revisa tildes): ",
       paste(missing_labels, collapse = ", "))
}

map_to_serv2 <- function(canon_vec) {
  canon2serv$serv2_label[match(canon_vec, canon2serv$canon)]
}

# ------------------------------------------------------------
# 2) Cargar panel y renombrar nutrientes
# ------------------------------------------------------------
panel <- readRDS(file.path(tmp_dir, "panel_city_month_food_1999_2025.rds"))

need_panel <- c("ciudad","fecha","articulo","precio_100g")
miss_panel <- setdiff(need_panel, names(panel))
if (length(miss_panel) > 0) stop("panel missing required columns: ", paste(miss_panel, collapse = ", "))

panel2 <- panel %>%
  mutate(ciudad = as.character(ciudad),
         articulo = as.character(articulo))

if (inherits(panel2$fecha, "Date")) {
  # ok
} else if (is.numeric(panel2$fecha)) {
  panel2 <- panel2 %>% mutate(fecha = as.Date(fecha, origin = "1899-12-30"))
} else {
  panel2 <- panel2 %>% mutate(fecha = as.Date(fecha))
}

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

nutr_cols <- c("Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
               "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
               "Sodium","Calcium","Iron","Zinc")

panel2 <- panel2 %>%
  filter(ciudad %in% cities_use) %>%
  arrange(ciudad, fecha, articulo)

if (!("Subgroup" %in% names(panel2)) && !("Group" %in% names(panel2))) {
  stop("Tu panel no trae Subgroup ni Group. Necesitas un grupo por alimento (articulo -> grupo).")
}

# ------------------------------------------------------------
# 3) Builder: CREA Subgroup + Group + excluye azúcares
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
      
      # label exacto como serv2
      Subgroup    = map_to_serv2(Group_canon),
      
      # <<<<<< CLAVE: CoRD requiere Group >>>>>>
      # lo alineamos con Subgroup para que calce con serv2/diverse
      Group       = Subgroup,
      
      Food        = as.character(articulo),
      Serving_g   = 100,
      Price_serving = suppressWarnings(as.numeric(precio_100g))
    ) %>%
    transmute(
      Food, Serving_g, Price_serving,
      Group, Subgroup, Group_canon, Group_raw,
      across(all_of(nutr_cols), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    filter(
      !is.na(Food), Food != "",
      !is.na(Group),            # excluye Azúcares 
      !is.na(Subgroup),
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
  
  panel_c <- panel2 %>% filter(ciudad == cc)
  fechas  <- sort(unique(panel_c$fecha))
  
  for (f in fechas) {
    
    df_cmf <- panel_c %>% filter(fecha == f)
    df_in  <- build_cordH_df(df_cmf)
    
    if (nrow(df_in) == 0) {
      fail_cordH[[length(fail_cordH) + 1]] <- tibble(
        ciudad = cc, fecha = f,
        motivo = "No valid foods/groups after cleaning (maybe missing price/energy or all sugars)"
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
        mutate(ciudad = cc, fecha = f, escenario = "precio_100g")
      
      comp_df <- as_tibble(cordH$comp) %>%
        mutate(ciudad = cc, fecha = f, escenario = "precio_100g")
      
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

# ------------------------------------------------------------
# 5) Save outputs
# ------------------------------------------------------------
saveRDS(cord_cost, file.path(out_dir, "cordH_cost_fullsample.rds"))
saveRDS(cord_comp, file.path(out_dir, "cordH_comp_fullsample.rds"))
write_csv(cord_cost, file.path(out_dir, "cordH_cost_fullsample.csv"))
write_csv(cord_comp, file.path(out_dir, "cordH_comp_fullsample.csv"))

write_xlsx(
  list(
    cord_cost = cord_cost,
    cord_comp = cord_comp,
    canon2serv = canon2serv
  ),
  file.path(out_dir, "cordH_fullsample.xlsx")
)

message("Saved CoRD_Herforth in: ", out_dir)