#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(FoodpriceR)
  library(lubridate)
  library(scales)
  library(ggsci)
})

#----------------------------------------------------------------------
# Directorios
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\Portatil\\Desktop\\Least-cost-diets-and-affordability\\Proyecto Interno\\"

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output/least_cost_metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp_dir <- file.path(out_dir, "tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

afford_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output/affordability")
dir.create(afford_dir, recursive = TRUE, showWarnings = FALSE)

fig_dir <- file.path(afford_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Helpers
#----------------------------------------------------------------------

to_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  
  if (is.numeric(x)) {
    d1 <- as.Date(x, origin = "1899-12-30")
    good_d1 <- !is.na(d1) & d1 >= as.Date("1990-01-01") & d1 <= as.Date("2035-12-31")
    if (all(good_d1)) return(d1)
    
    d2 <- as.Date(x, origin = "1970-01-01")
    good_d2 <- !is.na(d2) & d2 >= as.Date("1990-01-01") & d2 <= as.Date("2035-12-31")
    if (all(good_d2)) return(d2)
    
    return(d1)
  }
  
  x_chr <- as.character(x)
  
  d <- suppressWarnings(ymd(x_chr))
  good <- !is.na(d) & d >= as.Date("1990-01-01") & d <= as.Date("2035-12-31")
  if (sum(good) >= ceiling(0.8 * length(x_chr))) return(d)
  
  d <- suppressWarnings(mdy(x_chr))
  good <- !is.na(d) & d >= as.Date("1990-01-01") & d <= as.Date("2035-12-31")
  if (sum(good) >= ceiling(0.8 * length(x_chr))) return(d)
  
  d <- suppressWarnings(dmy(x_chr))
  good <- !is.na(d) & d >= as.Date("1990-01-01") & d <= as.Date("2035-12-31")
  if (sum(good) >= ceiling(0.8 * length(x_chr))) return(d)
  
  suppressWarnings(as.Date(x_chr))
}

norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  toupper(trimws(x))
}

norm_city <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  
  dplyr::case_when(
    x %in% c("BOGOTA D.C.", "BOGOTA", "SANTAFE DE BOGOTA", "SANTAFE DE BOGOTA D.C.") ~ "BOGOTA",
    x %in% c("MEDELLIN", "MEDELLIN D.C.", "MEDELLIN A.M.", "MEDELLIN AM") ~ "MEDELLIN",
    x %in% c("CALI", "SANTIAGO DE CALI", "CALI A.M.", "CALI AM") ~ "CALI",
    TRUE ~ x
  )
}

assign_person <- function(demo_group, sex) {
  dg <- norm_txt(demo_group)
  sx <- dplyr::case_when(
    sex %in% c(0, "0", "H", "MASC", "Male", "male") ~ "H",
    sex %in% c(1, "1", "M", "FEM", "Female", "female") ~ "M",
    TRUE ~ NA_character_
  )
  
  adulto_31_50 <- dg %in% c("31 A 50 ANOS", "31-50 ANOS", "31 A 50", "31 A 50 AÑOS")
  ninia_9_13   <- dg %in% c("9 A 13 ANOS", "9-13 ANOS", "9 A 13", "9 A 13 AÑOS")
  
  dplyr::case_when(
    adulto_31_50 & sx == "H" ~ 1L,
    adulto_31_50 & sx == "M" ~ 2L,
    ninia_9_13   & sx == "M" ~ 3L,
    TRUE ~ NA_integer_
  )
}

safe_quarter_from_monthly <- function(df, extra_numeric = NULL) {
  if (nrow(df) == 0) return(tibble())
  
  keep_numeric <- unique(c(
    "cost_day", "total_household", "per_capita", "per_capita_year", "per_capita_month",
    "Cost_1000kcal", "cost_1000kcal", "energy_day",
    extra_numeric
  ))
  
  df %>%
    mutate(
      fecha = to_date_safe(fecha),
      fecha_q = floor_date(fecha, "quarter"),
      trimestre = paste0(year(fecha_q), "Q", quarter(fecha_q))
    ) %>%
    dplyr::group_by(across(any_of(c("ciudad", "trimestre", "fecha_q", "Demo_Group", "Sex", "person")))) %>%
    dplyr::summarise(
      across(any_of(keep_numeric), ~ mean(as.numeric(.x), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::rename(fecha = fecha_q)
}

#----------------------------------------------------------------------
# Panel
#----------------------------------------------------------------------
panel_path_rds <- file.path(tmp_dir, "panel_city_month_food_1999_2025.rds")
panel_path_csv <- file.path(tmp_dir, "panel_city_month_food_1999_2025.csv")

if (file.exists(panel_path_rds)) {
  panel <- readRDS(panel_path_rds)
} else {
  panel <- read.csv(panel_path_csv, stringsAsFactors = FALSE)
}

panel <- panel %>%
  mutate(
    ciudad   = norm_city(ciudad),
    articulo = as.character(articulo),
    fecha    = to_date_safe(fecha)
  ) %>%
  filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025)

message("Rango de fechas panel:")
print(range(panel$fecha, na.rm = TRUE))

message("Ciudades en panel:")
print(sort(unique(panel$ciudad)))
print(panel %>% dplyr::count(ciudad, sort = TRUE))

city_vector <- sort(unique(panel$ciudad))
date_vector <- sort(unique(panel$fecha))

#----------------------------------------------------------------------
# Columnas nutricionales
#----------------------------------------------------------------------
nutr_cols <- c(
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)

#----------------------------------------------------------------------
# Cargar serv2 / diverse y normalizar
#----------------------------------------------------------------------

data("serv2",   package = "FoodpriceR")
data("diverse", package = "FoodpriceR")

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

serv_tbl <- as_tibble(serv2) %>%
  mutate(Subgroup = normalize_serv_subgroup(Subgroup)) %>%
  filter(norm_txt(Subgroup) != "AZUCARES")

diverse_tbl <- as_tibble(diverse) %>%
  mutate(Subgroup = normalize_serv_subgroup(Subgroup)) %>%
  filter(norm_txt(Subgroup) != "AZUCARES")

#----------------------------------------------------------------------
# Mapping grupos -> serv2
#----------------------------------------------------------------------

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

canon2serv <- tibble::tribble(
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
  "AZUCARES",     NA_character_
)

needed_labels <- canon2serv %>%
  filter(!is.na(serv2_label)) %>%
  pull(serv2_label) %>%
  unique()

missing_labels <- needed_labels[!(norm_txt(needed_labels) %in% norm_txt(unique(serv_tbl$Subgroup)))]
if (length(missing_labels) > 0) {
  stop("Mapping -> serv2 tiene labels que NO existen en serv_tbl$Subgroup: ",
       paste(missing_labels, collapse = ", "))
}

map_to_serv2 <- function(canon_vec) {
  canon2serv$serv2_label[match(norm_txt(canon_vec), norm_txt(canon2serv$canon))]
}

#----------------------------------------------------------------------
# Adapter
#----------------------------------------------------------------------

as_hcost_input <- function(df, model = c("CoCA","CoNA","CoRD")) {
  model <- match.arg(model)
  
  if (model %in% c("CoCA","CoNA")) {
    if (!("Price_100g" %in% names(df)) && ("Price_serving" %in% names(df))) df$Price_100g <- df$Price_serving
    if (!("Serving"   %in% names(df)) && ("Serving_g" %in% names(df)))      df$Serving    <- df$Serving_g
    
    df %>%
      select(Food, Price_100g, Serving, Group, Subgroup, all_of(nutr_cols))
  } else {
    if (!("Price_serving" %in% names(df)) && ("Price_100g" %in% names(df))) df$Price_serving <- df$Price_100g
    if (!("Serving_g" %in% names(df)) && ("Serving" %in% names(df)))        df$Serving_g     <- df$Serving
    
    df %>%
      select(Food, Serving_g, Price_serving, Group, Subgroup, all_of(nutr_cols))
  }
}

#----------------------------------------------------------------------
# Builder: CoCA/CoNA + CoRD corregido dentro del mismo Data
#----------------------------------------------------------------------

build_hcost_df <- function(df_city_month) {
  
  grp_src <- if ("Subgroup" %in% names(df_city_month)) {
    df_city_month$Subgroup
  } else if ("Group" %in% names(df_city_month)) {
    df_city_month$Group
  } else if ("subgroup" %in% names(df_city_month)) {
    df_city_month$subgroup
  } else if ("group" %in% names(df_city_month)) {
    df_city_month$group
  } else {
    NA
  }
  
  df_city_month %>%
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
    ) %>%
    mutate(
      Food = as.character(articulo),
      Price_100g = suppressWarnings(as.numeric(precio_100g)),
      Serving    = 100,
      Serving_g     = suppressWarnings(as.numeric(gramos_g_1_intercambio_1_intercambio)),
      Price_serving = Price_100g * Serving_g / 100,
      Group_raw   = grp_src,
      Group_canon = canon_subgroup(grp_src),
      Subgroup    = map_to_serv2(Group_canon),
      Group       = Subgroup
    ) %>%
    transmute(
      Food,
      Price_100g, Serving,
      Price_serving, Serving_g,
      Group, Subgroup,
      across(all_of(nutr_cols), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    filter(
      !is.na(Food), Food != "",
      !is.na(Price_100g), is.finite(Price_100g), Price_100g > 0,
      !is.na(Serving_g), is.finite(Serving_g), Serving_g > 0,
      !is.na(Price_serving), is.finite(Price_serving), Price_serving > 0,
      !is.na(Energy), is.finite(Energy), Energy > 0,
      !is.na(Subgroup), !is.na(Group)
    ) %>%
    group_by(Food, Group, Subgroup) %>%
    reframe(
      Price_100g    = mean(Price_100g, na.rm = TRUE),
      Price_serving = mean(Price_serving, na.rm = TRUE),
      Serving       = first(Serving),
      Serving_g     = first(Serving_g),
      across(all_of(nutr_cols), ~ mean(.x, na.rm = TRUE))
    ) %>%
    ungroup()
}

#----------------------------------------------------------------------
# compute: ciudad x fecha -> HCost
#----------------------------------------------------------------------

compute_hcost_city_date <- function(city.x, date.x, panel) {
  
  df_city_month <- panel %>% filter(ciudad == city.x, fecha == date.x)
  if (nrow(df_city_month) == 0) return(NULL)
  
  panel.aux <- build_hcost_df(df_city_month)
  if (nrow(panel.aux) == 0) return(NULL)
  
  hcost.aux <- tryCatch(
    suppressMessages(
      FoodpriceR::HCost(
        Data      = panel.aux,
        ERR       = EER,
        EER_LL    = EER_LL,
        UL        = UL,
        Household = FoodpriceR::Household,
        Serv      = serv_tbl,
        Diverse   = diverse_tbl
      )
    ),
    error = function(e) e
  )
  
  if (inherits(hcost.aux, "error")) {
    message("   -> HCost ERROR: ", conditionMessage(hcost.aux))
    return(NULL)
  }
  
  coca <- if ("Model_CoCA" %in% names(hcost.aux)) as_tibble(hcost.aux$Model_CoCA) else NULL
  cona <- if ("Model_CoNA" %in% names(hcost.aux)) as_tibble(hcost.aux$Model_CoNA) else NULL
  
  cord <- NULL
  cord_name <- names(hcost.aux)[str_detect(names(hcost.aux), regex("CoRD", ignore_case = TRUE))]
  if (length(cord_name) > 0) {
    cord <- as_tibble(hcost.aux[[cord_name[1]]])
  }
  
  if (!is.null(coca) && nrow(coca) > 0) {
    coca <- coca %>%
      dplyr::select(-dplyr::any_of(c("fecha", "trimestre", "year", "q"))) %>%
      dplyr::mutate(ciudad = city.x, fecha = date.x)
    if (!("person" %in% names(coca)) && all(c("Demo_Group", "Sex") %in% names(coca))) {
      coca <- coca %>% dplyr::mutate(person = assign_person(Demo_Group, Sex))
    }
  }
  
  if (!is.null(cona) && nrow(cona) > 0) {
    cona <- cona %>%
      dplyr::select(-dplyr::any_of(c("fecha", "trimestre", "year", "q"))) %>%
      dplyr::mutate(ciudad = city.x, fecha = date.x)
    if (!("person" %in% names(cona)) && all(c("Demo_Group", "Sex") %in% names(cona))) {
      cona <- cona %>% dplyr::mutate(person = assign_person(Demo_Group, Sex))
    }
  }
  
  if (!is.null(cord) && nrow(cord) > 0) {
    cord <- cord %>%
      dplyr::select(-dplyr::any_of(c("fecha", "trimestre", "year", "q"))) %>%
      dplyr::mutate(ciudad = city.x, fecha = date.x)
    if (!("person" %in% names(cord)) && all(c("Demo_Group", "Sex") %in% names(cord))) {
      cord <- cord %>% dplyr::mutate(person = assign_person(Demo_Group, Sex))
    }
  }
  
  list(CoCA = coca, CoNA = cona, CoRD = cord)
}

#----------------------------------------------------------------------
# Smoke test
#----------------------------------------------------------------------
cc0 <- city_vector[1]
f0  <- date_vector[1]
message("Smoke test: ", cc0, " | ", as.character(f0))
df0 <- panel %>% filter(ciudad == cc0, fecha == f0)
dfi <- build_hcost_df(df0)
message("  rows build_hcost_df = ", nrow(dfi))
if (nrow(dfi) > 0) print(dplyr::count(dfi, Group, sort = TRUE))

#----------------------------------------------------------------------
# Bucle: ciudades x fechas
#----------------------------------------------------------------------
res_coca <- list()
res_cona <- list()
res_cord <- list()

for (city.x in city_vector) {
  for (date.x in date_vector) {
    
    message("Procesando ciudad = ", city.x, " | fecha = ", as.character(date.x), " ...")
    
    out <- compute_hcost_city_date(city.x, date.x, panel)
    
    if (is.null(out)) {
      message("   -> sin resultado (error o sin datos suficientes).")
      next
    }
    
    if (!is.null(out$CoCA) && nrow(out$CoCA) > 0) res_coca[[length(res_coca) + 1]] <- out$CoCA
    if (!is.null(out$CoNA) && nrow(out$CoNA) > 0) res_cona[[length(res_cona) + 1]] <- out$CoNA
    if (!is.null(out$CoRD) && nrow(out$CoRD) > 0) res_cord[[length(res_cord) + 1]] <- out$CoRD
  }
}

coca_df <- if (length(res_coca) == 0) tibble() else bind_rows(res_coca)
cona_df <- if (length(res_cona) == 0) tibble() else bind_rows(res_cona)
cord_df <- if (length(res_cord) == 0) tibble() else bind_rows(res_cord)

coca_df <- coca_df %>% mutate(fecha = to_date_safe(fecha)) %>% filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025)
cona_df <- cona_df %>% mutate(fecha = to_date_safe(fecha)) %>% filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025)
cord_df <- cord_df %>% mutate(fecha = to_date_safe(fecha)) %>% filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025)

#----------------------------------------------------------------------
# Trimestrales
#----------------------------------------------------------------------
coca_q_df <- safe_quarter_from_monthly(coca_df)
cona_q_df <- safe_quarter_from_monthly(cona_df)
cord_q_df <- safe_quarter_from_monthly(cord_df, extra_numeric = c("Cost_1000kcal", "energy_day"))

coca_q_df <- coca_q_df %>% mutate(fecha = to_date_safe(fecha)) %>% filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025)
cona_q_df <- cona_q_df %>% mutate(fecha = to_date_safe(fecha)) %>% filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025)
cord_q_df <- cord_q_df %>% mutate(fecha = to_date_safe(fecha)) %>% filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025)

#----------------------------------------------------------------------
# Guardar resultados
#----------------------------------------------------------------------
saveRDS(coca_df, file = file.path(afford_dir, "CoCA_city_month.rds"))
saveRDS(cona_df, file = file.path(afford_dir, "CoNA_city_month.rds"))
saveRDS(cord_df, file = file.path(afford_dir, "CoRD_city_month.rds"))

saveRDS(coca_q_df, file = file.path(afford_dir, "CoCA_city_cuartiles.rds"))
saveRDS(cona_q_df, file = file.path(afford_dir, "CoNA_city_cuartiles.rds"))
saveRDS(cord_q_df, file = file.path(afford_dir, "CoRD_city_cuartiles.rds"))

write.csv(coca_df, file = file.path(afford_dir, "CoCA_city_month.csv"), row.names = FALSE)
write.csv(cona_df, file = file.path(afford_dir, "CoNA_city_month.csv"), row.names = FALSE)
write.csv(cord_df, file = file.path(afford_dir, "CoRD_city_month.csv"), row.names = FALSE)

write.csv(coca_q_df, file = file.path(afford_dir, "CoCA_city_cuartiles.csv"), row.names = FALSE)
write.csv(cona_q_df, file = file.path(afford_dir, "CoNA_city_cuartiles.csv"), row.names = FALSE)
write.csv(cord_q_df, file = file.path(afford_dir, "CoRD_city_cuartiles.csv"), row.names = FALSE)

message("Proceso completado.")
message("Rows mensuales: CoCA=", nrow(coca_df),
        " | CoNA=", nrow(cona_df),
        " | CoRD=", nrow(cord_df))
message("Rows trimestrales: CoCA=", nrow(coca_q_df),
        " | CoNA=", nrow(cona_q_df),
        " | CoRD=", nrow(cord_q_df))

#----------------------------------------------------------------------
# Visualization figures (paper style)
#----------------------------------------------------------------------

suppressWarnings({
  if (!inherits(coca_df$fecha, "Date")) coca_df$fecha <- to_date_safe(coca_df$fecha)
  if (!inherits(cona_df$fecha, "Date")) cona_df$fecha <- to_date_safe(cona_df$fecha)
  if (exists("cord_df") && is.data.frame(cord_df) && nrow(cord_df) > 0) {
    if (!inherits(cord_df$fecha, "Date")) cord_df$fecha <- to_date_safe(cord_df$fecha)
  }
})

pick_plot_col <- function(df, model_name = "model") {
  cand <- c(
    "per_capita_month",
    "percapita_month",
    "per_capita_monthly",
    "cost_per_capita_month",
    "monthly_per_capita",
    "per_capita"
  )
  
  hit <- intersect(cand, names(df))
  if (length(hit) > 0) return(hit[1])
  
  stop(
    "No encontré una columna usable para ", model_name, ". Columnas disponibles: ",
    paste(names(df), collapse = ", ")
  )
}

coca_col <- pick_plot_col(coca_df, "CoCA")
cona_col <- pick_plot_col(cona_df, "CoNA")
cord_col <- if (exists("cord_df") && is.data.frame(cord_df) && nrow(cord_df) > 0) pick_plot_col(cord_df, "CoRD") else NA_character_

theme_paper2 <- theme_classic(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 16, hjust = 0),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 10, color = "black"),
    legend.title    = element_text(size = 11),
    legend.text     = element_text(size = 10),
    legend.position = "top",
    legend.justification = "left",
    axis.line       = element_line(linewidth = 0.5),
    axis.ticks      = element_line(linewidth = 0.4),
    plot.margin     = margin(8, 10, 8, 8)
  )

color_scale <- scale_color_nejm(name = "City")

scale_x_month_clean <- scale_x_date(
  limits = c(as.Date("1999-01-01"), as.Date("2025-12-31")),
  date_breaks = "2 years",
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.02))
)

scale_x_quarter_clean <- scale_x_date(
  limits = c(as.Date("1999-01-01"), as.Date("2025-12-31")),
  date_breaks = "2 years",
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.02))
)

scale_y_clean <- scale_y_continuous(
  labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE),
  expand = expansion(mult = c(0.02, 0.04))
)

monthly_city_series <- function(df, value_col) {
  df %>%
    dplyr::mutate(
      fecha = to_date_safe(fecha),
      value_raw = as.numeric(.[[value_col]])
    ) %>%
    dplyr::filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025) %>%
    dplyr::group_by(ciudad, fecha) %>%
    dplyr::summarise(
      value = median(value_raw, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ciudad, fecha)
}

# trimestral = promedio de los meses del trimestre
# -------------------------
# Trimestral: usar directamente los q_df
# -------------------------

quarterly_city_series_from_qdf <- function(df, value_col) {
  df %>%
    dplyr::mutate(
      fecha = to_date_safe(fecha),
      value_raw = as.numeric(.[[value_col]])
    ) %>%
    dplyr::filter(!is.na(fecha), year(fecha) >= 1999, year(fecha) <= 2025) %>%
    dplyr::group_by(ciudad, fecha) %>%
    dplyr::summarise(
      value = mean(value_raw, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(ciudad, fecha)
}

coca_sum <- monthly_city_series(coca_df, coca_col)
cona_sum <- monthly_city_series(cona_df, cona_col)
cord_sum <- if (!is.na(cord_col)) monthly_city_series(cord_df, cord_col) else NULL

coca_q_col <- pick_plot_col(coca_q_df, "CoCA quarterly")
cona_q_col <- pick_plot_col(cona_q_df, "CoNA quarterly")
cord_q_col <- if (exists("cord_q_df") && is.data.frame(cord_q_df) && nrow(cord_q_df) > 0) {
  pick_plot_col(cord_q_df, "CoRD quarterly")
} else {
  NA_character_
}

coca_q_sum <- quarterly_city_series_from_qdf(coca_q_df, coca_q_col)
cona_q_sum <- quarterly_city_series_from_qdf(cona_q_df, cona_q_col)
cord_q_sum <- if (!is.na(cord_q_col)) quarterly_city_series_from_qdf(cord_q_df, cord_q_col) else NULL

g_coca <- ggplot(coca_sum,
                 aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_month_clean +
  scale_y_clean +
  color_scale +
  labs(
    title = "CoCA: Monthly Per Capita Cost",
    x = NULL,
    y = "Per capita monthly cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoCA_per_capita_month_city_time.png"),
  plot     = g_coca,
  width    = 11, height = 5.8, dpi = 300, bg = "white"
)

g_cona <- ggplot(cona_sum,
                 aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_month_clean +
  scale_y_clean +
  color_scale +
  labs(
    title = "CoNA: Monthly Per Capita Cost",
    x = NULL,
    y = "Per capita monthly cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoNA_per_capita_month_city_time.png"),
  plot     = g_cona,
  width    = 11, height = 5.8, dpi = 300, bg = "white"
)

if (!is.null(cord_sum) && nrow(cord_sum) > 0) {
  g_cord <- ggplot(cord_sum,
                   aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    scale_x_month_clean +
    scale_y_clean +
    color_scale +
    labs(
      title = "CoRD: Monthly Per Capita Cost",
      x = NULL,
      y = "Per capita monthly cost"
    ) +
    theme_paper2
  
  ggsave(
    filename = file.path(fig_dir, "CoRD_per_capita_month_city_time.png"),
    plot     = g_cord,
    width    = 11, height = 5.8, dpi = 300, bg = "white"
  )
}

g_coca_q <- ggplot(coca_q_sum,
                   aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_quarter_clean +
  scale_y_clean +
  color_scale +
  labs(
    title = "CoCA: Quarterly Per Capita Cost",
    x = "Quarter",
    y = "Per capita quarterly cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoCA_per_capita_quarter_city_time.png"),
  plot     = g_coca_q,
  width    = 11, height = 5.8, dpi = 300, bg = "white"
)

g_cona_q <- ggplot(cona_q_sum,
                   aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  scale_x_quarter_clean +
  scale_y_clean +
  color_scale +
  labs(
    title = "CoNA: Quarterly Per Capita Cost",
    x = "Quarter",
    y = "Per capita quarterly cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoNA_per_capita_quarter_city_time.png"),
  plot     = g_cona_q,
  width    = 11, height = 5.8, dpi = 300, bg = "white"
)

if (!is.null(cord_q_sum) && nrow(cord_q_sum) > 0) {
  g_cord_q <- ggplot(cord_q_sum,
                     aes(x = fecha, y = value, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 1) +
    scale_x_quarter_clean +
    scale_y_clean +
    color_scale +
    labs(
      title = "CoRD: Quarterly Per Capita Cost",
      x = "Quarter",
      y = "Per capita quarterly cost"
    ) +
    theme_paper2
  
  ggsave(
    filename = file.path(fig_dir, "CoRD_per_capita_quarter_city_time.png"),
    plot     = g_cord_q,
    width    = 11, height = 5.8, dpi = 300, bg = "white"
  )
}

coca_sc <- coca_sum %>% dplyr::rename(CoCA = value)
cona_sc <- cona_sum %>% dplyr::rename(CoNA = value)

scat_coca_cona <- coca_sc %>%
  inner_join(cona_sc, by = c("ciudad", "fecha"))

g_scatter_coca_cona <- ggplot(scat_coca_cona,
                              aes(x = CoCA, y = CoNA, color = ciudad)) +
  geom_point(alpha = 0.75, size = 2) +
  color_scale +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
  labs(
    title = "CoCA vs CoNA",
    x = "CoCA monthly per capita cost",
    y = "CoNA monthly per capita cost"
  ) +
  theme_paper2

ggsave(
  filename = file.path(fig_dir, "CoCA_vs_CoNA_scatter.png"),
  plot     = g_scatter_coca_cona,
  width    = 7.5, height = 5.5, dpi = 300, bg = "white"
)

if (!is.null(cord_sum) && nrow(cord_sum) > 0) {
  cord_sc <- cord_sum %>% dplyr::rename(CoRD = value)
  
  scat_coca_cord <- coca_sc %>%
    inner_join(cord_sc, by = c("ciudad", "fecha"))
  
  g_scatter_coca_cord <- ggplot(scat_coca_cord,
                                aes(x = CoCA, y = CoRD, color = ciudad)) +
    geom_point(alpha = 0.75, size = 2) +
    color_scale +
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    labs(
      title = "CoCA vs CoRD",
      x = "CoCA monthly per capita cost",
      y = "CoRD monthly per capita cost"
    ) +
    theme_paper2
  
  ggsave(
    filename = file.path(fig_dir, "CoCA_vs_CoRD_scatter.png"),
    plot     = g_scatter_coca_cord,
    width    = 7.5, height = 5.5, dpi = 300, bg = "white"
  )
  
  scat_cona_cord <- cona_sc %>%
    inner_join(cord_sc, by = c("ciudad", "fecha"))
  
  g_scatter_cona_cord <- ggplot(scat_cona_cord,
                                aes(x = CoNA, y = CoRD, color = ciudad)) +
    geom_point(alpha = 0.75, size = 2) +
    color_scale +
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    labs(
      title = "CoNA vs CoRD",
      x = "CoNA monthly per capita cost",
      y = "CoRD monthly per capita cost"
    ) +
    theme_paper2
  
  ggsave(
    filename = file.path(fig_dir, "CoNA_vs_CoRD_scatter.png"),
    plot     = g_scatter_cona_cord,
    width    = 7.5, height = 5.5, dpi = 300, bg = "white"
  )
}

message("Done. Outputs saved in: ", fig_dir)