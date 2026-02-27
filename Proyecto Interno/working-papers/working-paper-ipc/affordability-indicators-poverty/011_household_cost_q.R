#----------------------------------------------------------------------
# HCost QUARTERLY (median) + CoRD
#----------------------------------------------------------------------

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
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output/least_cost_metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp_dir <- file.path(out_dir, "tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

afford_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output/affordability")
dir.create(afford_dir, recursive = TRUE, showWarnings = FALSE)

fig_dir <- file.path(afford_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------------------
# Panel
#----------------------------------------------------------------------
panel_path <- file.path(tmp_dir, "panel_city_month_food_1999_2025.csv")
panel <- read.csv(panel_path, stringsAsFactors = FALSE)

#----------------------------------------------------------------------
# Fecha: normalizar (para que filter ciudad-fecha haga match)
#----------------------------------------------------------------------
to_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  as.Date(x)
}

panel <- panel %>%
  mutate(
    ciudad   = as.character(ciudad),
    articulo = as.character(articulo),
    fecha    = to_date_safe(fecha)
  )

city_vector <- sort(unique(panel$ciudad))
date_vector <- sort(unique(panel$fecha))

#----------------------------------------------------------------------
# Columnas nutricionales (formato FoodpriceR)
#----------------------------------------------------------------------
nutr_cols <- c(
  "Energy","Protein","Lipids","Carbohydrates","VitaminC","Folate","VitaminA",
  "Thiamine","Riboflavin","Niacin","VitaminB12","Magnesium","Phosphorus",
  "Sodium","Calcium","Iron","Zinc"
)

#----------------------------------------------------------------------
# CoRD: serv/diverse + helpers mapping
#----------------------------------------------------------------------
data("serv2",   package = "FoodpriceR")
data("diverse", package = "FoodpriceR")

serv_tbl    <- as_tibble(serv2)
diverse_tbl <- as_tibble(diverse)

norm_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  toupper(trimws(x))
}

canon_subgroup <- function(x) {
  s <- norm_txt(x)
  dplyr::case_when(
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
  "PLATANOS",     "Frutas",        # serv2 no separa plátanos
  "FRUTAS",       "Frutas",
  "VERDURAS",     "Verduras",
  "LACTEOS",      "Lácteos",
  "CARNES",       "Carnes",
  "HUEVOS",       "Carnes",        # serv2 no separa huevos
  "LEGUMINOSAS",  "Leguminosas",
  "GRASAS",       "Grasas",
  "AZUCARES",     NA_character_    # EXCLUIR
)

needed_labels <- canon2serv %>%
  filter(!is.na(serv2_label)) %>%
  pull(serv2_label) %>%
  unique()

missing_labels <- setdiff(needed_labels, unique(serv_tbl$Subgroup))
if (length(missing_labels) > 0) {
  stop("Mapping -> serv2 tiene labels que NO existen en serv2$Subgroup: ",
       paste(missing_labels, collapse = ", "),
       "\nRevisa tildes/ortografía (ej: 'Lácteos').")
}

map_to_serv2 <- function(canon_vec) {
  canon2serv$serv2_label[match(canon_vec, canon2serv$canon)]
}

#----------------------------------------------------------------------
# Builder: arma Data con AMBOS juegos de columnas (clave)
#   - deja Price_100g/Serving para CoCA-CoNA
#   - deja Price_serving/Serving_g para CoRD
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
      
      # MISMA INFO, DOS NOMBRES (para que no se rompa nada)
      Serving_g     = 100,
      Price_serving = suppressWarnings(as.numeric(precio_100g)),
      Serving       = Serving_g,
      Price_100g    = Price_serving,
      
      Group_raw   = grp_src,
      Group_canon = canon_subgroup(grp_src),
      Subgroup    = map_to_serv2(Group_canon),   # AZUCARES -> NA
      Group       = Subgroup
    ) %>%
    transmute(
      Food,
      # CoCA/CoNA
      Price_100g, Serving,
      # CoRD
      Price_serving, Serving_g,
      Group, Subgroup,
      across(all_of(nutr_cols), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    filter(
      !is.na(Food), Food != "",
      !is.na(Price_100g), is.finite(Price_100g), Price_100g > 0,
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
# compute: ciudad x fecha -> HCost (+ CoRD)
#----------------------------------------------------------------------
compute_hcost_city_date <- function(city.x, date.x, panel) {
  
  df_city_month <- panel %>% filter(ciudad == city.x, fecha == date.x)
  if (nrow(df_city_month) == 0) return(NULL)
  
  panel.aux <- build_hcost_df(df_city_month)
  if (nrow(panel.aux) == 0) return(NULL)
  
  hcost.aux <- FoodpriceR::HCost(
    Data      = panel.aux,
    ERR       = EER,
    EER_LL    = EER_LL,
    UL        = UL,
    Household = FoodpriceR::Household,
    Serv      = serv_tbl,
    Diverse   = diverse_tbl
  )
  
  if (inherits(hcost.aux, "error")) {
    message("   -> HCost ERROR: ", conditionMessage(hcost.aux))
    return(NULL)
  }
  
  coca <- if ("Model_CoCA" %in% names(hcost.aux)) as_tibble(hcost.aux$Model_CoCA) else NULL
  cona <- if ("Model_CoNA" %in% names(hcost.aux)) as_tibble(hcost.aux$Model_CoNA) else NULL
  
  # CoRD: si HCost lo trae con nombre variable; si no, fallback a FoodpriceR::CoRD()
  cord <- NULL
  cord_name <- names(hcost.aux)[str_detect(names(hcost.aux), regex("CoRD", ignore_case = TRUE))]
  if (length(cord_name) > 0) {
    cord <- as_tibble(hcost.aux[[cord_name[1]]])
  } else {
    if (exists("CoRD", where = asNamespace("FoodpriceR"), inherits = FALSE)) {
      cord_try <- tryCatch(
        FoodpriceR::CoRD(
          data    = panel.aux %>% select(Food, Serving_g, Price_serving, Group, Subgroup, all_of(nutr_cols)),
          diverse = diverse_tbl,
          serv    = serv_tbl
        ),
        error = function(e) e
      )
      if (!inherits(cord_try, "error")) cord <- as_tibble(cord_try)
    }
  }
  
  if (!is.null(coca)) coca <- coca %>% mutate(ciudad = city.x, fecha = date.x)
  if (!is.null(cona)) cona <- cona %>% mutate(ciudad = city.x, fecha = date.x)
  if (!is.null(cord)) cord <- cord %>% mutate(ciudad = city.x, fecha = date.x)
  
  list(CoCA = coca, CoNA = cona, CoRD = cord)
}

#----------------------------------------------------------------------
# Bucle: ciudades x fechas (mensual base)
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
    if (!is.null(out$CoRD) && is.data.frame(out$CoRD) && nrow(out$CoRD) > 0) res_cord[[length(res_cord) + 1]] <- out$CoRD
  }
}

coca_df <- if (length(res_coca) == 0) tibble() else bind_rows(res_coca)
cona_df <- if (length(res_cona) == 0) tibble() else bind_rows(res_cona)
cord_df <- if (length(res_cord) == 0) tibble() else bind_rows(res_cord)

#----------------------------------------------------------------------
# Guardar mensuales (por si los necesitas)
#----------------------------------------------------------------------
saveRDS(coca_df, file = file.path(afford_dir, "CoCA_city_month.rds"))
saveRDS(cona_df, file = file.path(afford_dir, "CoNA_city_month.rds"))
saveRDS(cord_df, file = file.path(afford_dir, "CoRD_city_month.rds"))

write.csv(coca_df, file = file.path(afford_dir, "CoCA_city_month.csv"), row.names = FALSE)
write.csv(cona_df, file = file.path(afford_dir, "CoNA_city_month.csv"), row.names = FALSE)
write.csv(cord_df, file = file.path(afford_dir, "CoRD_city_month.csv"), row.names = FALSE)

message("Proceso completado. Rows: CoCA=", nrow(coca_df),
        " | CoNA=", nrow(cona_df),
        " | CoRD=", nrow(cord_df))

#======================================================================
# AGREGACIÓN TRIMESTRAL (MEDIANA) 
#======================================================================
suppressWarnings({
  if (!inherits(coca_df$fecha, "Date")) coca_df$fecha <- as.Date(coca_df$fecha)
  if (!inherits(cona_df$fecha, "Date")) cona_df$fecha <- as.Date(cona_df$fecha)
  if (nrow(cord_df) > 0 && !inherits(cord_df$fecha, "Date")) cord_df$fecha <- as.Date(cord_df$fecha)
})

coca_q <- coca_df %>%
  mutate(year = year(fecha), q = quarter(fecha), trimestre = paste0(year, "Q", q))

cona_q <- cona_df %>%
  mutate(year = year(fecha), q = quarter(fecha), trimestre = paste0(year, "Q", q))

cord_q <- cord_df %>%
  mutate(year = year(fecha), q = quarter(fecha), trimestre = paste0(year, "Q", q))

coca_quarter <- coca_q %>%
  dplyr::group_by(ciudad, year, q, trimestre) %>%
  dplyr::summarise(per_capita_month = median(per_capita_month, na.rm = TRUE), .groups = "drop")

cona_quarter <- cona_q %>%
  dplyr::group_by(ciudad, year, q, trimestre) %>%
  dplyr::summarise(per_capita_month = median(per_capita_month, na.rm = TRUE), .groups = "drop")

cord_quarter <- cord_q %>%
  dplyr::group_by(ciudad, year, q, trimestre) %>%
  dplyr::summarise(per_capita_month = median(per_capita_month, na.rm = TRUE), .groups = "drop")

saveRDS(coca_q, file = file.path(afford_dir, "CoCA_city_cuartiles.rds"))
saveRDS(cona_q, file = file.path(afford_dir, "CoNA_city_cuartiles.rds"))
saveRDS(cord_q, file = file.path(afford_dir, "CoRD_city_cuartiles.rds"))

write.csv(coca_q, file = file.path(afford_dir, "CoCA_city_cuartiles.csv"), row.names = FALSE)
write.csv(cona_q, file = file.path(afford_dir, "CoNA_city_cuartiles.csv"), row.names = FALSE)
write.csv(cord_q, file = file.path(afford_dir, "CoRD_city_cuartiles.csv"), row.names = FALSE)

#----------------------------------------------------------------------
# Gráficos TRIMESTRALES 
#----------------------------------------------------------------------
to_quarter_date <- function(x) {
  x <- gsub("\\s+", "", as.character(x))
  year <- as.integer(str_extract(x, "^\\d{4}"))
  q    <- as.integer(str_extract(x, "(?<=Q)\\d+"))
  as.Date(sprintf("%d-%02d-01", year, (q - 1) * 3 + 1))
}

std_city <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTA") ~ "BOGOTA",
    x %in% c("MEDELLÍN", "MEDELLIN")                 ~ "MEDELLIN",
    x %in% c("CALI")                                 ~ "CALI",
    TRUE ~ x
  )
}

city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

coca_quarter <- coca_quarter %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels),
         tri_date = to_quarter_date(trimestre)) %>%
  arrange(ciudad, tri_date)

cona_quarter <- cona_quarter %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels),
         tri_date = to_quarter_date(trimestre)) %>%
  arrange(ciudad, tri_date)

cord_quarter <- cord_quarter %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels),
         tri_date = to_quarter_date(trimestre)) %>%
  arrange(ciudad, tri_date)

# --- Paper theme + scales (tu bloque)
theme_paper <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    plot.margin = margin(6, 6, 6, 6)
  )

color_scale_city <- scale_color_nejm(name = "City")

y_scale_cost <- scale_y_continuous(
  labels = label_number(big.mark = ","),
  expand = expansion(mult = c(0.02, 0.05))
)

quarter_label <- function(x) paste0(year(x), " Q", quarter(x))

x_scale_quarter_paper <- scale_x_date(
  date_breaks = "2 years",
  labels = quarter_label,
  expand = expansion(mult = c(0.01, 0.01))
)

#========================
# CoCA quarterly plot
#========================
g_coca_q <- ggplot(
  coca_quarter,
  aes(x = tri_date, y = per_capita_month, color = ciudad, group = ciudad)
) +
  geom_line(linewidth = 1.1) +
  color_scale_city +
  x_scale_quarter_paper +
  y_scale_cost +
  labs(
    title = "CoCA: Quarterly Median Per Capita Cost",
    x = "Quarter",
    y = "Per Capita Cost"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_dir, "CoCA_per_capita_quarter_city_time.png"),
  plot     = g_coca_q,
  width    = 9, height = 5, dpi = 300, bg = "white"
)

#========================
# CoNA quarterly plot
#========================
g_cona_q <- ggplot(
  cona_quarter,
  aes(x = tri_date, y = per_capita_month, color = ciudad, group = ciudad)
) +
  geom_line(linewidth = 1.1) +
  color_scale_city +
  x_scale_quarter_paper +
  y_scale_cost +
  labs(
    title = "CoNA: Quarterly Median Per Capita Cost",
    x = "Quarter",
    y = "Per Capita Cost"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_dir, "CoNA_per_capita_quarter_city_time.png"),
  plot     = g_cona_q,
  width    = 9, height = 5, dpi = 300, bg = "white"
)

#========================
# CoRD quarterly plot 
#========================
g_cord_q <- ggplot(
  cord_quarter,
  aes(x = tri_date, y = per_capita_month, color = ciudad, group = ciudad)
) +
  geom_line(linewidth = 1.1) +
  color_scale_city +
  x_scale_quarter_paper +
  y_scale_cost +
  labs(
    title = "CoRD: Quarterly Median Per Capita Cost",
    x = "Quarter",
    y = "Per Capita Cost"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_dir, "CoRD_per_capita_quarter_city_time.png"),
  plot     = g_cord_q,
  width    = 9,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)