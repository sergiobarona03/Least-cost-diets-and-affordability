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
# Adapter: asegura nombres de columnas según modelo
#----------------------------------------------------------------------
as_hcost_input <- function(df, model = c("CoCA","CoNA","CoRD")) {
  model <- match.arg(model)
  
  if (model %in% c("CoCA","CoNA")) {
    if (!("Price_100g" %in% names(df)) && ("Price_serving" %in% names(df))) df$Price_100g <- df$Price_serving
    if (!("Serving"   %in% names(df)) && ("Serving_g"     %in% names(df))) df$Serving   <- df$Serving_g
    df %>% select(Food, Price_100g, Serving, Group, Subgroup, all_of(nutr_cols))
  } else {
    if (!("Price_serving" %in% names(df)) && ("Price_100g" %in% names(df))) df$Price_serving <- df$Price_100g
    if (!("Serving_g"     %in% names(df)) && ("Serving"   %in% names(df))) df$Serving_g     <- df$Serving
    df %>% select(Food, Serving_g, Price_serving, Group, Subgroup, all_of(nutr_cols))
  }
}

#----------------------------------------------------------------------
# Builder: arma Data con AMBOS juegos de columnas (clave)
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
      
      # ---- MISMA INFO, DOS NOMBRES ----
      Serving_g     = 100,
      Price_serving = suppressWarnings(as.numeric(precio_100g)),
      Serving       = Serving_g,
      Price_100g    = Price_serving,
      
      Group_raw   = grp_src,
      Group_canon = canon_subgroup(grp_src),
      Subgroup    = map_to_serv2(Group_canon),   # AZUCARES -> NA
      Group       = Subgroup                     # alinear Group/Subgroup a serv2/diverse
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
      # mantiene ambos nombres consistentes
      Price_100g    = mean(Price_100g, na.rm = TRUE),
      Price_serving = mean(Price_serving, na.rm = TRUE),
      Serving       = first(Serving),
      Serving_g     = first(Serving_g),
      across(all_of(nutr_cols), ~ mean(.x, na.rm = TRUE))
    ) %>%
    ungroup()
}

#----------------------------------------------------------------------
# compute: ciudad x fecha -> HCost (+ CoRD fallback si aplica)
#----------------------------------------------------------------------
compute_hcost_city_date <- function(city.x, date.x, panel) {
  
  df_city_month <- panel %>% filter(ciudad == city.x, fecha == date.x)
  if (nrow(df_city_month) == 0) return(NULL)
  
  panel.aux <- build_hcost_df(df_city_month)
  if (nrow(panel.aux) == 0) return(NULL)
  
  # input CoCA/CoNA seguro (no se rompe por renombrar a CoRD)
  data_h <- as_hcost_input(panel.aux, "CoCA")
  
  hcost.aux <- FoodpriceR::HCost(
    Data    = panel.aux,
    ERR     = EER,
    EER_LL  = EER_LL,
    UL      = UL,
    Household = FoodpriceR::Household,
    Serv    = serv_tbl,
    Diverse = diverse_tbl
  )
  
  if (inherits(hcost.aux, "error")) {
    message("   -> HCost ERROR: ", conditionMessage(hcost.aux))
    return(NULL)
  }
  
  coca <- if ("Model_CoCA" %in% names(hcost.aux)) as_tibble(hcost.aux$Model_CoCA) else NULL
  cona <- if ("Model_CoNA" %in% names(hcost.aux)) as_tibble(hcost.aux$Model_CoNA) else NULL
  
  # CoRD: a veces viene dentro de HCost con nombre variable; si no, fallback a FoodpriceR::CoRD()
  cord <- NULL
  cord_name <- names(hcost.aux)[str_detect(names(hcost.aux), regex("CoRD", ignore_case = TRUE))]
  if (length(cord_name) > 0) {
    cord <- as_tibble(hcost.aux[[cord_name[1]]])
  } else {
    if (exists("CoRD", where = asNamespace("FoodpriceR"), inherits = FALSE)) {
      data_cord <- as_hcost_input(panel.aux, "CoRD")
      cord_try <- tryCatch(
        FoodpriceR::CoRD(data = data_cord, diverse = diverse_tbl, serv = serv_tbl),
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

#----------------------------------------------------------------------
# Guardar resultados
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

#----------------------------------------------------------------------
# Visualization figures (paper style)
#   1) CoCA: monthly per-capita cost by city over time
#   2) CoNA: monthly per-capita cost by city over time
#   3) CoRD: monthly per-capita cost by city over time
#   4) CoCA vs CoNA comparison (scatter)
#   5) CoCA vs CoRD comparison (scatter)
#   6) CoNA vs CoRD comparison (scatter)
#----------------------------------------------------------------------

suppressWarnings({
  if (!inherits(coca_df$fecha, "Date")) coca_df$fecha <- as.Date(coca_df$fecha)
  if (!inherits(cona_df$fecha, "Date")) cona_df$fecha <- as.Date(cona_df$fecha)
  if (exists("cord_df") && is.data.frame(cord_df) && nrow(cord_df) > 0) {
    if (!inherits(cord_df$fecha, "Date")) cord_df$fecha <- as.Date(cord_df$fecha)
  }
})

# -------------------------
# Helper: robustly locate per-capita monthly column
# -------------------------
pick_percap_col <- function(df) {
  cand <- c("per_capita_month", "percapita_month", "per_capita_monthly",
            "cost_per_capita_month", "monthly_per_capita", "per_capita")
  hit <- intersect(cand, names(df))
  if (length(hit) > 0) return(hit[1])
  
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- setdiff(num_cols, c("fecha"))
  if (length(num_cols) == 0) return(NA_character_)
  num_cols[1]
}

coca_col <- pick_percap_col(coca_df)
cona_col <- pick_percap_col(cona_df)
cord_col <- if (exists("cord_df") && is.data.frame(cord_df) && nrow(cord_df) > 0) pick_percap_col(cord_df) else NA_character_

if (is.na(coca_col)) stop("Could not find a per-capita monthly column in coca_df.")
if (is.na(cona_col)) stop("Could not find a per-capita monthly column in cona_df.")
if (exists("cord_df") && is.data.frame(cord_df) && nrow(cord_df) > 0 && is.na(cord_col)) {
  stop("Could not find a per-capita monthly column in cord_df.")
}


# Tema base
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

# Escalas X
x_scale_6m <- scale_x_date(
  date_breaks = "6 months",
  date_labels = "%Y-%m",
  expand = expansion(mult = c(0.01, 0.01))
)

# Y como %
y_scale_pct <- scale_y_continuous(
  labels = label_percent(scale = 1, accuracy = 0.1),
  expand = expansion(mult = c(0.02, 0.02))
)

# Paleta
color_scale <- scale_color_nejm(name = "City")

# -------------------------
# 1) CoCA: monthly per-capita by city-date
# -------------------------
coca_sum <- coca_df %>%
  group_by(ciudad, fecha) %>%
  dplyr::summarise(per_capita_month = sum(.data[[coca_col]], na.rm = TRUE),
                   .groups = "drop")

g_coca <- ggplot(coca_sum,
                 aes(x = fecha, y = per_capita_month, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 0.9) +
  x_scale_6m +
  labs(
    title = "CoCA: Monthly per-capita cost of the affordable diet",
    x = NULL,
    y = "Monthly per-capita cost"
  ) +
  color_scale +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_dir, "CoCA_per_capita_month_city_time.png"),
  plot     = g_coca,
  width    = 10, height = 5, dpi = 300, bg = "white"
)

# -------------------------
# 2) CoNA: monthly per-capita by city-date
# -------------------------
cona_sum <- cona_df %>%
  group_by(ciudad, fecha) %>%
  dplyr::summarise(per_capita_month = sum(.data[[cona_col]], na.rm = TRUE),
                   .groups = "drop")

g_cona <- ggplot(cona_sum,
                 aes(x = fecha, y = per_capita_month, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 0.9) +
  x_scale_6m +
  labs(
    title = "CoNA: Monthly per-capita cost of the nutritious diet",
    x = NULL,
    y = "Monthly per-capita cost"
  ) +
  color_scale +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(afford_dir, "CoNA_per_capita_month_city_time.png"),
  plot     = g_cona,
  width    = 10, height = 5, dpi = 300, bg = "white"
)

# -------------------------
# 3) CoRD: monthly per-capita by city-date
# -------------------------
cord_sum <- NULL
if (exists("cord_df") && is.data.frame(cord_df) && nrow(cord_df) > 0) {
  
  cord_sum <- cord_df %>%
    group_by(ciudad, fecha) %>%
    dplyr::summarise(per_capita_month = sum(.data[[cord_col]], na.rm = TRUE),
                     .groups = "drop")
  
  g_cord <- ggplot(cord_sum,
                   aes(x = fecha, y = per_capita_month, color = ciudad, group = ciudad)) +
    geom_line(linewidth = 0.9) +
    x_scale_6m +
    labs(
      title = "CoRD: Monthly per-capita cost of the recommended diet",
      x = NULL,
      y = "Monthly per-capita cost"
    ) +
    color_scale +
    theme_paper +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    filename = file.path(afford_dir, "CoRD_per_capita_month_city_time.png"),
    plot     = g_cord,
    width    = 10, height = 5, dpi = 300, bg = "white"
  )
} else {
  message("Note: cord_df is empty or missing, so CoRD figures were not produced.")
}

# -------------------------
# 4) CoCA vs CoNA scatter (city-date matched)
# -------------------------
coca_sc <- coca_sum %>% dplyr::rename(CoCA_per_capita_month = per_capita_month)
cona_sc <- cona_sum %>% dplyr::rename(CoNA_per_capita_month = per_capita_month)

scat_coca_cona <- coca_sc %>%
  inner_join(cona_sc, by = c("ciudad", "fecha"))

g_scatter_coca_cona <- ggplot(scat_coca_cona,
                              aes(x = CoCA_per_capita_month, y = CoNA_per_capita_month, color = ciudad)) +
  geom_point(alpha = 0.8, size = 1.9) +
  labs(
    title = "CoCA vs CoNA: Monthly per-capita cost comparison",
    x = "CoCA (monthly per-capita cost)",
    y = "CoNA (monthly per-capita cost)"
  ) +
  color_scale +
  theme_paper

ggsave(
  filename = file.path(afford_dir, "CoCA_vs_CoNA_scatter_per_capita_month.png"),
  plot     = g_scatter_coca_cona,
  width    = 7.5, height = 5.5, dpi = 300, bg = "white"
)

# -------------------------
# 5) CoCA vs CoRD scatter (city-date matched)
# -------------------------
if (!is.null(cord_sum) && nrow(cord_sum) > 0) {
  
  cord_sc <- cord_sum %>% dplyr::rename(CoRD_per_capita_month = per_capita_month)
  
  scat_coca_cord <- coca_sc %>%
    inner_join(cord_sc, by = c("ciudad", "fecha"))
  
  g_scatter_coca_cord <- ggplot(scat_coca_cord,
                                aes(x = CoCA_per_capita_month, y = CoRD_per_capita_month, color = ciudad)) +
    geom_point(alpha = 0.8, size = 1.9) +
    labs(
      title = "CoCA vs CoRD: Monthly per-capita cost comparison",
      x = "CoCA (monthly per-capita cost)",
      y = "CoRD (monthly per-capita cost)"
    ) +
    color_scale +
    theme_paper
  
  ggsave(
    filename = file.path(afford_dir, "CoCA_vs_CoRD_scatter_per_capita_month.png"),
    plot     = g_scatter_coca_cord,
    width    = 7.5, height = 5.5, dpi = 300, bg = "white"
  )
  
  # -------------------------
  # 6) CoNA vs CoRD scatter (city-date matched)
  # -------------------------
  scat_cona_cord <- cona_sc %>%
    inner_join(cord_sc, by = c("ciudad", "fecha"))
  
  g_scatter_cona_cord <- ggplot(scat_cona_cord,
                                aes(x = CoNA_per_capita_month, y = CoRD_per_capita_month, color = ciudad)) +
    geom_point(alpha = 0.8, size = 1.9) +
    labs(
      title = "CoNA vs CoRD: Monthly per-capita cost comparison",
      x = "CoNA (monthly per-capita cost)",
      y = "CoRD (monthly per-capita cost)"
    ) +
    color_scale +
    theme_paper
  
  ggsave(
    filename = file.path(afford_dir, "CoNA_vs_CoRD_scatter_per_capita_month.png"),
    plot     = g_scatter_cona_cord,
    width    = 7.5, height = 5.5, dpi = 300, bg = "white"
  )
}