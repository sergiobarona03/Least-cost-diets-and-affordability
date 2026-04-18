# =========================================================
# CoNA vs PI vs food classes in the diet composition
# =========================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(janitor)
library(fuzzyjoin)

# =========================
# 1. PATHS
# =========================

path_cona    <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/cona/230326_cona_full.xlsx"
path_panel   <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/output/tcac_food_table/panel_city_month_food_1999_2025.rds"
path_weights <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/input/cpi-weights/cpi-weights-2024.xlsx"

out_path <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/food-security-paper/input/Masters"
dir.create(out_path, showWarnings = FALSE)
dir.create(file.path(out_path, "plots_by_group"), showWarnings = FALSE)

# =========================
# 2. CLASSES / TRANSLATIONS / STYLES
# =========================

clases_ipc <- c(
  "01110000 - Pan Y Cereales",
  "01120000 - Carnes",
  "01130000 - Pescado",
  "01140000 - Leche, Queso Y Huevos",
  "01150000 - Aceites Y Grasas",
  "01160000 - Frutas",
  "01170000 - Legumbres",
  "01180000 - Azúcar, Mermelada, Miel, Chocolate Y Dulces De Azúcar"
)

traductor <- c(
  "Pan Y Cereales" = "Bread and cereals",
  "Carnes" = "Meat",
  "Pescado" = "Fish",
  "Leche, Queso Y Huevos" = "Milk, cheese and eggs",
  "Aceites Y Grasas" = "Oils and fats",
  "Frutas" = "Fruits",
  "Legumbres" = "Legumes",
  "Azúcar, Mermelada, Miel, Chocolate Y Dulces De Azúcar" = "Sugar, jam, honey, chocolate and sweets"
)

color_values <- c(
  "CoNA" = "#1B1B1B",
  "PI" = "#4D4D4D",
  "Bread and cereals" = "#1F3A5F",
  "Meat" = "#8C2D04",
  "Fish" = "#2B6C8E",
  "Milk, cheese and eggs" = "#6A4C93",
  "Oils and fats" = "#B07D00",
  "Fruits" = "#2E6F40",
  "Legumes" = "#7A3E2B",
  "Sugar, jam, honey, chocolate and sweets" = "#6B7280"
)

linetype_values <- c(
  "CoNA" = "solid",
  "PI" = "solid",
  "Bread and cereals" = "dashed",
  "Meat" = "dotted",
  "Fish" = "dotdash",
  "Milk, cheese and eggs" = "longdash",
  "Oils and fats" = "twodash",
  "Fruits" = "dashed",
  "Legumes" = "dotted",
  "Sugar, jam, honey, chocolate and sweets" = "dotdash"
)

series_order_classes <- c(
  "PI",
  "Bread and cereals",
  "Meat",
  "Fish",
  "Milk, cheese and eggs",
  "Oils and fats",
  "Fruits",
  "Legumes",
  "Sugar, jam, honey, chocolate and sweets"
)

series_order_validation <- c("CoNA", "PI")

# =========================
# 3. CLEAN FUNCTION
# =========================

clean_text <- function(x){
  x %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    stringr::str_replace_all("[[:punct:]]", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

safe_name <- function(x){
  x %>%
    as.character() %>%
    stringr::str_replace_all("[\\[\\]\\(\\), ]", "") %>%
    stringr::str_replace_all("[^A-Za-z0-9_-]", "_")
}

# =========================
# 4. CPI -> ARTICULO -> CLASE
# =========================

weights <- readxl::read_excel(path_weights) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    nivel = stringr::str_to_upper(nivel),
    nombre_clean = clean_text(nombre)
  )

article_class <- weights %>%
  dplyr::mutate(
    clase_codigo_raw = ifelse(nivel == "CLASE", codigo, NA_character_),
    clase_nombre_raw = ifelse(nivel == "CLASE", nombre, NA_character_),
    clase_nombre_clean = ifelse(nivel == "CLASE", nombre_clean, NA_character_)
  ) %>%
  tidyr::fill(clase_codigo_raw, clase_nombre_raw, clase_nombre_clean, .direction = "down") %>%
  dplyr::filter(nivel %in% c("ARTICULO", "ARTÍCULO")) %>%
  dplyr::transmute(
    articulo_clean = nombre_clean,
    clase_codigo = clase_codigo_raw,
    clase_nombre = clase_nombre_raw
  ) %>%
  dplyr::distinct()

# =========================
# 5. CONA
# =========================

cost <- readxl::read_excel(path_cona, "cost") %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    ciudad = clean_text(ciudad),
    fecha = as.Date(fecha)
  )

comp <- readxl::read_excel(path_cona, "comp") %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    ciudad = clean_text(ciudad),
    food_clean = clean_text(food),
    fecha = as.Date(fecha)
  )

# =========================
# 6. MATCH AUTOMATICO
# =========================

comp_match <- fuzzyjoin::stringdist_left_join(
  comp,
  article_class,
  by = c("food_clean" = "articulo_clean"),
  method = "jw",
  max_dist = 0.12,
  distance_col = "dist"
) %>%
  dplyr::group_by(ciudad, fecha, demo_group, sex, food, quantity) %>%
  dplyr::slice_min(dist, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

# =========================
# 7. PRECIOS
# =========================

panel <- readRDS(path_panel) %>%
  dplyr::mutate(
    ciudad = clean_text(ciudad),
    articulo_clean = clean_text(articulo),
    fecha = as.Date(fecha)
  ) %>%
  dplyr::select(ciudad, fecha, articulo_clean, precio = precio_final)

# =========================
# 8. BASE
# =========================

base <- comp_match %>%
  dplyr::filter(!is.na(clase_codigo), !is.na(clase_nombre)) %>%
  dplyr::left_join(panel, by = c("ciudad", "fecha", "articulo_clean")) %>%
  dplyr::left_join(
    cost %>% dplyr::select(ciudad, fecha, demo_group, sex, cost_day, cost_1000kcal),
    by = c("ciudad", "fecha", "demo_group", "sex")
  )

# =========================
# 9. PRECIO BASE
# =========================

precio_base_2019 <- base %>%
  dplyr::filter(fecha == as.Date("2019-01-01")) %>%
  dplyr::group_by(ciudad, articulo_clean) %>%
  dplyr::summarise(
    precio_base_2019 = dplyr::first(na.omit(precio)),
    .groups = "drop"
  )

precio_base_first <- base %>%
  dplyr::filter(!is.na(precio)) %>%
  dplyr::arrange(ciudad, articulo_clean, fecha) %>%
  dplyr::group_by(ciudad, articulo_clean) %>%
  dplyr::summarise(
    precio_base_first = dplyr::first(precio),
    .groups = "drop"
  )

base <- base %>%
  dplyr::left_join(precio_base_2019, by = c("ciudad", "articulo_clean")) %>%
  dplyr::left_join(precio_base_first, by = c("ciudad", "articulo_clean")) %>%
  dplyr::mutate(
    precio_base = dplyr::coalesce(precio_base_2019, precio_base_first)
  )

# =========================
# 10. BASE FINAL MINIMA
# =========================

base_food_final <- base %>%
  dplyr::filter(
    !is.na(precio),
    !is.na(precio_base),
    !is.na(quantity),
    !is.na(clase_codigo),
    !is.na(clase_nombre)
  ) %>%
  dplyr::transmute(
    food = food,
    quantity = quantity,
    demo_group = demo_group,
    sex = sex,
    ciudad = ciudad,
    fecha = fecha,
    food_clean = food_clean,
    articulo_clean = articulo_clean,
    clase_codigo = clase_codigo,
    clase_nombre = clase_nombre,
    dist = dist,
    precio = precio,
    cost_day = cost_day,
    cost_1000kcal = cost_1000kcal,
    precio_base_2019 = precio_base_2019,
    precio_base = precio_base
  )

openxlsx::write.xlsx(
  base_food_final,
  file.path(out_path, "base_food_final.xlsx"),
  overwrite = TRUE
)

# =========================
# 11. PI TOTAL POR GRUPO
# =========================

pi_total <- base_food_final %>%
  dplyr::group_by(ciudad, fecha, demo_group, sex) %>%
  dplyr::summarise(
    PI = sum(precio * quantity, na.rm = TRUE) / sum(precio_base * quantity, na.rm = TRUE) * 100,
    cost_day = dplyr::first(cost_day),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ciudad, demo_group, sex, fecha) %>%
  dplyr::group_by(ciudad, demo_group, sex) %>%
  dplyr::mutate(
    PI_yoy = (PI / dplyr::lag(PI, 12) - 1) * 100,
    CoNA_yoy = (cost_day / dplyr::lag(cost_day, 12) - 1) * 100
  ) %>%
  dplyr::ungroup()

openxlsx::write.xlsx(
  pi_total,
  file.path(out_path, "pi_total.xlsx"),
  overwrite = TRUE
)

# =========================
# 12. PI POR CLASE POR GRUPO
# =========================

pi_class <- base_food_final %>%
  dplyr::group_by(ciudad, fecha, demo_group, sex, clase_codigo, clase_nombre) %>%
  dplyr::summarise(
    class_index = sum(precio * quantity, na.rm = TRUE) / sum(precio_base * quantity, na.rm = TRUE) * 100,
    q_class = sum(quantity, na.rm = TRUE),
    n_foods = dplyr::n_distinct(food),
    gasto_base = sum(precio_base * quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ciudad, demo_group, sex, clase_nombre, fecha) %>%
  dplyr::group_by(ciudad, demo_group, sex, clase_codigo, clase_nombre) %>%
  dplyr::mutate(
    class_yoy = (class_index / dplyr::lag(class_index, 12) - 1) * 100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    serie = dplyr::recode(clase_nombre, !!!traductor)
  )

# =========================
# 13. PI TOTAL GENERAL
# =========================

pi_total_general <- base_food_final %>%
  dplyr::group_by(ciudad, fecha) %>%
  dplyr::summarise(
    PI = sum(precio * quantity, na.rm = TRUE) / sum(precio_base * quantity, na.rm = TRUE) * 100,
    cost_day = mean(cost_day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ciudad, fecha) %>%
  dplyr::group_by(ciudad) %>%
  dplyr::mutate(
    PI_yoy = (PI / dplyr::lag(PI, 12) - 1) * 100,
    CoNA_yoy = (cost_day / dplyr::lag(cost_day, 12) - 1) * 100
  ) %>%
  dplyr::ungroup()

openxlsx::write.xlsx(
  pi_total_general,
  file.path(out_path, "pi_total_general.xlsx"),
  overwrite = TRUE
)

# =========================
# 14. PI POR CLASE GENERAL
# =========================

pi_class_general <- base_food_final %>%
  dplyr::group_by(ciudad, fecha, clase_codigo, clase_nombre) %>%
  dplyr::summarise(
    class_index = sum(precio * quantity, na.rm = TRUE) / sum(precio_base * quantity, na.rm = TRUE) * 100,
    q_class = sum(quantity, na.rm = TRUE),
    n_foods = dplyr::n_distinct(food),
    gasto_base = sum(precio_base * quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ciudad, clase_nombre, fecha) %>%
  dplyr::group_by(ciudad, clase_codigo, clase_nombre) %>%
  dplyr::mutate(
    class_yoy = (class_index / dplyr::lag(class_index, 12) - 1) * 100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    serie = dplyr::recode(clase_nombre, !!!traductor)
  )

# =========================
# 15. CHEQUEO LEGUMES BOGOTA
# =========================

check_legumes_bogota <- pi_class %>%
  dplyr::filter(
    ciudad == "BOGOTA",
    serie == "Legumes"
  ) %>%
  dplyr::select(
    ciudad, demo_group, sex, fecha, serie,
    q_class, n_foods, gasto_base, class_index, class_yoy
  )

openxlsx::write.xlsx(
  check_legumes_bogota,
  file.path(out_path, "check_legumes_bogota.xlsx"),
  overwrite = TRUE
)

# =========================
# 16. FUNCIONES DE GRAFICAS
# =========================

plot_validation_fun <- function(pi_total_df, title_text, file_name){
  
  plot_validation <- pi_total_df %>%
    dplyr::filter(ciudad %in% c("BOGOTA", "CALI", "MEDELLIN")) %>%
    dplyr::select(ciudad, fecha, CoNA_yoy, PI_yoy) %>%
    tidyr::pivot_longer(
      cols = c(CoNA_yoy, PI_yoy),
      names_to = "serie",
      values_to = "y"
    ) %>%
    dplyr::mutate(
      serie = dplyr::recode(
        serie,
        "CoNA_yoy" = "CoNA",
        "PI_yoy" = "PI"
      )
    ) %>%
    dplyr::filter(!is.na(y))
  
  p_validation <- ggplot2::ggplot(
    plot_validation,
    ggplot2::aes(x = fecha, y = y, color = serie, linetype = serie)
  ) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggplot2::facet_wrap(~ciudad, nrow = 1, scales = "free_y") +
    ggplot2::scale_color_manual(
      values = color_values[c("CoNA", "PI")],
      breaks = series_order_validation
    ) +
    ggplot2::scale_linetype_manual(
      values = linetype_values[c("CoNA", "PI")],
      breaks = series_order_validation
    ) +
    ggplot2::labs(
      x = "fecha",
      y = "Year-on-year variation (%)",
      color = NULL,
      linetype = NULL,
      title = title_text
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  ggplot2::ggsave(
    file_name,
    p_validation,
    width = 12,
    height = 4.5,
    dpi = 300
  )
}

plot_main_fun <- function(pi_total_df, pi_class_df, title_text, file_name){
  
  plot_pi <- pi_total_df %>%
    dplyr::filter(ciudad %in% c("BOGOTA", "CALI", "MEDELLIN")) %>%
    dplyr::transmute(ciudad, fecha, serie = "PI", y = PI_yoy)
  
  plot_classes <- pi_class_df %>%
    dplyr::filter(
      ciudad %in% c("BOGOTA", "CALI", "MEDELLIN"),
      clase_codigo %in% sub(" -.*", "", clases_ipc)
    ) %>%
    dplyr::transmute(ciudad, fecha, serie = serie, y = class_yoy) %>%
    dplyr::filter(!is.na(y))
  
  plot_main <- dplyr::bind_rows(plot_pi, plot_classes) %>%
    dplyr::mutate(
      serie = factor(serie, levels = series_order_classes)
    )
  
  p_main <- ggplot2::ggplot(
    plot_main,
    ggplot2::aes(x = fecha, y = y, color = serie, linetype = serie)
  ) +
    ggplot2::geom_line(linewidth = 0.95, na.rm = TRUE) +
    ggplot2::facet_wrap(~ciudad, nrow = 1, scales = "free_y") +
    ggplot2::scale_color_manual(
      values = color_values,
      breaks = series_order_classes,
      drop = TRUE
    ) +
    ggplot2::scale_linetype_manual(
      values = linetype_values,
      breaks = series_order_classes,
      drop = TRUE
    ) +
    ggplot2::labs(
      x = "fecha",
      y = "Year-on-year variation (%)",
      color = NULL,
      linetype = NULL,
      title = title_text
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  ggplot2::ggsave(
    file_name,
    p_main,
    width = 15,
    height = 4.8,
    dpi = 300
  )
}

# =========================
# 17. GRAFICAS GENERALES
# =========================

plot_validation_fun(
  pi_total_df = pi_total_general,
  title_text = "CoNA vs PI - General",
  file_name = file.path(out_path, "pi_vs_cona_yoy_general.png")
)

plot_main_fun(
  pi_total_df = pi_total_general,
  pi_class_df = pi_class_general,
  title_text = "PI and CPI food classes in the diet composition - General",
  file_name = file.path(out_path, "pi_classes_yoy_general.png")
)

# =========================
# 18. GRAFICAS POR GRUPO
# =========================

group_grid <- pi_total %>%
  dplyr::distinct(demo_group, sex) %>%
  dplyr::arrange(demo_group, sex)

for(i in seq_len(nrow(group_grid))){
  
  demo_i <- group_grid$demo_group[i]
  sex_i  <- group_grid$sex[i]
  
  tag_i <- paste0("group_", safe_name(demo_i), "_sex_", safe_name(sex_i))
  
  pi_total_i <- pi_total %>%
    dplyr::filter(demo_group == demo_i, sex == sex_i)
  
  pi_class_i <- pi_class %>%
    dplyr::filter(demo_group == demo_i, sex == sex_i)
  
  plot_validation_fun(
    pi_total_df = pi_total_i,
    title_text = paste0("CoNA vs PI - demo_group: ", demo_i, " | sex: ", sex_i),
    file_name = file.path(out_path, "plots_by_group", paste0("pi_vs_cona_yoy_", tag_i, ".png"))
  )
  
  plot_main_fun(
    pi_total_df = pi_total_i,
    pi_class_df = pi_class_i,
    title_text = paste0("PI and CPI food classes - demo_group: ", demo_i, " | sex: ", sex_i),
    file_name = file.path(out_path, "plots_by_group", paste0("pi_classes_yoy_", tag_i, ".png"))
  )
}