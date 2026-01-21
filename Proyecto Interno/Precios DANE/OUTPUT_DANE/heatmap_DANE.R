# ============================================================
# MATRICES / HEATMAPS - DANE (precios minoristas deflactados)
# ============================================================

library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(fpp3)
library(stringi)
library(scales)
library(stringr)
library(forcats)

# ---------------- HELPERS ----------------
safe <- function(x) gsub("[^A-Za-z0-9]+", "_", stringi::stri_trans_general(x, "Latin-ASCII"))

clean_key <- function(x){
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- str_squish(x)
  x
}

# label corto tipo SIPSA (1 palabra o 2 segun convenga)
grupo_short_sipsa <- function(grupo){
  g <- clean_key(grupo)
  dplyr::case_when(
    str_detect(g, "VERDUR") ~ "VERDURAS",
    str_detect(g, "HORTAL") ~ "VERDURAS",
    str_detect(g, "TUBERC") ~ "TUBERCULOS",
    str_detect(g, "RAICES") ~ "TUBERCULOS",
    str_detect(g, "PLATAN") ~ "TUBERCULOS",
    str_detect(g, "PROCES") ~ "PROCESADOS",
    str_detect(g, "PESCAD") ~ "PESCADOS",
    str_detect(g, "LACT")   ~ "LACTEOS",
    str_detect(g, "HUEV")   ~ "LACTEOS",
    str_detect(g, "GRANO")  ~ "GRANOS",
    str_detect(g, "CEREAL") ~ "GRANOS",
    str_detect(g, "FRUT")   ~ "FRUTAS",
    str_detect(g, "CARN")   ~ "CARNES",
    TRUE ~ g
  )
}

# ---------------- paths ----------------
path_xlsx <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/precios_DANE_deflactados_base2018_12_con_grupo.xlsx"

panel_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios DANE/OUTPUT_DANE/heatmaps"
dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------- config ----------------
CIUDADES_OBJ <- c("Bogota", "Medellin", "Cali")
N_ALIM <- 5  # top N por grupo-ciudad

YM_MIN <- yearmonth("2013 Jan")
YM_MAX <- yearmonth("2018 Dec")

GAP_BETWEEN_GROUPS <- 1.3
DRAW_LINES <- FALSE

# ---------------- leer datos (hoja buena) ----------------
sheets <- excel_sheets(path_xlsx)
sheet_use <- if ("precios_deflactados" %in% sheets) {
  "precios_deflactados"
} else if ("dane_defl_con_grupoSIPSA" %in% sheets) {
  "dane_defl_con_grupoSIPSA"
} else {
  sheets[1]
}

dane <- read_excel(path_xlsx, sheet = sheet_use)

# ---------------- preparar time series ----------------
dane_ts <- dane %>%
  mutate(
    Year_Month = yearmonth(paste0(as.character(anio_mes), "-01")),
    Ciudad = stringi::stri_trans_general(as.character(nombre_ciudad), "Latin-ASCII") %>%
      toupper() %>% str_squish(),
    Ciudad = case_when(
      str_detect(Ciudad, "^BOGOTA") ~ "Bogota",
      str_detect(Ciudad, "^MEDELLIN") ~ "Medellin",
      str_detect(Ciudad, "^CALI") ~ "Cali",
      TRUE ~ Ciudad
    ),
    # >>> NO TOCAR NOMBRES <<<
    Grupo    = as.character(grupo_sipsa),
    Alimento = as.character(articulo),
    Precio   = suppressWarnings(as.numeric(precio_real_base2018_12))
  ) %>%
  filter(
    !is.na(Year_Month),
    Year_Month >= YM_MIN, Year_Month <= YM_MAX,
    Ciudad %in% CIUDADES_OBJ,
    !is.na(Grupo), Grupo != "",
    !is.na(Alimento), Alimento != "",
    !is.na(Precio), is.finite(Precio)
  ) %>%
  group_by(Year_Month, Ciudad, Grupo, Alimento) %>%
  summarise(Precio_prom = mean(Precio, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Grupo_short = grupo_short_sipsa(Grupo)
  )

# ---------------- seleccionar alimentos TOP por completitud ----------------
foods_pick <- dane_ts %>%
  group_by(Ciudad, Grupo, Grupo_short, Alimento) %>%
  summarise(n_meses = n_distinct(Year_Month), .groups = "drop") %>%
  group_by(Ciudad, Grupo) %>%
  slice_max(order_by = n_meses, n = N_ALIM, with_ties = FALSE) %>%
  ungroup() %>%
  select(Ciudad, Grupo, Alimento)

set.seed(123)
heat_data <- dane_ts %>%
  semi_join(foods_pick, by = c("Ciudad", "Grupo", "Alimento")) %>%
  group_by(Ciudad, Grupo) %>%
  mutate(.ord_food = sample.int(n_distinct(Alimento))[match(Alimento, unique(Alimento))]) %>%
  ungroup() %>%
  arrange(Ciudad, Grupo, .ord_food, Alimento)

# ============================================================
# Helpers ejes y orden Y (idéntico SIPSA)
# ============================================================
make_month_breaks <- function(x, step_months = 12) {
  xmin <- as.Date(min(as.Date(x), na.rm = TRUE))
  xmax <- as.Date(max(as.Date(x), na.rm = TRUE))
  yearmonth(seq.Date(xmin, xmax, by = paste0(step_months, " months")))
}

prep_city <- function(df_city, gap = 1.2) {
  ord_foods <- df_city %>%
    distinct(Grupo, Grupo_short, Alimento, .ord_food) %>%
    arrange(Grupo, .ord_food, Alimento) %>%
    group_by(Grupo, Grupo_short) %>%
    mutate(.i_in_group = row_number(),
           .n_in_group = n()) %>%
    ungroup() %>%
    mutate(.g_rank = dense_rank(Grupo))
  
  ord_foods <- ord_foods %>%
    mutate(
      y_base = row_number(),
      y = y_base + (.g_rank - 1) * gap
    )
  
  map_y <- ord_foods %>%
    select(Grupo, Alimento, y, Grupo_short, .g_rank, .n_in_group)
  
  df_city2 <- df_city %>%
    left_join(map_y, by = c("Grupo", "Alimento"))
  
  group_pos <- map_y %>%
    group_by(Grupo, Grupo_short) %>%
    summarise(y_mid = mean(y), .groups = "drop")
  
  group_bounds <- map_y %>%
    group_by(Grupo) %>%
    summarise(y_min = min(y) - 0.5, y_max = max(y) + 0.5, .groups = "drop") %>%
    arrange(Grupo) %>%
    mutate(y_line = lag(y_min)) %>%
    filter(!is.na(y_line))
  
  list(
    df = df_city2,
    breaks_y = map_y$y,
    labels_y = map_y$Alimento,
    group_pos = group_pos,
    bounds = group_bounds
  )
}

# ============================================================
# PLOTS
# ============================================================
plot_heat_continuo <- function(ciudad_obj) {
  df0 <- heat_data %>% filter(Ciudad == ciudad_obj)
  if (nrow(df0) == 0) return(invisible(NULL))
  
  pre <- prep_city(df0, gap = GAP_BETWEEN_GROUPS)
  df  <- pre$df
  gp  <- pre$group_pos
  bd  <- pre$bounds
  
  x_min <- min(df$Year_Month, na.rm = TRUE)
  x_max <- max(df$Year_Month, na.rm = TRUE)
  x_lab <- x_max + 2
  
  br_2yr <- make_month_breaks(df$Year_Month, 24)
  
  p <- ggplot(df, aes(x = Year_Month, y = y, fill = Precio_prom)) +
    geom_tile(color = "grey10", linewidth = 0.30) +
    geom_text(
      data = gp,
      aes(x = x_lab, y = y_mid, label = Grupo_short),
      inherit.aes = FALSE,
      angle = -90, hjust = 0.5, vjust = 0.5,
      fontface = "bold", size = 3.8
    ) +
    scale_fill_viridis_c(
      option = "turbo",
      na.value = "white",
      name = "Precio real/kg (base 2018-12)",
      labels = comma
    ) +
    scale_x_yearmonth(breaks = br_2yr) +
    scale_y_continuous(
      breaks = pre$breaks_y,
      labels = pre$labels_y,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(title = paste("Matriz de precios mensuales –", ciudad_obj), x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 90, size = 8),
      plot.margin = margin(10, 190, 10, 10),
      legend.position = "right"
    ) +
    coord_cartesian(xlim = c(x_min, x_lab), clip = "off")
  
  if (isTRUE(DRAW_LINES)) {
    br_year <- make_month_breaks(df$Year_Month, 12)
    p <- p +
      geom_vline(xintercept = as.numeric(br_year), linewidth = 0.40, alpha = 0.95) +
      geom_vline(xintercept = as.numeric(br_2yr),  linewidth = 0.80, alpha = 1) +
      geom_hline(data = bd, aes(yintercept = y_line), inherit.aes = FALSE, linewidth = 1.10)
  }
  
  ggsave(
    filename = file.path(panel_dir, paste0("heatmap_DANE_", safe(ciudad_obj), "_cont_2013_2018.png")),
    plot = p, width = 19, height = 11.5, dpi = 300
  )
}

plot_heat_cuartiles <- function(ciudad_obj) {
  df0 <- heat_data %>% filter(Ciudad == ciudad_obj)
  if (nrow(df0) == 0) return(invisible(NULL))
  
  pre <- prep_city(df0, gap = GAP_BETWEEN_GROUPS)
  df  <- pre$df
  gp  <- pre$group_pos
  bd  <- pre$bounds
  
  x_min <- min(df$Year_Month, na.rm = TRUE)
  x_max <- max(df$Year_Month, na.rm = TRUE)
  x_lab <- x_max + 2
  
  br_2yr <- make_month_breaks(df$Year_Month, 24)
  
  # Cuartiles por alimento dentro de cada grupo (igual SIPSA)
  df <- df %>%
    group_by(Ciudad, Grupo, Alimento) %>%
    mutate(
      Q = if_else(is.na(Precio_prom), NA_character_, as.character(ntile(Precio_prom, 4))),
      Q = factor(Q, levels = c("1","2","3","4"), labels = c("Q1","Q2","Q3","Q4"))
    ) %>%
    ungroup()
  
  p <- ggplot(df, aes(x = Year_Month, y = y, fill = Q)) +
    geom_tile(color = "grey10", linewidth = 0.30) +
    geom_text(
      data = gp,
      aes(x = x_lab, y = y_mid, label = Grupo_short),
      inherit.aes = FALSE,
      angle = -90, hjust = 0.5, vjust = 0.5,
      fontface = "bold", size = 3.8
    ) +
    scale_fill_manual(
      values = c("Q1" = "#2D004B", "Q2" = "#5A189A", "Q3" = "#E11D48", "Q4" = "#F59E0B"),
      name = "Cuartil",
      na.value = "white",
      na.translate = FALSE
    ) +
    scale_x_yearmonth(breaks = br_2yr) +
    scale_y_continuous(
      breaks = pre$breaks_y,
      labels = pre$labels_y,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(title = paste("Matriz de cuartiles –", ciudad_obj), x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 90, size = 8),
      plot.margin = margin(10, 190, 10, 10),
      legend.position = "right"
    ) +
    coord_cartesian(xlim = c(x_min, x_lab), clip = "off")
  
  if (isTRUE(DRAW_LINES)) {
    br_year <- make_month_breaks(df$Year_Month, 12)
    p <- p +
      geom_vline(xintercept = as.numeric(br_year), linewidth = 0.40, alpha = 0.95) +
      geom_vline(xintercept = as.numeric(br_2yr),  linewidth = 0.80, alpha = 1) +
      geom_hline(data = bd, aes(yintercept = y_line), inherit.aes = FALSE, linewidth = 1.10)
  }
  
  ggsave(
    filename = file.path(panel_dir, paste0("heatmap_DANE_", safe(ciudad_obj), "_quart_2013_2018.png")),
    plot = p, width = 19, height = 11.5, dpi = 300
  )
}

# ---------------- run ----------------
for (c in CIUDADES_OBJ) {
  plot_heat_continuo(c)
  plot_heat_cuartiles(c)
}

message("✅ Heatmaps DANE guardados en: ", panel_dir)
