# =========================
# SERIES DE TIEMPO - SIPSA
# =========================

library(dplyr)
library(tidyverse)
library(lubridate)
library(fpp3)
library(stringi)
library(scales)
library(forcats)

safe <- function(x) gsub("[^A-Za-z0-9]+", "_", stringi::stri_trans_general(x, "Latin-ASCII"))
first_word <- function(x) sub(" .*", "", x)

base_dir   <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios al por mayor"
output_dir <- file.path(base_dir, "Graficas")
panel_dir  <- file.path(output_dir, "heatmap")
dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)

path_bases <- file.path(base_dir, "Bases historicas")
files_rds  <- list.files(path_bases, pattern = "\\.rds$", full.names = TRUE)
dataset    <- files_rds %>% lapply(readRDS) %>% bind_rows()

dataset <- dataset %>%
  mutate(
    Fecha     = as.Date(Fecha),
    Precio_kg = as.numeric(Precio_kg),
    Ciudad = trimws(sub(",.*", "", Mercado)),
    Ciudad = stri_trans_general(Ciudad, "Latin-ASCII"),
    Ciudad = case_when(
      Ciudad %in% c("Bogota", "Bogota D.C.", "Bogota, D.C.") ~ "Bogota",
      TRUE ~ Ciudad
    ),
    Grupo    = stri_trans_general(Grupo, "Latin-ASCII") %>% toupper() %>% trimws(),
    Alimento = as.character(Alimento)
  ) %>%
  filter(!is.na(Fecha), !is.na(Precio_kg), Ciudad %in% c("Bogota", "Medellin", "Cali"))

dataset_ts <- dataset %>%
  mutate(Year_Month = yearmonth(Fecha)) %>%
  group_by(Year_Month, Ciudad, Grupo, Alimento) %>%
  summarise(Precio_kg_prom = mean(Precio_kg, na.rm = TRUE), .groups = "drop") %>%
  mutate(Grupo_short = first_word(Grupo))

set.seed(123)
CIUDADES_OBJ <- c("Bogota", "Medellin", "Cali")
N_ALIM <- 5

foods_random <- dataset_ts %>%
  distinct(Ciudad, Grupo, Grupo_short, Alimento) %>%
  filter(Ciudad %in% CIUDADES_OBJ) %>%
  group_by(Ciudad, Grupo) %>%
  group_modify(~{
    k <- min(N_ALIM, nrow(.x))
    dplyr::slice_sample(.x, n = k, replace = FALSE)
  }) %>%
  ungroup()

heat_data <- dataset_ts %>%
  semi_join(foods_random, by = c("Ciudad", "Grupo", "Alimento")) %>%
  group_by(Ciudad, Grupo) %>%
  mutate(.ord_food = sample.int(n_distinct(Alimento))[match(Alimento, unique(Alimento))]) %>%
  ungroup() %>%
  arrange(Ciudad, Grupo, .ord_food, Alimento)

make_month_breaks <- function(x, step_months = 12) {
  xmin <- as.Date(min(x, na.rm = TRUE))
  xmax <- as.Date(max(x, na.rm = TRUE))
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
  
  breaks_y <- map_y$y
  labels_y <- map_y$Alimento
  
  group_pos <- map_y %>%
    group_by(Grupo, Grupo_short) %>%
    summarise(y_mid = mean(y), .groups = "drop")
  
  group_bounds <- map_y %>%
    group_by(Grupo) %>%
    summarise(y_min = min(y) - 0.5, y_max = max(y) + 0.5, .groups = "drop") %>%
    arrange(Grupo) %>%
    mutate(y_line = lag(y_min)) %>%
    filter(!is.na(y_line))
  
  list(df = df_city2, breaks_y = breaks_y, labels_y = labels_y, group_pos = group_pos, bounds = group_bounds)
}

GAP_BETWEEN_GROUPS <- 1.3
DRAW_LINES <- FALSE

plot_heat_continuo <- function(ciudad_obj) {
  df0 <- heat_data %>% filter(Ciudad == ciudad_obj)
  pre <- prep_city(df0, gap = GAP_BETWEEN_GROUPS)
  df  <- pre$df
  gp  <- pre$group_pos
  bd  <- pre$bounds
  
  x_min <- min(df$Year_Month, na.rm = TRUE)
  x_max <- max(df$Year_Month, na.rm = TRUE)
  x_lab <- x_max + 2
  
  br_2yr  <- make_month_breaks(df$Year_Month, 24)
  
  p <- ggplot(df, aes(x = Year_Month, y = y, fill = Precio_kg_prom)) +
    geom_tile(color = "grey10", linewidth = 0.30) +
    geom_text(
      data = gp,
      aes(x = x_lab, y = y_mid, label = Grupo_short),
      inherit.aes = FALSE,
      angle = -90,
      hjust = 0.5,
      vjust = 0.5,
      fontface = "bold",
      size = 3.8
    ) +
    scale_fill_viridis_c(
      option = "turbo",
      na.value = "white",
      name = "Precio/kg",
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
    filename = file.path(panel_dir, paste0("heatmap_", ciudad_obj, "_cont.png")),
    plot = p, width = 19, height = 11.5, dpi = 300
  )
}

plot_heat_cuartiles <- function(ciudad_obj) {
  df0 <- heat_data %>% filter(Ciudad == ciudad_obj)
  pre <- prep_city(df0, gap = GAP_BETWEEN_GROUPS)
  df  <- pre$df
  gp  <- pre$group_pos
  bd  <- pre$bounds
  
  x_min <- min(df$Year_Month, na.rm = TRUE)
  x_max <- max(df$Year_Month, na.rm = TRUE)
  x_lab <- x_max + 2
  
  br_2yr  <- make_month_breaks(df$Year_Month, 24)
  
  # Cuartiles por alimento dentro de cada grupo:
  df <- df %>%
    group_by(Ciudad, Grupo, Alimento) %>%
    mutate(
      Q = if_else(is.na(Precio_kg_prom), NA_character_, as.character(ntile(Precio_kg_prom, 4))),
      Q = factor(Q, levels = c("1","2","3","4"), labels = c("Q1","Q2","Q3","Q4"))
    ) %>%
    ungroup()
  
  p <- ggplot(df, aes(x = Year_Month, y = y, fill = Q)) +
    geom_tile(color = "grey10", linewidth = 0.30) +
    geom_text(
      data = gp,
      aes(x = x_lab, y = y_mid, label = Grupo_short),
      inherit.aes = FALSE,
      angle = -90,
      hjust = 0.5,
      vjust = 0.5,
      fontface = "bold",
      size = 3.8
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
    filename = file.path(panel_dir, paste0("heatmap_", ciudad_obj, "_quart.png")),
    plot = p, width = 19, height = 11.5, dpi = 300
  )
}

for (c in CIUDADES_OBJ) {
  plot_heat_continuo(c)
  plot_heat_cuartiles(c)
}

message("Heatmaps guardados en: ", panel_dir)
