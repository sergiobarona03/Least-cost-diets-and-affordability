# =========================
# SERIES DE TIEMPO - SIPSA
# =========================

library(dplyr)
library(tidyverse)
library(lubridate)
library(fpp3)
library(stringi)

# -------------------------
# Helpers
# -------------------------
safe <- function(x) {
  gsub("[^A-Za-z0-9]+", "_",
       stringi::stri_trans_general(x, "Latin-ASCII"))
}

theme_clean <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}

# -------------------------
# Rutas
# -------------------------
base_dir   <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/Precios al por mayor"
output_dir <- file.path(base_dir, "Graficas")
panel_dir  <- file.path(output_dir, "panel")
ind_dir    <- file.path(output_dir, "individuales")

dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ind_dir,   recursive = TRUE, showWarnings = FALSE)

path_bases <- file.path(base_dir, "Bases historicas")

# -------------------------
# Cargar bases
# -------------------------
files_rds <- list.files(path_bases, pattern = "\\.rds$", full.names = TRUE)
dataset <- files_rds %>%lapply(readRDS) %>%bind_rows()

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
    
    Grupo    = stri_trans_general(Grupo, "Latin-ASCII") %>%toupper() %>%trimws(),
    Alimento = as.character(Alimento)
  ) %>%
  filter(
    !is.na(Fecha),
    !is.na(Precio_kg),
    Ciudad %in% c("Bogota", "Medellin", "Cali")
  )

# -------------------------
# Exploracion grupos
# -------------------------
resumen_grupos <- dataset %>%
  distinct(Grupo, Alimento) %>%
  count(Grupo, name = "n_alimentos") %>%
  arrange(n_alimentos)

grupos_objetivo <- resumen_grupos %>%slice(1:2) %>%pull(Grupo)

# -------------------------
# Series mensuales
# -------------------------
dataset_ts <- dataset %>%
  mutate(Year_Month = yearmonth(Fecha)) %>%
  group_by(Year_Month, Ciudad, Grupo, Alimento) %>%
  summarise(
    Precio_kg_prom = mean(Precio_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Grupo %in% grupos_objetivo)

# -------------------------
# Top N alimentos
# -------------------------
N <- 8

top_foods <- dataset_ts %>%
  group_by(Ciudad, Grupo, Alimento) %>%
  summarise(score = mean(Precio_kg_prom), .groups = "drop") %>%
  group_by(Ciudad, Grupo) %>%
  slice_max(score, n = N, with_ties = FALSE)

tsibble_top <- dataset_ts %>%
  semi_join(top_foods, by = c("Ciudad","Grupo","Alimento")) %>%
  mutate(Serie = paste(Ciudad, Grupo, Alimento, sep = " / ")) %>%
  as_tsibble(
    key   = c(Serie, Ciudad, Grupo, Alimento),
    index = Year_Month
  ) %>%
  group_by_key() %>%
  fill_gaps(Precio_kg_prom = NA) %>%
  ungroup()

# =========================
# GRAFICAS PANEL
# =========================

pal_panel <- rep(
  c("red","blue","darkgreen","orange","purple","brown",
    "darkcyan","gold","deeppink","darkolivegreen"),
  length.out = n_distinct(tsibble_top$Serie)
)

# Lineas panel
p_panel_lineas <- autoplot(tsibble_top, Precio_kg_prom) +
  facet_grid(Grupo ~ Ciudad, scales = "free_y") +
  scale_colour_manual(values = pal_panel) +
  labs(
    title = paste0("Precio promedio mensual (Top ", N, " alimentos)"),
    x = "Mes",
    y = "Precio por kg"
  ) +
  theme_clean() +
  theme(legend.position = "none")

ggsave(
  filename = file.path(panel_dir, "lineas_panel.png"),
  plot     = p_panel_lineas,
  width    = 14,
  height   = 8,
  dpi      = 300
)

# Season panel
p_panel_season <- tsibble_top %>%
  gg_season(Precio_kg_prom, period = "year") +
  facet_wrap(~ Ciudad + Grupo, scales = "free_y") +
  labs(title = "Estacionalidad mensual (panel)") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(panel_dir, "season_panel.png"),
  plot     = p_panel_season,
  width    = 14,
  height   = 8,
  dpi      = 300
)

# Subseries panel
p_panel_sub <- tsibble_top %>%
  gg_subseries(Precio_kg_prom) +
  facet_wrap(~ Ciudad + Grupo, scales = "free_y") +
  labs(title = "Subseries mensuales (panel)") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(panel_dir, "subseries_panel.png"),
  plot     = p_panel_sub,
  width    = 14,
  height   = 8,
  dpi      = 300
)

# =========================
# GRAFICAS INDIVIDUALES
# =========================

combos <- tsibble_top %>%distinct(Ciudad, Grupo)

for (i in seq_len(nrow(combos))) {
  
  c_i <- combos$Ciudad[i]
  g_i <- combos$Grupo[i]
  
  subdir <- file.path(
    ind_dir,
    paste0("Ciudad_", safe(c_i), "__Grupo_", safe(g_i))
  )
  dir.create(subdir, showWarnings = FALSE)
  
  tsi  <- tsibble_top %>%filter(Ciudad == c_i, Grupo == g_i)
  subt <- paste0("Grupo: ", g_i, " - Top ", N, " alimentos")
  
  # Lineas individuales
  p1 <- ggplot(tsi, aes(Year_Month, Precio_kg_prom, color = Alimento)) +
    geom_line(linewidth = 0.9, alpha = 0.85) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = paste0("Precio mensual - ", c_i),
      subtitle = subt,
      x = "Anio",
      y = "Precio por kg"
    ) +
    theme_clean() +
    theme(legend.title = element_blank())
  
  ggsave(
    filename = file.path(subdir, "lineas_precio.png"),
    plot     = p1,
    width    = 12,
    height   = 7,
    dpi      = 300
  )
  
  # Season por alimento
  for (ali in unique(tsi$Alimento)) {
    
    p2 <- tsi %>%
      filter(Alimento == ali) %>%
      gg_season(Precio_kg_prom, period = "year") +
      labs(
        title = paste("Estacionalidad -", ali),
        subtitle = paste(c_i, "/", g_i)
      ) +
      theme_clean() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(
      filename = file.path(subdir, paste0("season_", safe(ali), ".png")),
      plot     = p2,
      width    = 9,
      height   = 5,
      dpi      = 300
    )
  }
  
  # Subseries por alimento
  for (ali in unique(tsi$Alimento)) {
    
    p3 <- tsi %>%
      filter(Alimento == ali) %>%
      gg_subseries(Precio_kg_prom) +
      labs(
        title = paste("Subseries -", ali),
        subtitle = paste(c_i, "/", g_i)
      ) +
      theme_clean() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(
      filename = file.path(subdir, paste0("subseries_", safe(ali), ".png")),
      plot     = p3,
      width    = 9,
      height   = 5,
      dpi      = 300
    )
  }
}

message("Graficas guardadas en: ", output_dir)
