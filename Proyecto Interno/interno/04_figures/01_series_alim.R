# ============================================================
# SERIES DIARIAS DE PRECIOS POR ALIMENTO Y CIUDAD — FACETAS
# ============================================================

library(tidyverse)
library(stringr)
library(fs)
library(scales)
library(lubridate)
library(zoo)

# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"

ruta_panel  <- file.path(base_dir, "interno/output/paneles")
ruta_series <- file.path(base_dir, "interno/output/Series alimentos")

# ============================================================
# Datos
# ============================================================

panel_balanceado <- readRDS(file.path(ruta_panel, "panel_balanceado.rds"))

base_graficas <- panel_balanceado %>%
  mutate(
    fecha      = as.Date(fecha),
    city       = str_squish(city),
    sipsa_name = str_squish(sipsa_name),
    sku_code   = as.character(sku_code),
    price      = as.numeric(price)
  ) %>%
  filter(!is.na(fecha)) %>%
  select(city, sipsa_name, sku_code, fecha, price) %>%
  group_by(city, sipsa_name, sku_code, fecha) %>%
  summarise(
    price = median(price[price > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(price = if_else(is.nan(price), NA_real_, price))

# ============================================================
# Periodo y calendario
# ============================================================

fecha_inicio <- as.Date("2025-07-01")
fecha_fin    <- as.Date("2025-09-30")

mes_labels <- c("7" = "Julio", "8" = "Agosto", "9" = "Septiembre")

base_graficas_completa <- base_graficas %>%
  group_by(city, sipsa_name, sku_code) %>%
  complete(fecha = seq.Date(fecha_inicio, fecha_fin, by = "day")) %>%
  ungroup() %>%
  mutate(
    mes = factor(mes_labels[as.character(month(fecha))],
                 levels = c("Julio", "Agosto", "Septiembre"))
  ) %>%
  arrange(city, sipsa_name, sku_code, fecha)

# ============================================================
# Carpetas de salida
# ============================================================

ciudades <- sort(unique(base_graficas_completa$city))
dir_create(ruta_series)
walk(ciudades, ~ dir_create(file.path(ruta_series, .x)))

# ============================================================
# Helpers
# ============================================================

limpiar_nombre <- function(x) {
  x %>%
    str_replace_all("[/\\\\:*?\"<>|]", "_") %>%
    str_replace_all("\\s+", "_") %>%
    str_sub(1, 120)
}

fmt_cop <- function(x) {
  paste0("$", format(round(x), big.mark = ".", decimal.mark = ","))
}

# ============================================================
# Paleta
# ============================================================

COL_BLUE  <- "#2A5FA5"
COL_CORAL <- "#C04B22"
COL_BADGE <- "#FAECE7"

# ============================================================
# Tema
# ============================================================

tema_facetas <- function() {
  theme_bw(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = "white", color = NA),
      panel.grid.major.y = element_line(color = "#EEEEEE", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(color = "#DDDDDD", fill = NA, linewidth = 0.5),
      strip.background   = element_rect(fill = "#F5F5F5", color = "#DDDDDD", linewidth = 0.5),
      strip.text         = element_text(size = 10, face = "bold", color = "#333333", margin = margin(4, 0, 4, 0)),
      axis.text          = element_text(size = 8, color = "#666666"),
      axis.title         = element_blank(),
      axis.ticks         = element_line(color = "#CCCCCC", linewidth = 0.3),
      plot.title         = element_text(size = 13, face = "bold", color = "#1A1A1A", margin = margin(b = 3)),
      plot.subtitle      = element_text(size = 8,  color = "#888888", margin = margin(b = 10)),
      plot.caption       = element_text(size = 7,  color = "#AAAAAA", hjust = 1, margin = margin(t = 8)),
      panel.spacing      = unit(6, "pt"),
      plot.margin        = margin(14, 18, 10, 14)
    )
}

# ============================================================
# Función de gráfica
# ============================================================

graficar_serie <- function(city, sipsa_name, sku_code) {
  
  df <- base_graficas_completa %>%
    filter(
      city       == !!city,
      sipsa_name == !!sipsa_name,
      sku_code   == !!sku_code
    ) %>%
    arrange(fecha) %>%
    group_by(mes) %>%
    mutate(
      gap_dias    = as.numeric(fecha - lag(fecha)),
      nuevo_tramo = if_else(is.na(price) | is.na(lag(price)) | gap_dias > 1, 1L, 0L),
      tramo       = cumsum(replace_na(nuevo_tramo, 1L))
    ) %>%
    ungroup()
  
  df_obs <- df %>% filter(!is.na(price))
  if (nrow(df_obs) == 0) return(NULL)
  
  medianas_mes <- df_obs %>%
    group_by(mes) %>%
    summarise(mediana_mes = median(price, na.rm = TRUE), .groups = "drop") %>%
    mutate(label_med = paste0("Md ", fmt_cop(mediana_mes))) %>%
    left_join(
      df %>% group_by(mes) %>% summarise(x_badge = max(fecha), .groups = "drop"),
      by = "mes"
    )
  
  y_min  <- min(df_obs$price, na.rm = TRUE)
  y_max  <- max(df_obs$price, na.rm = TRUE)
  margen <- max((y_max - y_min) * 0.22, y_max * 0.04)
  
  ggplot(df, aes(x = fecha, y = price)) +
    
    geom_hline(
      data      = medianas_mes,
      aes(yintercept = mediana_mes),
      color     = COL_CORAL,
      linewidth = 0.5,
      linetype  = "dashed",
      alpha     = 0.9
    ) +
    
    geom_label(
      data       = medianas_mes,
      aes(x = x_badge, y = y_max + margen * 0.6, label = label_med),
      size       = 2.5,
      color      = COL_CORAL,
      fill       = COL_BADGE,
      label.size = 0,
      label.r    = unit(2, "pt"),
      hjust      = 1,
      vjust      = 1
    ) +
    
    geom_line(
      aes(group = interaction(mes, tramo)),
      color     = COL_BLUE,
      linewidth = 0.8,
      alpha     = 0.9,
      na.rm     = TRUE
    ) +
    
    geom_point(
      data  = df_obs,
      color = COL_BLUE,
      size  = 1.3,
      alpha = 0.8
    ) +
    
    facet_wrap(~ mes, scales = "free_x", nrow = 1) +
    
    scale_x_date(
      date_breaks = "7 days",
      date_labels = "%d",
      expand      = expansion(mult = 0.03)
    ) +
    
    scale_y_continuous(
      limits = c(y_min - margen, y_max + margen),
      labels = function(x) paste0("$", format(round(x / 100) * 100,
                                              big.mark = ".", scientific = FALSE)),
      n.breaks = 4,
      expand   = expansion(mult = 0.01)
    ) +
    
    labs(
      title    = paste0(sipsa_name, "  ·  ", city),
      subtitle = paste0("SKU ", sku_code, "   ·   COP / unidad   ·   mediana mensual en rojo"),
      caption  = paste0(
        "n = ", nrow(df_obs), " obs.   ·   ",
        format(fecha_inicio, "%d %b %Y"), " – ", format(fecha_fin, "%d %b %Y")
      )
    ) +
    
    tema_facetas()
}

# ============================================================
# Exportar
# ============================================================

combos <- base_graficas %>%
  distinct(city, sipsa_name, sku_code)

pwalk(combos, function(city, sipsa_name, sku_code) {
  
  grafica <- graficar_serie(city, sipsa_name, sku_code)
  
  if (!is.null(grafica)) {
    ggsave(
      filename = file.path(
        ruta_series,
        city,
        paste0(limpiar_nombre(sipsa_name), "_SKU_", limpiar_nombre(sku_code), ".png")
      ),
      plot   = grafica,
      width  = 14,
      height = 5,
      dpi    = 150,
      bg     = "white"
    )
  }
})

message("Gráficas exportadas: ", nrow(combos))