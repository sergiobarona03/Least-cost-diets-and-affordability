# ============================================================
# SERIES DIARIAS DE PRECIOS POR ALIMENTO Y CIUDAD
# ============================================================

library(tidyverse)
library(stringr)
library(fs)
library(scales)
library(lubridate)
library(zoo)

# Rutas 
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"

ruta_panel  <- file.path(base_dir, "interno/output/paneles")
ruta_series <- file.path(base_dir, "interno/output/Series alimentos")

# Datos 
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
  mutate(
    price = if_else(is.nan(price), NA_real_, price)
  )

# Fechas completas del periodo
fecha_inicio <- as.Date("2025-07-01")
fecha_fin    <- as.Date("2025-09-30")

calendario <- tibble(
  fecha = seq.Date(fecha_inicio, fecha_fin, by = "day")
)

# Completar calendario para cada alimento-ciudad
base_graficas_completa <- base_graficas %>%
  group_by(city, sipsa_name, sku_code) %>%
  complete(
    fecha = seq.Date(fecha_inicio, fecha_fin, by = "day")
  ) %>%
  ungroup() %>%
  arrange(city, sipsa_name, sku_code, fecha)

# Carpetas de salida 
ciudades <- sort(unique(base_graficas_completa$city))

dir_create(ruta_series)

walk(ciudades, ~ dir_create(file.path(ruta_series, .x)))

# Helpers 
limpiar_nombre <- function(x) {
  x %>%
    str_replace_all("[/\\\\:*?\"<>|]", "_") %>%
    str_replace_all("\\s+", "_") %>%
    str_sub(1, 120)
}

tema_series <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background    = element_rect(fill = "#FAFAFA", color = NA),
      panel.background   = element_rect(fill = "#FFFFFF", color = NA),
      panel.border       = element_rect(color = "#DDDDDD", fill = NA, linewidth = 0.5),
      panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title    = element_text(face = "bold", size = 14, color = "#1A1A2E", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, color = "#555577", margin = margin(b = 10)),
      plot.caption  = element_text(size = 8, color = "#999999", hjust = 1, margin = margin(t = 8)),
      axis.title    = element_text(size = 10, color = "#444444"),
      axis.text     = element_text(size = 9, color = "#555555"),
      axis.ticks    = element_line(color = "#CCCCCC", linewidth = 0.3),
      plot.margin   = margin(16, 20, 12, 16)
    )
}

# Función de grafica
graficar_serie <- function(city, sipsa_name, sku_code) {
  
  df <- base_graficas_completa %>%
    filter(
      city == !!city,
      sipsa_name == !!sipsa_name,
      sku_code == !!sku_code
    ) %>%
    arrange(fecha) %>%
    mutate(
      obs = !is.na(price),
      fecha_anterior = lag(fecha),
      gap_dias = as.numeric(fecha - fecha_anterior),
      nuevo_tramo = if_else(is.na(price) | is.na(lag(price)) | gap_dias > 1, 1, 0),
      tramo = cumsum(replace_na(nuevo_tramo, 1))
    )
  
  df_obs <- df %>%
    filter(!is.na(price))
  
  if (nrow(df_obs) == 0) {
    return(NULL)
  }
  
  precio_med <- median(df_obs$price, na.rm = TRUE)
  
  y_min <- min(df_obs$price, na.rm = TRUE)
  y_max <- max(df_obs$price, na.rm = TRUE)
  
  margen_y <- max((y_max - y_min) * 0.20, y_max * 0.03)
  
  ggplot(df, aes(x = fecha, y = price)) +
    
    geom_hline(
      yintercept = precio_med,
      linetype = "dashed",
      color = "#FF6B6B",
      linewidth = 0.45,
      alpha = 0.7
    ) +
    
    geom_line(
      aes(group = tramo),
      color = "#4C6EF5",
      linewidth = 0.75,
      alpha = 0.90,
      na.rm = TRUE
    ) +
    
    geom_point(
      data = df_obs,
      color = "#4C6EF5",
      size = 1.5,
      alpha = 0.85
    ) +
    
    scale_x_date(
      limits = c(fecha_inicio, fecha_fin),
      date_breaks = "15 days",
      date_labels = "%d\n%b",
      expand = expansion(mult = 0.01)
    ) +
    
    scale_y_continuous(
      limits = c(y_min - margen_y, y_max + margen_y),
      labels = label_number(big.mark = ".", decimal.mark = ","),
      expand = expansion(mult = 0.02)
    ) +
    
    labs(
      title = paste0(sipsa_name, " · ", city),
      subtitle = paste0(
        "SKU: ", sku_code,
        "   |   Mediana: $",
        format(round(precio_med), big.mark = ".", decimal.mark = ",")
      ),
      x = NULL,
      y = "Precio diario (COP)",
      caption = paste0(
        "n = ", nrow(df_obs), " obs.   ·   ",
        format(fecha_inicio, "%d %b %Y"), " – ",
        format(fecha_fin, "%d %b %Y"),
        "   ·   Puntos = días observados"
      )
    ) +
    
    tema_series()
}

# Exportar 
combos <- base_graficas %>%
  distinct(city, sipsa_name, sku_code)

pwalk(combos, function(city, sipsa_name, sku_code) {
  
  grafica <- graficar_serie(city, sipsa_name, sku_code)
  
  if (!is.null(grafica)) {
    ggsave(
      filename = file.path(
        ruta_series,
        city,
        paste0(
          limpiar_nombre(sipsa_name),
          "_SKU_",
          limpiar_nombre(sku_code),
          ".png"
        )
      ),
      plot   = grafica,
      width  = 11,
      height = 5,
      dpi    = 150,
      bg     = "white"
    )
  }
})

message("Gráficas exportadas: ", nrow(combos))
