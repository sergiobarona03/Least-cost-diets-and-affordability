# ============================================================
# SERIES DIARIAS DE PRECIOS POR ALIMENTO Y CIUDAD
# ============================================================

library(tidyverse)
library(stringr)
library(fs)
library(scales)
library(lubridate)

# Rutas 
base_dir    <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"
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
  filter(!is.na(fecha), !is.na(price), price > 0) %>%
  select(city, sipsa_name, sku_code, fecha, price) %>%
  group_by(city, sipsa_name, sku_code, fecha) %>%
  summarise(price = median(price, na.rm = TRUE), .groups = "drop")

# Carpetas de salida 
ciudades <- sort(unique(base_graficas$city))
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
      axis.text     = element_text(size = 9,  color = "#555555"),
      axis.ticks    = element_line(color = "#CCCCCC", linewidth = 0.3),
      plot.margin   = margin(16, 20, 12, 16)
    )
}

# Función de graficación 
graficar_serie <- function(city, sipsa_name, sku_code) {
  
  df <- base_graficas %>%
    filter(city == !!city, sipsa_name == !!sipsa_name, sku_code == !!sku_code) %>%
    arrange(fecha)
  
  n_meses  <- interval(min(df$fecha), max(df$fecha)) %/% months(1)
  breaks_x <- case_when(n_meses <= 12 ~ "1 month", n_meses <= 36 ~ "3 months", TRUE ~ "6 months")
  label_x  <- if (n_meses <= 24) "%b\n%Y" else "%b %Y"
  
  precio_med <- median(df$price, na.rm = TRUE)
  y_min      <- min(df$price, na.rm = TRUE) * 0.97
  y_max      <- max(df$price, na.rm = TRUE) * 1.03
  
  ggplot(df, aes(x = fecha, y = price)) +
    geom_hline(yintercept = precio_med,
               linetype = "dashed", color = "#FF6B6B", linewidth = 0.45, alpha = 0.8) +
    geom_line(color = "#4C6EF5", linewidth = 0.75, alpha = 0.95) +   # sin puntos — serie diaria densa
    scale_x_date(date_breaks = breaks_x, date_labels = label_x,
                 expand = expansion(mult = 0.01)) +
    scale_y_continuous(
      limits = c(y_min, y_max),
      labels = label_number(big.mark = ".", decimal.mark = ","),
      expand = expansion(mult = 0)
    ) +
    labs(
      title    = paste0(sipsa_name, " · ", city),
      subtitle = paste0("SKU: ", sku_code, "   |   Mediana: $",
                        format(round(precio_med), big.mark = ".", decimal.mark = ",")),
      x        = NULL,
      y        = "Precio diario (COP)",
      caption  = paste0("n = ", nrow(df), " obs.   ·   ",
                        format(min(df$fecha), "%d %b %Y"), " – ",
                        format(max(df$fecha), "%d %b %Y"))
    ) +
    tema_series()
}

# Exportar 
combos <- base_graficas %>% distinct(city, sipsa_name, sku_code)

pwalk(combos, function(city, sipsa_name, sku_code) {
  ggsave(
    filename = file.path(ruta_series, city,
                         paste0(limpiar_nombre(sipsa_name), "_SKU_", limpiar_nombre(sku_code), ".png")),
    plot   = graficar_serie(city, sipsa_name, sku_code),
    width  = 11,
    height = 5,
    dpi    = 150,
    bg     = "white"
  )
})

message("✅ Gráficas exportadas: ", nrow(combos))

