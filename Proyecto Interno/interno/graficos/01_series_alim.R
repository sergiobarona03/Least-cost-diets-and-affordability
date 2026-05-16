# ============================================================
# Librerías
# ============================================================

library(tidyverse)
library(ggplot2)
library(stringr)
library(fs)


# ============================================================
# Rutas
# ============================================================

base_dir <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/"

ruta_panel <- file.path(base_dir, "interno/output/paneles")

ruta_series <- file.path(base_dir, "interno/output/Series alimentos")


# ============================================================
# Cargar panel balanceado
# ============================================================

panel_balanceado <- readRDS(file.path(ruta_panel, "panel_balanceado.rds"))


# ============================================================
# Preparar base para gráficas
# ============================================================

base_graficas <- panel_balanceado %>%
  mutate(
    fecha = as.Date(fecha),
    city = str_squish(city),
    sipsa_name = str_squish(sipsa_name),
    sku_code = as.character(sku_code),
    price = as.numeric(price)
  ) %>%
  filter(
    !is.na(fecha),
    !is.na(price),
    price > 0
  )


# ============================================================
# Crear carpetas por ciudad
# ============================================================

ciudades <- sort(unique(base_graficas$city))

walk(ciudades, ~ dir_create(file.path(ruta_series, .x)))


# ============================================================
# Función para limpiar nombres de archivos
# ============================================================

limpiar_nombre_archivo <- function(x) {
  x %>%
    str_replace_all("[/\\\\:*?\"<>|]", "_") %>%
    str_replace_all("\\s+", "_") %>%
    str_sub(1, 120)
}


# ============================================================
# Graficar alimento por ciudad
# ============================================================

base_graficas %>%
  distinct(city, sipsa_name, sku_code) %>%
  pwalk(function(city, sipsa_name, sku_code) {
    
    base_plot <- base_graficas %>%
      filter(
        city == !!city,
        sipsa_name == !!sipsa_name,
        sku_code == !!sku_code
      ) %>%
      arrange(fecha)
    
    grafica <- ggplot(base_plot, aes(x = fecha, y = price)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.6) +
      labs(
        title = paste0(sipsa_name, " - ", city),
        subtitle = paste0("SKU: ", sku_code),
        x = "Fecha",
        y = "Precio"
      ) +
      theme_minimal()
    
    nombre_archivo <- paste0(
      limpiar_nombre_archivo(sipsa_name),
      "_SKU_",
      limpiar_nombre_archivo(sku_code),
      ".png"
    )
    
    ggsave(
      filename = file.path(ruta_series, city, nombre_archivo),
      plot = grafica,
      width = 9,
      height = 5,
      dpi = 300
    )
  })