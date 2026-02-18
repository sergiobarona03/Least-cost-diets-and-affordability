#######################################################################
## FIGURA: Incidencia de pobreza monetaria (PM) y extrema (PME)
## MENSUAL + TRIMESTRAL   
#######################################################################

#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(ggsci)
library(stringr)

#----------------------------------------------------------------------
# RUTAS 
#----------------------------------------------------------------------
poverty_month_path <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/working-papers/working-paper-ipc/output/poverty_rates/poverty_rates_city_month.rds"

poverty_quarter_path <- "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno/working-papers/working-paper-ipc/output/poverty_rates/poverty_rates_city_cuartiles.rds"

# Carpeta de salida (misma carpeta poverty_rates)
output_dir <- dirname(poverty_month_path)

#======================================================================
# Estética PAPER 
#======================================================================

city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

std_city <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTÁ", "BOGOTA") ~ "BOGOTA",
    x %in% c("MEDELLÍN", "MEDELLIN")                           ~ "MEDELLIN",
    x %in% c("CALI")                                           ~ "CALI",
    TRUE ~ x
  )
}

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

# Escalas
x_scale_6m <- scale_x_date(
  date_breaks = "6 months",
  date_labels = "%Y-%m",
  expand = expansion(mult = c(0.01, 0.01))
)

y_scale_pct <- scale_y_continuous(
  labels = label_percent(scale = 1, accuracy = 0.1),
  expand = expansion(mult = c(0.02, 0.02))
)

# Paleta por ciudad (elige UNA para todo el paper)
color_scale_city <- scale_color_nejm(name = "Ciudad")

#======================================================================
# CARGAR BASE MENSUAL
#======================================================================

poverty_m <- readRDS(poverty_month_path) %>%
  mutate(
    fecha  = as.Date(fecha),
    ciudad = factor(std_city(dominio), levels = city_levels)
  ) %>%
  select(ciudad, fecha, pm, pme) %>%
  filter(ciudad %in% city_levels) %>%
  arrange(ciudad, fecha)

#======================================================================
# GRÁFICAS MENSUALES (paper)
#======================================================================

# PM mensual
plot_pm_m <- ggplot(poverty_m, aes(x = fecha, y = pm, color = ciudad)) +
  geom_line(linewidth = 1) +
  x_scale_6m +
  y_scale_pct +
  color_scale_city +
  labs(
    title = "Incidencia de pobreza monetaria (PM)",
    x = NULL,
    y = "Incidencia"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(output_dir, "poverty_pm_city_month.png"),
  plot_pm_m, width = 10, height = 6, dpi = 300, bg = "white"
)

# PME mensual
plot_pme_m <- ggplot(poverty_m, aes(x = fecha, y = pme, color = ciudad)) +
  geom_line(linewidth = 1) +
  x_scale_6m +
  y_scale_pct +
  color_scale_city +
  labs(
    title = "Incidencia de pobreza monetaria extrema (PME)",
    x = NULL,
    y = "Incidencia"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(output_dir, "poverty_pme_city_month.png"),
  plot_pme_m, width = 10, height = 6, dpi = 300, bg = "white"
)

#======================================================================
# CARGAR BASE TRIMESTRAL
#======================================================================

poverty_q <- readRDS(poverty_quarter_path) %>%
  mutate(
    ciudad = factor(std_city(dominio), levels = city_levels)
  ) %>%
  select(ciudad, trimestre, pm, pme) %>%
  filter(ciudad %in% city_levels)

# Orden correcto de trimestres
tri_levels <- poverty_q %>%
  distinct(trimestre) %>%
  mutate(
    y  = as.integer(str_extract(trimestre, "^\\d{4}")),
    qq = as.integer(str_extract(trimestre, "(?<=Q)\\d+"))
  ) %>%
  arrange(y, qq) %>%
  pull(trimestre)

poverty_q <- poverty_q %>%
  mutate(trimestre = factor(trimestre, levels = tri_levels)) %>%
  arrange(ciudad, trimestre)

#======================================================================
# QUARTERLY PLOTS (paper)
#======================================================================

# Quarterly PM
plot_pm_q <- ggplot(poverty_q, aes(x = trimestre, y = pm, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  y_scale_pct +
  color_scale_city +
  labs(
    title = "Monetary Poverty Incidence (PM)",
    x = "Quarter",
    y = "Incidence"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(output_dir, "poverty_pm_city_quarter.png"),
  plot_pm_q, width = 12, height = 6, dpi = 300, bg = "white"
)

# Quarterly PME
plot_pme_q <- ggplot(poverty_q, aes(x = trimestre, y = pme, color = ciudad, group = ciudad)) +
  geom_line(linewidth = 1) +
  y_scale_pct +
  color_scale_city +
  labs(
    title = "Extreme Monetary Poverty Incidence (PME)",
    x = "Quarter",
    y = "Incidence"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(output_dir, "poverty_pme_city_quarter.png"),
  plot_pme_q, width = 12, height = 6, dpi = 300, bg = "white"
)

#######################################################################
## DONE
#######################################################################

