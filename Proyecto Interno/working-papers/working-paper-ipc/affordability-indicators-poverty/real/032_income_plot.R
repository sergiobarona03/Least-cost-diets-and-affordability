#######################################################################
# Ridgeline Plot - Ingreso per cápita MENSUAL + TRIMESTRAL por ciudad
#######################################################################

library(tidyverse)
library(lubridate)
library(ggridges)
library(scales)
library(ggsci)
library(stringr)

#----------------------------------------------------------------------
# Directorios
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

income_dir <- file.path("working-papers", "working-paper-ipc", "output", "incomecol")

#----------------------------------------------------------------------
# ESTÉTICA PAPER 
#----------------------------------------------------------------------
city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

theme_paper_ridges <- theme_classic(base_size = 11) +
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
    plot.margin = margin(6, 6, 6, 6),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_blank()
  )

# Paleta por ciudad (elige UNA para todo el paper; aquí NEJM)
fill_city_scale <- scale_fill_nejm(name = NULL)

#----------------------------------------------------------------------
# 1) Cargar IncomeCol (mensual)
#----------------------------------------------------------------------
income_files_m <- list.files(
  income_dir,
  pattern = "^IncomeCol_\\d{4}\\.rds$",
  full.names = TRUE
)

df_m <- income_files_m %>%
  lapply(readRDS) %>%
  bind_rows()

#----------------------------------------------------------------------
# 2) Preparar variables (MENSUAL)
#----------------------------------------------------------------------
df_m <- df_m %>%
  mutate(
    fecha      = ymd(paste(year, mes, "01", sep = "-")),
    ciudad     = dominio,
    ingreso_pc = as.numeric(per_capita_income),
    mes_lab    = format(fecha, "%Y-%m")
  ) %>%
  filter(
    ciudad %in% c("BOGOTA", "MEDELLIN", "CALI"),
    !is.na(ingreso_pc),
    ingreso_pc > 0
  ) %>%
  mutate(
    ciudad = factor(ciudad, levels = city_levels)
  )

# Recorte de outliers global
p99_m <- quantile(df_m$ingreso_pc, 0.99, na.rm = TRUE)
df_m <- df_m %>% filter(ingreso_pc < p99_m)

# Orden correcto meses
df_m <- df_m %>%
  arrange(fecha) %>%
  mutate(mes_lab = factor(mes_lab, levels = unique(mes_lab)))

#----------------------------------------------------------------------
# 3) Ridgeline plot MENSUAL (paper)
#----------------------------------------------------------------------
plot_income_m <- ggplot(
  df_m,
  aes(
    x = log(ingreso_pc),
    y = mes_lab,
    group = mes_lab,
    fill = ciudad
  )
) +
  stat_density_ridges(
    quantile_lines = FALSE,
    alpha = 0.55,
    scale = 2.0,
    rel_min_height = 0.01,
    color = "white",      
    linewidth = 0.2
  ) +
  facet_wrap(~ciudad, ncol = 3, scales = "free_x") +
  fill_city_scale +
  labs(
    title = "Distribución de Ln(Ingreso per cápita) por mes",
    y = "Mes",
    x = "Ln(Ingreso per cápita)"
  ) +
  theme_paper_ridges +
  theme(
    # ridgeline se ve mejor sin eje Y ultra cargado
    axis.text.y = element_text(size = 8),
    # reduce “ruido” visual
    panel.border = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(
  file.path(income_dir, "income_ridgeline_plot_monthly.png"),
  plot = plot_income_m,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

#----------------------------------------------------------------------
# 4) Cargar IncomeCol TRIMESTRAL
#----------------------------------------------------------------------
income_files_q <- list.files(
  income_dir,
  pattern = "^IncomeCol_quartiles_\\d{4}\\.rds$",
  full.names = TRUE
)

df_q <- income_files_q %>%
  lapply(readRDS) %>%
  bind_rows()

#----------------------------------------------------------------------
# 5) Preparar variables (TRIMESTRAL)
#----------------------------------------------------------------------
df_q <- df_q %>%
  mutate(
    ciudad     = dominio,
    ingreso_pc = as.numeric(per_capita_income),
    trimestre  = if ("trimestre" %in% names(.)) {
      as.character(trimestre)  # por si ya viene "2018Q1" o "2018 Q1"
    } else {
      paste0(year, " Q", q)
    }
  ) %>%
  filter(
    ciudad %in% c("BOGOTA", "MEDELLIN", "CALI"),
    !is.na(ingreso_pc),
    ingreso_pc > 0
  ) %>%
  mutate(
    ciudad = factor(ciudad, levels = city_levels)
  )

# Recorte de outliers global
p99_q <- quantile(df_q$ingreso_pc, 0.99, na.rm = TRUE)
df_q <- df_q %>% filter(ingreso_pc < p99_q)

# Orden correcto de trimestres (por year y q)
tri_levels <- df_q %>%
  distinct(trimestre, year, q) %>%
  mutate(
    year = if ("year" %in% names(.)) as.integer(year) else as.integer(str_extract(trimestre, "^\\d{4}")),
    q    = if ("q" %in% names(.)) as.integer(q) else as.integer(str_extract(trimestre, "(?<=Q)\\d+"))
  ) %>%
  arrange(year, q) %>%
  pull(trimestre)

df_q <- df_q %>%
  mutate(trimestre = factor(trimestre, levels = unique(tri_levels)))

#----------------------------------------------------------------------
# 6) Ridgeline plot QUARTERLY 
#----------------------------------------------------------------------
plot_income_q <- ggplot(
  df_q,
  aes(
    x = log(ingreso_pc),
    y = trimestre,
    group = trimestre,
    fill = ciudad
  )
) +
  stat_density_ridges(
    quantile_lines = FALSE,
    alpha = 0.55,
    scale = 2.0,
    rel_min_height = 0.01,
    color = "white",
    linewidth = 0.2
  ) +
  facet_wrap(~ciudad, ncol = 3, scales = "free_x") +
  fill_city_scale +
  labs(
    title = "Distribution of Log(Per Capita Income)",
    y = "Quarter",
    x = "Log(Per Capita Income)"
  ) +
  theme_paper_ridges +
  theme(
    axis.text.y = element_text(size = 9),
    panel.border = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(
  file.path(income_dir, "income_ridgeline_plot_quarterly.png"),
  plot = plot_income_q,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)

#######################################################################
# FIN
#######################################################################

