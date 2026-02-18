#######################################################################
## FIGURA 5 (MENSUAL + TRIMESTRAL) - HEATMAPS (paper-ready)
## Incidencia de pobreza de asequibilidad por decil (CoCA / CoNA)
#######################################################################

#----------------------------------------------------------------------
# Packages
#----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(scales)
library(tidyr)

#----------------------------------------------------------------------
# Directories
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

out_dir <- file.path(base_dir, "working-papers/working-paper-ipc/output")
afford_metrics_dir <- file.path(out_dir, "affordability_metrics")

#----------------------------------------------------------------------
# Helpers
#----------------------------------------------------------------------

cap_p95 <- function(x) {
  p95 <- suppressWarnings(quantile(x, probs = 0.95, na.rm = TRUE, names = FALSE, type = 7))
  pmin(x, p95)
}

cap_val <- function(x, cap = 60) {
  pmin(x, cap)
}

make_bins <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x < 1    ~ "0–<1",
    x < 5    ~ "1–<5",
    x < 10   ~ "5–<10",
    x < 20   ~ "10–<20",
    x < 40   ~ "20–<40",
    x < 60   ~ "40–<60",
    TRUE     ~ "60+"
  )
}

bin_levels <- c("0–<1", "1–<5", "5–<10", "10–<20", "20–<40", "40–<60", "60+")

bin_colors <- c(
  "0–<1"   = "#f7fbff",
  "1–<5"   = "#deebf7",
  "5–<10"  = "#c6dbef",
  "10–<20" = "#9ecae1",
  "20–<40" = "#6baed6",
  "40–<60" = "#3182bd",
  "60+"    = "#08519c"
)

city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

std_city <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTA") ~ "BOGOTA",
    x %in% c("MEDELLÍN", "MEDELLIN")                 ~ "MEDELLIN",
    x %in% c("CALI")                                 ~ "CALI",
    TRUE ~ x
  )
}

#----------------------------------------------------------------------
# Paper-style theme
#----------------------------------------------------------------------
theme_heat_paper <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_blank(),
    
    panel.grid = element_blank(),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.key.height = unit(3.2, "mm"),
    legend.key.width  = unit(12, "mm"),
    
    plot.margin = margin(6, 6, 6, 6)
  )

x_scale_year_clean <- scale_x_date(
  date_breaks = "1 year",
  date_labels = "%Y",
  expand = expansion(mult = c(0, 0))
)

y_scale_decil <- scale_y_discrete(limits = rev(as.character(1:10)))
facet_city <- facet_wrap(~ciudad, nrow = 1)

#----------------------------------------------------------------------
# Continuous heatmap factory
#----------------------------------------------------------------------
plot_heat_continuous <- function(df, x_var, fill_var, title_text, file_name,
                                 width = 14, height = 4.6, cap_max = 60) {
  
  p <- ggplot(df, aes(x = {{ x_var }}, y = factor(decil_num), fill = {{ fill_var }})) +
    geom_tile(color = "white", linewidth = 0.15) +
    facet_city +
    x_scale_year_clean +
    y_scale_decil +
    scale_fill_viridis_c(
      option = "C",
      direction = -1,
      limits = c(0, cap_max),
      oob = scales::squish,
      name = "Incidence (%)"
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = "Decile"
    ) +
    theme_heat_paper
  
  ggsave(
    filename = file.path(afford_metrics_dir, file_name),
    plot = p,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  
  p
}

#----------------------------------------------------------------------
# Binned heatmap factory
#----------------------------------------------------------------------
plot_heat_bins <- function(df, x_var, title_text, file_name,
                           width = 14, height = 4.6) {
  
  p <- ggplot(df, aes(x = {{ x_var }}, y = factor(decil_num), fill = bin)) +
    geom_tile(color = "white", linewidth = 0.15) +
    facet_city +
    x_scale_year_clean +
    y_scale_decil +
    scale_fill_manual(
      name = "Incidence (%)",
      values = bin_colors,
      breaks = bin_levels,
      drop = FALSE
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = "Decile"
    ) +
    theme_heat_paper
  
  ggsave(
    filename = file.path(afford_metrics_dir, file_name),
    plot = p,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  
  p
}

#======================================================================
# MONTHLY DATA
#======================================================================
afford_m <- readRDS(file.path(afford_metrics_dir, "Afford_city_month.rds")) %>%
  mutate(
    fecha = as.Date(fecha, origin = "1970-01-01"),
    mes = floor_date(fecha, "month"),
    decil_num = as.integer(readr::parse_number(deciles)),
    ciudad = factor(std_city(ciudad), levels = city_levels),
    model = as.character(model)
  )

mes_levels <- sort(unique(afford_m$mes))

afford_m_full <- afford_m %>%
  select(ciudad, model, mes, decil_num, rate) %>%
  distinct() %>%
  complete(ciudad, model, decil_num, mes = mes_levels, fill = list(rate = NA_real_)) %>%
  group_by(ciudad, model) %>%
  mutate(
    rate_cap = cap_p95(rate),
    rate_plot = cap_val(rate_cap, 60)
  ) %>%
  ungroup() %>%
  mutate(
    bin = factor(make_bins(rate_plot), levels = bin_levels)
  )

# Monthly CoCA
plot_heat_continuous(
  df = afford_m_full %>% filter(model == "CoCA"),
  x_var = mes,
  fill_var = rate_plot,
  title_text = "CoCA incidence by decile (monthly)",
  file_name = "CoCA_decil_month_matrix_CONT.png"
)

# Monthly CoNA
plot_heat_continuous(
  df = afford_m_full %>% filter(model == "CoNA"),
  x_var = mes,
  fill_var = rate_plot,
  title_text = "CoNA incidence by decile (monthly)",
  file_name = "CoNA_decil_month_matrix_CONT.png"
)

#======================================================================
# QUARTERLY DATA
#======================================================================
afford_q <- readRDS(file.path(afford_metrics_dir, "Afford_city_cuartiles.rds")) %>%
  mutate(
    decil_num = as.integer(readr::parse_number(deciles)),
    trimestre = as.character(trimestre),
    year = as.integer(str_extract(trimestre, "^\\d{4}")),
    q = as.integer(str_extract(trimestre, "(?<=Q)\\d+")),
    tri_date = as.Date(sprintf("%d-%02d-01", year, (q - 1) * 3 + 1)),
    ciudad = factor(std_city(ciudad), levels = city_levels),
    model = as.character(model)
  )

tri_levels <- sort(unique(afford_q$tri_date))

afford_q_full <- afford_q %>%
  select(ciudad, model, tri_date, decil_num, rate) %>%
  distinct() %>%
  complete(ciudad, model, decil_num, tri_date = tri_levels, fill = list(rate = NA_real_)) %>%
  group_by(ciudad, model) %>%
  mutate(
    rate_cap = cap_p95(rate),
    rate_plot = cap_val(rate_cap, 60)
  ) %>%
  ungroup()

# Quarterly CoCA
plot_heat_continuous(
  df = afford_q_full %>% filter(model == "CoCA"),
  x_var = tri_date,
  fill_var = rate_plot,
  title_text = "CoCA incidence by decile",
  file_name = "CoCA_decil_quarter_matrix_CONT.png"
)

# Quarterly CoNA
plot_heat_continuous(
  df = afford_q_full %>% filter(model == "CoNA"),
  x_var = tri_date,
  fill_var = rate_plot,
  title_text = "CoNA incidence by decile",
  file_name = "CoNA_decil_quarter_matrix_CONT.png"
)

#######################################################################
## DONE
#######################################################################

